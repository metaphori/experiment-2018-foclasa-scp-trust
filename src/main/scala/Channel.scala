import it.unibo.alchemist.implementation.nodes.NodeManager
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins.Bounded
import it.unibo.alchemist.model.interfaces.Time
import scala.collection.mutable.{Map => MMap}

class Channel extends AggregateProgram with ScafiAlchemistSupport with MyLib {
  lazy val trustThreshold = env.get[Double]("trustThreshold")
  lazy val startTrustAlgorithmAt = env.get[Double]("startTrustAlgorithmAt")
  lazy val observationWindow = env.get[Double]("observationWindowSize").toInt
  lazy val minObservations = env.get[Int]("minObservations")
  lazy val commRadius = env.get[Double]("commRadius")
  lazy val errorLB = env.get[Double]("errorLB")
  lazy val maxErrorFactor = env.get[Double]("maxErrorFactor")
  lazy val fakeAreaSize = env.get[Double]("fakeAreaSize")
  
  lazy val useRecommendations: Boolean = sense[Int]("useRecomm")==1
  lazy val useTrust: Boolean = sense[Int]("useTrust")==1
  
  import FieldUtils.withSelf._
  
  lazy val channelWidth = env.get[Double]("channelWidth")
  def isSrc: Boolean = sense[Int]("source")==1
  def isTarget: Boolean = sense[Int]("target")==1
  def isObstacleSource: Boolean = sense[Int]("obstacle")==1
  def isObstacle: Boolean = anyHood(nbr { isObstacleSource })
  def isFake: Boolean = sense[Int]("fake")==1
  def isInFakeArea: Boolean = gradient(isFake)<=fakeAreaSize

  var fakeValue: Double = 0.0
  val keepFakeValueTime = 0
  def nextFakeValue = (nextRandom)*30 // 2
  
  // Keeps a pair (a,b) for each node that is used to calculate a trust score
  var map = Map[ID,(Double,Double)]()
  
  override def main(): Boolean = {
    val fake = isInFakeArea
    env.put[Boolean]("isFake", isFake)
    env.put[Boolean]("isInFakeArea", fake)
    //val fakeSrc = if(isFake) scala.util.Random.nextBoolean else false
    
    branch(fake){ // Sets fake value to be produced in this round
      fakeValue = keepFor(keepFakeValueTime, nextFakeValue)
    }{ /* do nothing */ }
    
    val obstacle = isObstacle
    env.put[Boolean]("isObstacle", obstacle)
    
    branch(!obstacle){
      val c = channel(isSrc, isTarget, channelWidth, gradient(_))
      val cFake = channel(isSrc, isTarget, channelWidth, gradient(_,fake))
      env.put[Double]("channel-fake", if(cFake) 1 else 0)
      env.put[Double]("channel-err-fake", if(c ^ cFake) 1 else 0)
      branch(useTrust){
        val cTrust = channel(isSrc, isTarget, channelWidth, gradientWithTrust(_,fake)) 
        env.put[Double]("channel-trust", if(cTrust) 1 else 0)
        env.put[Double]("channel-err-trust", if(c ^ cTrust) 1 else 0)
      }{}
      branch(useRecommendations){
        val cRecomms = channel(isSrc, isTarget, channelWidth, gradientWithTrustPlusRecommendations(_,fake)) 
        env.put[Double]("channel-recomms", if(cRecomms) 1 else 0)
        env.put[Double]("channel-err-recomms", if(c ^ cRecomms) 1 else 0)
      }{}
      
      c
    }{false} 
  }

  def channel(source: Boolean, target: Boolean, width: Double, g: Boolean => Double): Boolean =
    g(source) + g(target) <= distanceBetween(source, target, g) + width  
  
  def broadcast[V: Bounded](source: Boolean, field: V): V =
    G[V](source, field, v => v, nbrRange)

  def distanceBetween(source: Boolean, target: Boolean, g: Boolean => Double): Double =
    broadcast(source, g(target))    
    
  def gradient(source: Boolean, fake: Boolean = false): Double =
    rep(Double.PositiveInfinity){
      distance => mux(source) { 0.0 } {
        def shareDistance = if(!fake) nbr{distance} else nbr{fakeValue}
        foldhoodPlus(Double.PositiveInfinity)(Math.min)(shareDistance + nbrRange)
      }
    }
  
    def G[V: Bounded](source: Boolean, field: V, acc: V => V, metric: => Double): V =
      rep((Double.MaxValue, field)) { case (dist, value) =>
        mux(source) {
          (0.0, field)
        } {
          minHoodPlus {
            (nbr { dist } + metric, acc(nbr { value }))
          }
        }
      }._2  
  
  /**
   * a: represent a positive observation
   * b: represent a negative observation
   */
  case class TrustParams(a: Double, b: Double, numObservations: Int)
  case class TrustProfile(nbrId: ID, params: TrustParams, value: Double)
  
  def gradientWithTrust(source: Boolean, fake: Boolean = false, startAt: Double = startTrustAlgorithmAt): Double = {
    rep(Double.PositiveInfinity){ distance => 
      val dist = if(!fake) distance else fakeValue
      def nbrDist = nbr { dist }
      val n = countHood(nbrDist.isFinite)
      //env.put("nbrVals", mapHood(MMap[ID,Double](),-1.0)((x,y) => nbrDist))
      val sumValues = sumHood(mux(nbrDist.isFinite){ nbrDist }{ 0.0 } )
      val xmean =  sumValues / n
      val sumSqDev = sumHood(mux(nbrDist.isFinite){ Math.pow(dist - xmean, 2) } { 0.0 })
      val s = Math.sqrt(sumSqDev / n)
      //env.put("n_sum_mean_sumsq_s", (n, sumValues, xmean, sumSqDev, s))
      
      //val fakeNbr = anyHood{ nbr(fake) }
      //val nearFake = anyHood { nbr(fakeNbr) }
      //val nearNearFake = anyHood { nbr(nearFake) }
      //env.put("s_value_nbr_fake", if(fakeNbr) s else Double.NaN)
      //env.put("s_value_near_fake", if(nearFake && !fakeNbr) s else Double.NaN)
      //env.put("s_value_near_near_fake", if(nearNearFake && !nearFake) s else Double.NaN)
      //env.put("s_value_far_fake", if(!nearNearFake) s else Double.NaN)
      
      mux(source) { 0.0 } {
        branch(currTime > startAt){
          var distrustedNbrs = List[ID]()
          val res = foldhoodPlus(Double.PositiveInfinity)(Math.min){
            val trustParams = calculateTrustParams(dist, xmean, s)
            val trustValue = beta(trustParams.a, trustParams.b)
            val isTrusted = if(trustParams.numObservations >= minObservations) trustable(trustValue) else true
            val nbrId = nbr{mid()}
            if(!isTrusted) { distrustedNbrs = nbrId :: distrustedNbrs}
            val newg = mux(isTrusted){ nbr{dist} + nbrRange }{ Double.PositiveInfinity } // dist/Math.pow(trustValue,4) 
            //env.put(s"trust_${nbr(mid())}__", trustValue + " >= " + trustThreshold + "??? " + isTrusted)
            newg
          }

          env.put("distrustedNbrs", distrustedNbrs)
          val distrustedBy = foldhoodPlus(Map[ID,List[ID]]())(_++_)(Map(nbr{mid()->distrustedNbrs})).filter(_._2.contains(mid())).keySet
          
          val distrusted = !distrustedBy.isEmpty
          env.put("distrusted", distrusted)
          env.put("distrustedBy", distrustedBy)
          res.orIf(_==Double.PositiveInfinity){dist}
        }{
          minHood( nbr{dist} + nbrRange )
        }
      }
    } 
  }  
  
  def gradientWithTrustPlusRecommendations(source: Boolean, fake: Boolean = false, startAt: Double = startTrustAlgorithmAt): Double = {
    rep(Double.PositiveInfinity){ distance => 
      val dist = if(!fake) distance else fakeValue
      def nbrDist = nbr { dist }
      val n = countHood(nbrDist.isFinite)
      val sumValues = sumHood(mux(nbrDist.isFinite){ nbrDist }{ 0.0 } )
      val xmean =  sumValues / n
      val sumSqDev = sumHood(mux(nbrDist.isFinite){ Math.pow(dist - xmean, 2) } { 0.0 })
      val s = Math.sqrt(sumSqDev / n)
         
      mux(source) { 0.0 } {
        branch(currTime > startAt){
          var distrustedNbrs = List[ID]()
          // NOTE: beware of foldhoodPlus with side effects:
          // as it is impl by now, on top of foldhood, 
          // the expression is run for the 'self' as well!
          val localTrustProfiles = foldhoodPlus(Map[ID,TrustProfile]())(_++_){
            val nbrId = nbr{mid()}
            val trustParams = calculateTrustParams(dist, xmean, s)
            Map(nbrId -> TrustProfile(nbrId, trustParams, nbr{dist}+nbrRange))
          }
          
          val nbrTrustProfiles = foldhoodPlus(Map[ID,Map[ID,TrustProfile]]())(_++_){
            Map(nbr{mid -> localTrustProfiles})
          }
          
          // NOTE: beware using aggregate constructs inside foldhood:
          // results of inner nodes are reused by all the neighbours!
          // Also: remember that foldhood.aggregate.nbr is to be ignored
          val res = foldhoodPlus(Double.PositiveInfinity)(Math.min){
            mux(!nbrDist.isFinite){ 
              env.put(s"trustParams_${nbr(mid())}", s"NOT FINITE: $nbrDist")
              Double.PositiveInfinity 
            }{
              val nbrId = nbr{mid()}
              val (aRec: Double, bRec: Double) = nbrTrustProfiles
                .mapValues(_.get(nbrId).map(p => (p.params.a, p.params.b)).getOrElse(0.0,0.0))
                .foldLeft((0.0, 0.0))((acc, value) => {
                  // i = mid, j = nbrId ; a_j and b_j calculated from all nbrs k != i,j
                  val TrustParams(a_ik, b_ik, nObs) = localTrustProfiles.get(value._1).map(_.params).getOrElse(TrustParams(0.0,0.0,0))
                  val a_kj = value._2._1
                  val b_kj = value._2._2
                  val denom = (b_ik+2)*(a_kj+b_kj+2)+2*a_ik
                  val a_j = acc._1 + 2*a_ik*a_kj/denom
                  val b_j = acc._2 + 2*a_ik*b_kj/denom
                  (a_j, b_j)
                })
              val localParams = localTrustProfiles.get(nbrId).map(_.params).getOrElse(TrustParams(0.0,0.0,0))
              var a = localParams.a + aRec
              var b = localParams.b + bRec
              val trustValue = beta(a,b)
              val isTrusted = if(localParams.numObservations >= minObservations) trustable(trustValue) else true
              if(!isTrusted) { distrustedNbrs = nbrId :: distrustedNbrs}
              val newg = mux(isTrusted){ nbr{dist} + nbrRange }{ Double.PositiveInfinity }
              newg
            }
          }
          
          env.put("distrustedNbrs", distrustedNbrs)
          val distrustedBy = foldhoodPlus(Map[ID,List[ID]]())(_++_)(Map(nbr{mid()->distrustedNbrs})).filter(_._2.contains(mid())).keySet

          val distrusted = !distrustedBy.isEmpty
          env.put("distrusted-recomm", distrusted)
          env.put("distrustedBy-recomm", distrustedBy)
          res.orIf(_==Double.PositiveInfinity){dist}
        }{
          minHood( nbr{dist} + nbrRange )
        }
      }
    } 
  }
  
  /**
   * Returns a score based on a beta distribution.
   */
  def calculateTrustParams(field: => Double, xmean: Double, s: Double): TrustParams = {        
    val (nbrId, nbrVal) = nbr{ (mid(), field) }
    val deviation = Math.abs(nbrVal - xmean)
    val maxError = Math.max(s, errorLB)
    
    type MutableField[T] = MMap[ID,T]
    def MutableField[T](): MutableField[T] = MMap[ID,T]()
    type AlfaBetaPair = (Double, Double)
    type AlfaBetaHistory = List[AlfaBetaPair]
    
    val m = rep(MutableField[AlfaBetaHistory]()){ m => m }
    
    val history = m.getOrElse(nbrId, List())
    //env.put(s"trust_LASTcheck_${nbrId}__", s"|$nbrVal - $xmean| = $deviation > ${maxError} ($s*$maxErrorFactor) ===> ${deviation > maxError}")
    val obsEval = if(nbrVal.isFinite && s.isFinite){
      if(deviation > maxError) (0.0, 1.0) else (1.0, 0.0)
    } else {
      (0.0,0.0)
    }
    if(obsEval._1!=0 || obsEval._2!=0)
      m.put(nbrId, (obsEval :: history).take(observationWindow))    
    
    val obss = m.getOrElse(nbrId, List())
    
    val (a,b) = obss.foldRight((0.0,0.0))((t,u) => (t._1+u._1, t._2+u._2))
    //env.put(s"trust_params(a,b)_${nbr(mid())}_", (a,b,obss.size))
    TrustParams(a, b, obss.size)
  }
  
  def beta(a: Double, b: Double) = (a+1)/(a+b+2)
  
  def trustable(trustValue: Double): Boolean = trustValue >= trustThreshold
}