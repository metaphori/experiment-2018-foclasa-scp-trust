import it.unibo.alchemist.implementation.nodes.NodeManager
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.model.interfaces.Time
import scala.collection.mutable.{Map => MMap}

trait ScafiAlchemistSupport { self: AggregateProgram =>
  def env = sense[NodeManager]("manager")
}

class gradient extends AggregateProgram with ScafiAlchemistSupport {
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
  lazy val performAnalysis: Boolean = sense[Int]("performAnalysis")==1
  
  def isSrc: Boolean = sense[Int]("source")==1
  def isFake: Boolean = sense[Int]("fake")==1
  def isInFakeArea: Boolean = gradient(isFake)<=fakeAreaSize
  lazy val willBeFake: Boolean = sense[Int]("willBeFake")==1

  def nbrRange = nbrvar[Double](NBR_RANGE_NAME)
  def currTime: Double = sense[Time]("time").toDouble()
  def deltaTime: Double = sense[Double]("dt")
  def nextRandom: Double = sense[()=>java.lang.Double]("random")().toDouble

  var fakeValue: Double = 0.0
  val keepFakeValueTime = 0
  def nextFakeValue = (nextRandom)*30 // 2
  
  // Keeps a pair (a,b) for each node that is used to calculate a trust score
  var map = Map[ID,(Double,Double)]()
  
  override def main(): Double = {
    val fake = isInFakeArea
    env.put[Boolean]("isFake", isFake)
    env.put[Boolean]("isInFakeArea", fake)
    //val fakeSrc = if(isFake) scala.util.Random.nextBoolean else false
    
    branch(fake){ // Sets fake value to be produced in this round
      fakeValue = keepFor(keepFakeValueTime, nextFakeValue)
    }{ /* do nothing */ }

    val dist = gradient(isSrc)
    env.put[Double]("gradient", dist)
    
    val distWithFakes = gradient(isSrc, fake)
    env.put[Double]("gradientUnderAttack", distWithFakes)
    env.put[Double]("error-fakes", Math.abs(dist-distWithFakes))
    env.put[Boolean]("n-error-fakes", Math.abs(dist-distWithFakes) > (5*dist/100))
    
    branch(useTrust){
    val distWithTrust = gradientWithTrust(isSrc, fake)
    env.put[Double]("gradientWithTrust", distWithTrust)
    env.put[Double]("error-trust", Math.abs(dist-distWithTrust))
    env.put[Boolean]("n-error-trust", Math.abs(dist-distWithTrust) > (5*dist/100))
    }{}
    
    branch(useRecommendations){
    val distWithTrustRecomm = gradientWithTrustPlusRecommendations(isSrc, fake)
    env.put[Double]("gradientWithTrustPlusRecs", distWithTrustRecomm)
    env.put[Double]("error-trust-recomm", Math.abs(dist-distWithTrustRecomm))
    env.put[Boolean]("n-error-trust-recomm", Math.abs(dist-distWithTrustRecomm) > (5*dist/100))
    }{}
    
    dist
  }

  def gradient(source: Boolean, fake: Boolean = false): Double =
    rep(Double.PositiveInfinity){
      distance => mux(source) { 0.0 } {
        def shareDistance = if(!fake) nbr{distance} else nbr{fakeValue}
        foldhoodPlus(Double.PositiveInfinity)(Math.min)(shareDistance + nbrRange)
      }
    }
  
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
      
      branch(performAnalysis){
        val fakeNbr = anyHood{ nbr(willBeFake) }
        val nearFake = anyHood { nbr(fakeNbr) } && !fakeNbr
        val nearNearFake = anyHood { nbr(nearFake) } && !nearFake
        env.put("fake_nbr", fakeNbr)
        env.put("fake_near", nearFake)
        env.put("s_value_nbr_fake", if(fakeNbr) s else Double.NaN)
        env.put("s_value_near_fake", if(nearFake && !fakeNbr) s else Double.NaN)
        env.put("s_value_near_near_fake", if(nearNearFake && !nearFake) s else Double.NaN)
        env.put("s_value_far_fake", if(!nearNearFake && !nearFake && !fakeNbr) s else Double.NaN)
      }{}
      
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
            merrdiff = 0.0
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
  
  var merrdiff = 0.0
  
  /**
   * Returns a score based on a beta distribution.
   */
  def calculateTrustParams(field: => Double, xmean: Double, s: Double): TrustParams = {        
    val (nbrId, nbrVal) = nbr{ (mid(), field) }
    val deviation = Math.abs(nbrVal - xmean)
    val maxError = Math.max(s, errorLB)
    merrdiff = merrdiff + (if(maxError > errorLB) 1.0 else 0.0)
    env.put("merrdiff", merrdiff)
    
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
  
  def mapHood[T](m: MMap[ID,T], default: T)(f: (ID,T)=>T) = foldhoodPlus[MMap[ID,T]](m)(_++=_){ 
    val nbrId = nbr{mid()}
    MMap(nbrId -> f(nbrId, m.getOrElse(nbrId, default))) 
  }
  
  case class FoldingOptions(includeSelf: Boolean = false, onlyFinite: Boolean = true)
  def minHood(field: => Double)(implicit fops: FoldingOptions = FoldingOptions()) = 
    fold[Double](fops.includeSelf)(Double.PositiveInfinity)(Math.min)(field)
  def maxHood(field: => Double)(implicit fops: FoldingOptions = FoldingOptions()) = 
    fold[Double](fops.includeSelf)(Double.NegativeInfinity)(Math.max)(field)
  def sumHood(field: => Double)(implicit fops: FoldingOptions = FoldingOptions()) = 
    fold[Double](fops.includeSelf)(0)(_+_)(field.orIf(fops.onlyFinite && !_.isFinite)(0.0))
  def anyHood(field: => Boolean)(implicit fops: FoldingOptions = FoldingOptions()) = 
    fold[Boolean](fops.includeSelf)(false)(_||_)(field)
  def countHood(field: => Boolean)(implicit fops: FoldingOptions = FoldingOptions()) = 
    fold[Int](fops.includeSelf)(0)(_+_)(if(field) 1 else 0)
  def fold[T](includeSelf: Boolean) = 
    branch(includeSelf){ (foldhood[T] _) }{ (foldhoodPlus[T] _) }
    
  def keepFor(time: Double, f: => Double): Double = rep[(Double,Double)]((currTime,f)){ case (lastTime,oldVal) =>
    mux(currTime-lastTime > time){ (currTime, f) } { (lastTime, oldVal) }
  }._2
  
  def meanCounter(value: Double, frequency: Long): Double = {
	  val time = currTime
	  val dt = deltaTime
	  val count = rep ((0.0,0.0)) { case x => { // (accumulated value, last time)
	    // Splits into windows of multiples of 'frequency'
	    // and restarts at the beginning of each new window.
	    // E.g., for frequency=5
	    // Time:           0_____5_____10_____15_____20 ...
	    // Restart:              ^      ^      ^      ^ ...
	    // Floor(Time/freq):  0     1      2      3     ... 
		  val restart = rep((false, time)) { t =>
			  (Math.floor(time/frequency) > Math.floor(t._2/frequency), time)
		  }._1
		  // Reset value and time on restart
		  val old = if (restart) (0.0,0.0)  else x
		  // Filters infinite values out
		  if (Double.NegativeInfinity < value && value < Double.PositiveInfinity) {
		    // Sums value weighed by time
		    (old._1+value*dt, old._2+dt)
		  } else old
	  } }
	  // E.g., consider these values and deltas: (5.0,2), (6,1), (Inf,2), (7,1), (5,1)
		// You'll finally have (5.0*2 + 6*1 + 7*1 + 5*1) / (2+1+1+1) = 28/5 = 5.6
	  count._1 / count._2
  }
  
  implicit class RichValue[T](val value: T){
    def orIf(pred: T => Boolean)(newValue: T): T = orIfCompute(pred)(_ => newValue)
    def orIfCompute(pred: T => Boolean)(compute: T => T): T = if(pred(value)) compute(value) else value
  }  
  
  implicit class RichPred[T](val p: T => Boolean) {
     def not: T => Boolean = v => !p(v)
  }
  
  implicit class MyRichDouble(val d: Double) {
    def isFinite: Boolean = !d.isInfinite && !d.isNaN
  }  
}