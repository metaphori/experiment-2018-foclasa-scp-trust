import it.unibo.alchemist.implementation.nodes.NodeManager
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.model.interfaces.Time
import scala.collection.mutable.{Map => MMap}

trait ScafiAlchemistSupport { self: AggregateProgram =>
  def env = sense[NodeManager]("manager")
}

class gradient extends AggregateProgram with ScafiAlchemistSupport {
  val trustThreshold = 0.65
  var fakeValue: Double = 0.0
  val startTrustAlgorithmAt = 24
  
  def isSrc: Boolean = sense[Int]("source")==1
  def isFake: Boolean = sense[Int]("fake")==1

  def nbrRange = nbrvar[Double](NBR_RANGE_NAME)
  def currTime: Long = sense[Time]("time").toDouble().toLong
  def deltaTime: Long = sense[Double]("dt").toLong  
  
  lazy val commRadius = env.get[Double]("commRadius")
  
  // Keeps a pair (a,b) for each node that is used to calculate a trust score
  var map = Map[ID,(Double,Double)]()
  
  override def main(): Double = {
    env.put[Boolean]("isFake", isFake)
    //val fakeSrc = if(isFake) scala.util.Random.nextBoolean else false
    val fake = isFake
    
    if(isFake){ // Sets fake value to be produced in this round
      fakeValue = 2 // (scala.util.Random.nextDouble())*1000
    }

    val dist = gradient(isSrc)
    env.put[Double]("gradient", dist)
    
    val distWithFakes = gradient(isSrc, fake)
    env.put[Double]("gradientUnderAttack", distWithFakes)
    
    //if(currTime > 22){
    val distWithTrust = gradientWithTrust(isSrc, fake)
    env.put[Double]("gradientWithTrust", distWithTrust)    
    //}
    
    env.put[Boolean]("error", Math.abs(dist-distWithTrust) > (5*dist/100))
    
    dist
  }

  def gradient(source: Boolean, fake: Boolean = false): Double =
    rep(Double.PositiveInfinity){
      distance => mux(source) { 0.0 } {
        def shareDistance = if(!fake) nbr{distance} else nbr{fakeValue}
        foldhoodPlus(Double.PositiveInfinity)(Math.min)(shareDistance + nbrRange)
      }
    }
  
  def gradientWithTrust(source: Boolean, fake: Boolean = false, startAt: Long = startTrustAlgorithmAt): Double = {
    rep(Double.PositiveInfinity){ distance => 
      def dist = if(!fake) distance else fakeValue
      val n = sumHoodP(1)
      env.put("nbrVals", mapHood(MMap[ID,Double](),-1.0)((x,y) => nbr{dist}))
      val xmean = sumHoodP(dist) / n
      //val sLB = maxHood { if(nbr{source}) nbrRange else Double.NegativeInfinity } // lower bound value for s
      //env.put("sLB", sLB)
      //val s = Math.max(sLB, Math.sqrt( sumHoodP{ Math.pow(dist - xmean, 2) } / n ))
      val s = Math.sqrt( sumHoodP{ Math.pow(dist - xmean, 2) } / n)
      env.put("n_sum_mean_sumsq_s", (n,sumHoodP(dist),xmean,sumHoodP{ Math.pow(dist-xmean, 2) },s))
         
      mux(source) { 0.0 } {
        if(currTime > startAt){
          var distrustedNbrs = List[ID]()
          val res = foldhoodPlus(Double.PositiveInfinity)(Math.min){
            val trustValue = calculateTrust(dist, xmean, s)
            val isTrusted = trustable(trustValue) 
            val nbrId = nbr{mid()}
            if(!isTrusted) { distrustedNbrs = nbrId :: distrustedNbrs}
            val newg = mux(isTrusted){ nbr{dist} + nbrRange }{ Double.PositiveInfinity } // dist/Math.pow(trustValue,4) 
            env.put(s"trust_${nbr(mid())}__", trustValue + " >= " + trustThreshold + "??? " + isTrusted)
            newg
          }
          env.put("distrustedNbrs", distrustedNbrs)
          val distrustedBy = foldhoodPlus(Map[ID,List[ID]]())(_++_)(Map(nbr{mid()->distrustedNbrs})).filter(_._2.contains(mid())).keys
          env.put("distrusted", !distrustedBy.isEmpty)
          env.put("distrustedBy", distrustedBy)
          res.orIf(_==Double.PositiveInfinity){dist}
        } else{
          minHood( nbr{dist} + nbrRange )
        }
      }
    } 
  }
  
  /**
   * Returns the alpha and beta parameters of the beta distribution
   */
  def calculateTrust(field: => Double, xmean: Double, s: Double): Double = {        
    val nbrId = nbr(mid())
    val nbrVal = nbr(field)
    val deviation = Math.abs(nbrVal - xmean)
    val declareItselfAsSource = nbr(isSrc)
    val maxError = Math.max(s, 2) //30*s
    
    val (oldA,oldB) = map.getOrElse(nbrId, (0.0, 0.0))
    env.put(s"trust_lastcheck_${nbrId}__", s"|$nbrVal - $xmean| = $deviation > ${maxError} ===> ${deviation > maxError}")
    map += nbrId -> (if(deviation > maxError && !declareItselfAsSource) (oldA, oldB+1) else (oldA+1, oldB))
    val (a,b) = map(nbrId)
    //    val (a,b) = rep[MMap[ID,(Double,Double)]](MMap()){ m => 
    //      mapHood(m, (0.0, 0.0)){ case (_,(oldA,oldB)) => if(deviation > s) (oldA, oldB+1) else (oldA+1, oldB) }
    //    }.getOrElse(nbrId, (0.0,0.0))
    
    env.put(s"trust_params_${nbr(mid())}__", (a,b))
    beta(a,b)
  }
  
  def beta(a: Double, b: Double) = (a+1)/(a+b+2)
  
  def trustable(trustValue: Double): Boolean = trustValue >= trustThreshold
  
  def mapHood[T](m: MMap[ID,T], default: T)(f: (ID,T)=>T) = foldhoodPlus[MMap[ID,T]](m)(_++=_){ 
    val nbrId = nbr{mid()}
    MMap(nbrId -> f(nbrId, m.getOrElse(nbrId, default))) 
  }
  
  def minHood(field: => Double) = foldhood[Double](Double.PositiveInfinity)(Math.min)(field)
  def maxHood(field: => Double) = foldhood[Double](Double.NegativeInfinity)(Math.max)(field)
  def sumHood(field: => Double) = foldhood[Double](0)(_+_)(nbr { field })
  def sumHood(field: => Boolean) = foldhood[Int](0)(_+_)(if(nbr{field}) 1 else 0)
  def sumHoodP(field: => Double) = foldhoodPlus[Double](0)(_+_)(nbr { field })
  def sumHoodP(field: => Boolean) = foldhoodPlus[Int](0)(_+_)(if(nbr{field}) 1 else 0)  
  
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