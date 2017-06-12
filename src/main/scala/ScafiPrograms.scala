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
  lazy val commRadius = env.get[Double]("commRadius")
  lazy val errorLB = env.get[Double]("errorLB")
  lazy val fakeAreaSize = env.get[Double]("fakeAreaSize")
  
  def isSrc: Boolean = sense[Int]("source")==1
  def isFake: Boolean = sense[Int]("fake")==1
  def isInFakeArea: Boolean = gradient(isFake)<=fakeAreaSize

  def nbrRange = nbrvar[Double](NBR_RANGE_NAME)
  def currTime: Double = sense[Time]("time").toDouble()
  def deltaTime: Double = sense[Double]("dt")

  var fakeValue: Double = 0.0
  val keepFakeValueTime = 0
  def nextFakeValue = (scala.util.Random.nextDouble())*30 // 2
  
  // Keeps a pair (a,b) for each node that is used to calculate a trust score
  var map = Map[ID,(Double,Double)]()
  
  override def main(): Double = {
    val fake = isInFakeArea
    env.put[Boolean]("isFake", isFake)
    env.put[Boolean]("isInFakeArea", fake)
    //val fakeSrc = if(isFake) scala.util.Random.nextBoolean else false
    
    branch(isFake){ // Sets fake value to be produced in this round
      fakeValue = keepFor(keepFakeValueTime, nextFakeValue)
    }{ /* do nothing */ }

    val dist = gradient(isSrc)
    env.put[Double]("gradient", dist)
    
    val distWithFakes = gradient(isSrc, fake)
    env.put[Double]("gradientUnderAttack", distWithFakes)
    
    val distWithTrust = gradientWithTrust(isSrc, fake)
    env.put[Double]("gradientWithTrust", distWithTrust)

    env.put[Double]("error-fakes", Math.abs(dist-distWithFakes))
    env.put[Double]("error-trust", Math.abs(dist-distWithTrust))
    env.put[Boolean]("n-error-fakes", Math.abs(dist-distWithFakes) > (5*dist/100))
    env.put[Boolean]("n-error-trust", Math.abs(dist-distWithTrust) > (5*dist/100))
    
    dist
  }

  def gradient(source: Boolean, fake: Boolean = false): Double =
    rep(Double.PositiveInfinity){
      distance => mux(source) { 0.0 } {
        def shareDistance = if(!fake) nbr{distance} else nbr{fakeValue}
        foldhoodPlus(Double.PositiveInfinity)(Math.min)(shareDistance + nbrRange)
      }
    }
  
  def gradientWithTrust(source: Boolean, fake: Boolean = false, startAt: Double = startTrustAlgorithmAt): Double = {
    rep(Double.PositiveInfinity){ distance => 
      def dist = if(!fake) distance else fakeValue
      def nbrDist = nbr { dist }
      val n = countHood(nbrDist.isFinite)
      env.put("nbrVals", mapHood(MMap[ID,Double](),-1.0)((x,y) => nbrDist))
      val sumValues = sumHood(nbrDist)
      val xmean =  sumValues / n
      val sumSqDev = sumHood{ Math.pow(dist - xmean, 2) }
      val s = Math.sqrt(sumSqDev / n)
      env.put("n_sum_mean_sumsq_s", (n, sumValues, xmean, sumSqDev, s))
         
      mux(source) { 0.0 } {
        branch(currTime > startAt){
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
        }{
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
    val maxError = Math.max(s, errorLB)
    
    val obss = rep[MMap[ID,List[(Double,Double)]]](MMap()){ m => 
      val history = m.getOrElse(nbrId, List())
      env.put(s"trust_lastcheck_${nbrId}__", s"|$nbrVal - $xmean| = $deviation > ${maxError} ===> ${deviation > maxError}")
      val obs = if(nbrVal.isFinite){
        if(deviation > maxError) (0.0, 1.0) else (1.0, 0.0)
      } else {
        (0.0,0.0)
      }
      m.put(nbrId, (obs :: history).take(observationWindow))
      m
    }.getOrElse(nbrId, List())
    val (a,b) = obss.foldRight((1.0,0.0))((t1,t2) => (t1._1+t2._1, t1._2+t2._2))
    beta(a,b)
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