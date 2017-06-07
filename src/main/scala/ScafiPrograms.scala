import it.unibo.alchemist.implementation.nodes.NodeManager
import it.unibo.alchemist.model.interfaces.Node
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

trait ScafiAlchemistSupport { self: AggregateProgram =>
  def env = sense[NodeManager]("manager")
}

class gradient extends AggregateProgram with ScafiAlchemistSupport {
  import scala.collection.mutable.{Map => MMap}
  val trustThreshold = 0.5
  lazy val commRadius = env.get[Double]("commRadius")
  
  def isSrc: Boolean = sense[Int]("source")==1
  def isFake: Boolean = sense[Int]("fake")==1
  
  override def main(): Double = {
    env.put[Boolean]("isFake", isFake)
    //val fakeSrc = if(isFake) scala.util.Random.nextBoolean else false
    val fake = false // isFake

    val dist = gradient(isSrc)
    env.put[Double]("gradient", dist)
    
    val distWithFakes = gradient(isSrc, fake)
    env.put[Double]("gradientUnderAttack", distWithFakes)
    
    //if(currTime > 22){
    val distWithTrust = gradientWithTrust(isSrc, fake)
    env.put[Double]("gradientWithTrust", distWithTrust)    
    //}
    
    dist
  }

  def gradient(source: Boolean, fake: Boolean = false): Double =
    rep(Double.PositiveInfinity){
      distance => mux(source) { 0.0 } {
        def shareDistance = if(!fake) nbr{distance} else nbr{(scala.util.Random.nextDouble())*1000}
        foldhoodPlus(Double.PositiveInfinity)(Math.min)(shareDistance + nbrRange)
      }
    }
  
  implicit class RichValue[T](val value: T){
    def orIf(pred: T => Boolean)(newValue: T): T = orIfCompute(pred)(_ => newValue)
    def orIfCompute(pred: T => Boolean)(compute: T => T): T = if(pred(value)) compute(value) else value
  }
  
  def gradientWithTrust(source: Boolean, fake: Boolean = false, startAt: Long = 26): Double = {
    rep(Double.PositiveInfinity){ distance => 
      def dist = if(!fake) distance else (scala.util.Random.nextDouble())*1000
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
          foldhoodPlus(Double.PositiveInfinity)(Math.min){
            val trustValue = calculateTrust(dist, xmean, s)
            val newg = mux(trustable(trustValue)){ nbr{dist} + nbrRange }{ Double.PositiveInfinity } // dist/Math.pow(trustValue,4) 
            env.put(s"trust_${nbr(mid())}__", trustValue + " >= " + trustThreshold + "???")
            newg
          }.orIf(_==Double.PositiveInfinity){dist}
        } else{
          minHood( nbr{dist} + nbrRange )
        }
      }
    } 
  }
   
  var map = Map[ID,(Double,Double)]()
  
  /**
   * Returns the alpha and beta parameters of the beta distribution
   */
  def calculateTrust(field: => Double, xmean: Double, s: Double): Double = {        
    val nbrId = nbr(mid())
    val nbrVal = nbr(field)
    val deviation = Math.abs(nbrVal - xmean)
    val declareItselfAsSource = nbr(isSrc)
    val maxError = 10*s
    
    val (oldA,oldB) = map.getOrElse(nbrId, (0.0, 0.0))
    env.put(s"trust_lastcheck_${nbrId}__", s"|$nbrVal - $xmean| = $deviation > ${maxError} ===> ${deviation > maxError}")
    map += nbrId -> (if(deviation > maxError && !declareItselfAsSource) (oldA, oldB+1) else (oldA+1, oldB))
    val (a,b) = map(nbrId)
//    val (a,b) = rep[MMap[ID,(Double,Double)]](MMap()){ m => 
//      mapHood(m, (0.0, 0.0)){ case (_,(oldA,oldB)) => if(deviation > s) (oldA, oldB+1) else (oldA+1, oldB) }
//    }.getOrElse(nbrId, (0.0,0.0))
    
    env.put(s"trust_params_${nbr(mid())}__", (a,b))
    (a+1)/(a+b+2)
  }
  
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
  
  def nbrRange = nbrvar[Double](NBR_RANGE_NAME)
  import it.unibo.alchemist.model.interfaces.Time
  def currTime: Long = sense[Time]("time").toDouble().toLong
}