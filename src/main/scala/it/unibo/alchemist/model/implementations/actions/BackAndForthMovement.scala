package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.interfaces._
import it.unibo.alchemist.model.implementations.positions.Continuous2DEuclidean
import it.unibo.alchemist.model.implementations.positions.ContinuousGenericEuclidean
import scala.util.Random


class BackAndForthMovement[T](env: Environment[T], node: Node[T], 
    var xdir: Double, val xstart: Double, val xuntil: Double, val xrand: Double,
    var ydir: Double, val ystart: Double, val yuntil: Double, val yrand: Double, val seed: Long = 0) 
  extends AbstractMoveNode(env,node){
  lazy val rand = new Random(seed)
  
  def getNextPosition(): Position = {
    val curr = getCurrentPosition().asInstanceOf[ContinuousGenericEuclidean]
    def signum(x: Double) = x.signum match { case 0 => 1; case x => x }
    val xdelta = xdir + signum(xdir)*(rand.nextDouble()*xrand-xrand/2)
    val ydelta = ydir + signum(ydir)*(rand.nextDouble()*yrand-yrand/2)
    
    val newPos = new ContinuousGenericEuclidean(curr.getCoordinate(0) + xdelta, curr.getCoordinate(1) + ydelta)
    val (newx, newy) = (newPos.getCoordinate(0), newPos.getCoordinate(1))
    if((newx>=xuntil-xdir && xdir.signum>0) || (newx<=xstart+xdir && xdir.signum<0)) xdir = -1 * xdir
    if((newy>=yuntil-ydir && ydir.signum>0) || (newy<=ystart+ydir && ydir.signum<0)) ydir = -1 * ydir    
    
    new ContinuousGenericEuclidean(xdelta,ydelta)
  }
  
  def cloneAction(node: Node[T], reaction: Reaction[T]): Action[T] = ???
}
