//add an orthogonal render method (have it take a vector)

import java.awt.Color
import java.awt.Graphics;
import java.awt.Polygon;

object C{
  val black = new Color(0, 0, 0, 256/2)       //black
  val red = new Color(255, 0, 0, 256/2)       //red
  val orange = new Color(255, 100, 0, 256/2)  //orange
  val yellow = new Color(255, 255, 0, 256/2)  //yellow
  val green = new Color(0, 255, 0, 256/2)     //green
  val blue = new Color(0, 0, 255, 256/2)      //blue
  val magenta = new Color(255, 0, 255, 256/2) //magenta
}

class Visible(val color:Color=C.black)
class VPoint(val p:(Int), override val color:Color=C.red) extends Visible(color)
class VStr(override val p:(Int), val str:String, override val color:Color=C.magenta) extends VPoint(p, color)
class VEdge(val p:(Int, Int), override val color:Color=C.orange) extends Visible(color)
class VTri(val p:(Int, Int, Int), override val color:Color=C.yellow) extends Visible(color)
class VQuad(val p:(Int, Int, Int, Int), override val color:Color=C.green) extends Visible(color)
object Visible{
  def getVis(p:(Int)):VPoint = new VPoint(p)
  def getVis(p:(Int), str:String):VStr = new VStr(p, str)
  def getVis(p:(Int, Int)):VEdge = new VEdge(p)
  def getVis(p:(Int, Int, Int)):VTri = new VTri(p)
  def getVis(p:(Int, Int, Int, Int)):VQuad = new VQuad(p)
}

//https://github.com/AlexLamson/3DRenderer/tree/master/src/entities

//https://github.com/AlexLamson/3DRenderer/blob/master/src/entities/Entity.java
abstract class Entity(var pos:Point=Point.origin, val orientation:Vector=Vector.up) {
  def tick():Unit
  def render(g:Graphics):Unit
  override def toString():String = pos.toString
}
  
//https://github.com/AlexLamson/3DRenderer/blob/master/src/entities/VisibleObject.java
class VisibleObject(val points:List[Point], val visibles:List[Visible]) extends Entity{
  
  def getIntersections(line:Line):List[Point] = getIntersectionsAndColors(line).map(_._1)
  
  def getIntersectionsAndColors(line:Line):List[(Point,Color)] = {
    def intersect(v:Visible) = 
      v match {
        case v:VPoint => None
        case v:VEdge => None
        case v:VTri => to3DMath(v).getIntersection(line)
        case v:VQuad => to3DMath(v).getIntersection(line)
      }
    
    val intersectionPoints = for{
      v <- visibles
      x = intersect(v)
      if(x.isInstanceOf[Some[Point]])
    } yield (x.get,v.color)
    
    intersectionPoints.toList
  }
  
  def tick() = {}
  
  def render(g:Graphics) = {
    //https://github.com/AlexLamson/3DRenderer/blob/master/src/entities/VisibleObject.java#L66
    
    def getPos(i:Int):(Int, Int) = World.camera.getScreenPos((pos+p(i)).toPoint) getOrElse( (0,0) )
    
    for(v <- visibles){
      g.setColor(v.color)
      v match {
        case v:VStr => {
          val (x,y) = getPos(v.p)
          g.drawString(v.str, x, y)
        }
        case v:VPoint => {
          val (x,y) = getPos(v.p)
          val r = 2 //5
          g.drawOval(x-r, y-r, 2*r, 2*r)
        }
        case v:VEdge => {
          val (x1,y1) = getPos(v.p._1)
          val (x2,y2) = getPos(v.p._2)
          g.drawLine(x1, y1, x2, y2)
        }
        case v:VTri => {
          val (x1,y1) = getPos(v.p._1)
          val (x2,y2) = getPos(v.p._2)
          val (x3,y3) = getPos(v.p._3)
          val poly = new Polygon(Array(x1, x2, x3), Array(y1, y2, y3), 3)
          g.fillPolygon(poly)
        }
        case v:VQuad => {
          val (x1,y1) = getPos(v.p._1)
          val (x2,y2) = getPos(v.p._2)
          val (x3,y3) = getPos(v.p._3)
          val (x4,y4) = getPos(v.p._4)
          val poly = new Polygon(Array(x1, x2, x3, x4), Array(y1, y2, y3, y4), 4)
          g.fillPolygon(poly)
          g.setColor(C.blue)
          g.drawPolygon(poly)
        }
        case _ => println("Unknown visible used in render method")
      }
    }
  }
  
  private def p(i:Int) = points(i)
  
  def to3DMath(v:VPoint) = points(v.p)
  def to3DMath(v:VEdge) = new LineSegment(p(v.p._1), p(v.p._2))
  def to3DMath(v:VTri) = new Triangle(p(v.p._1), p(v.p._2), p(v.p._3))
  def to3DMath(v:VQuad) = new Rectangle(p(v.p._1), p(v.p._2), p(v.p._3))
}
