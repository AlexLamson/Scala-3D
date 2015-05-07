import java.awt.Color
import java.awt.Graphics;
import java.awt.Polygon;

class Marker(pos:Point=Point.origin, s:String="") 
extends VisibleObject(List(pos), List(new VStr(0, s, Color.red)))

//https://github.com/AlexLamson/3DRenderer/blob/master/src/entities/Cube.java
class Cube(pos:Point=Point.origin, val width:Double) 
extends VisibleObject(Cube.makePoints(width), Cube.makeVisibles)
object Cube{
  def makePoints(width:Double):List[Point] = {
    List(
      Point(  width/2,  width/2,  width/2 ),
      Point(  width/2, -width/2,  width/2 ),
      Point( -width/2, -width/2,  width/2 ),
      Point( -width/2,  width/2,  width/2 ),
      Point(  width/2,  width/2, -width/2 ),
      Point(  width/2, -width/2, -width/2 ),
      Point( -width/2, -width/2, -width/2 ),
      Point( -width/2,  width/2, -width/2 ),
      Point( 0,0,0 )
    )
  }
  
  def makeVisibles():List[Visible] = {
    val faces = List(
      (0,1,2,3),
      (0,1,5,4),
      (1,2,6,5),
      (2,3,7,6),
      (0,3,7,4),
      (4,5,6,7)
    )
    def ranInt():Int = (math.random*256).toInt
    def randColor():Color = new Color(ranInt, ranInt, ranInt, 256/2)
    val list = for{ face <- faces } yield new VQuad(face, randColor)
    list ::: List(new VStr(8, "Cube"))
  }
}

//Pyramid

//Sphere
//https://github.com/AlexLamson/3DRenderer/blob/master/src/entities/Sphere.java
class Sphere(pos:Point=Point.origin, val radius:Double, val zs:Int=30, val yaws:Int=30) 
extends VisibleObject(Sphere.makePoints(radius, zs, yaws), Sphere.makeVisibles(zs, yaws))
object Sphere{
  def makePoints(radius:Double, zs:Int, yaws:Int):List[Point] = {
    val points = for{
      z <- -radius+radius/zs to radius by 2*radius/zs
//      z <- -1.0 to 1.0 by 2.0/zs
      theta <- 0 to 360 by 360/yaws
//      r = radius*math.sin( math.Pi * z/(2.0*radius) )
      r = math.sqrt(radius*radius-(z*z))
      t = theta.toRadians
      x = r*math.cos(t)
      y = r*math.sin(t)
      p = Point(x, y, z)
    } yield p
    
    points.toList
  }
  
  def makeVisibles(zs:Int, yaws:Int):List[Visible] = {
    val list = for{ p <- 0 to zs*yaws } yield new VPoint(p)
    list.toList
  }
}


class Axis(val length:Double) 
extends VisibleObject(Axis.makePoints(length), Axis.makeVisibles){
  pos = Point.origin
}
object Axis{
  def makePoints(length:Double):List[Point] = {
    val l = length
    List(
      Point( 0 ,0 , 0 ),
      Point( -l,0 , 0 ),
      Point( l ,0 , 0 ),
      Point( 0 ,-l, 0 ),
      Point( 0 ,l , 0 ),
      Point( 0 ,0 ,-l ),
      Point( 0 ,0 , l )
    )
  }
  
  def makeVisibles():List[Visible] = {
    val edges = List(
      (0,1),
      (0,2),
      (0,3),
      (0,4),
      (0,5),
      (0,6)
    )
    for{ edge <- edges } yield new VEdge(edge, Color.black)
  }
}
