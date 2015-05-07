/*
TODO
Add epsilons to everything (compare to a threshold instead of zero directly)
 */

//representation of a 3d line
class Line(val vec:Vector, val pos:Point){
  def this(p1:Point, p2:Point) = this((p2-p1).toVector, p1)
  
  def getPoint(t:Double):Point = ((vec*t)+pos).toPoint
  
  //return true if the given point lays on the line
  def contains(point:Point):Boolean = (point-this.pos).isParallelTo(this.vec)
  
  //TODO check that this works
  //math.harvard.edu/~ytzeng/worksheet/distance.pdf
  //return distance from point to this line
  def distance(point:Point):Double = ((pos-point) x vec).magnitude / vec.magnitude
  
  //TODO check that this works
  //find distance between this line and another line
  def distance(line:Line):Double =  ((pos-line.pos) dot (vec x line.vec)) / (vec x line.vec).magnitude
  
  override def toString() = vec+" "+pos
}

//representation of a 3d line segment
class LineSegment(val p1:Point, val p2:Point){
  def length() = p1.distanceTo(p2)
  
  def contains(point:Point):Boolean = {
    if(!toLine.contains(point)) return false
    
    val maxDist = p1.distanceTo(p2)
    if(point.distanceTo(p1) > maxDist) return false
    if(point.distanceTo(p2) > maxDist) return false
    else true
  }
  
  //return distance from point to this line
  def distance(point:Point):Double = toLine.distance(point)
  
  def toLine() = new Line(p2-p1, p1)
  
  override def toString() = p1+" "+p2
}

//representation of a plane
class Plane(val normal:Vector, val pos:Point){
  def this(a:Double, b:Double, c:Double, d:Double) = 
    this(Vector(a, b, c), Point(0, 0, -d/c))
    //note the the point chosen is arbitrary
  
  //return the values of a,b,c,d for the cartesian form equation ax+by+cz+d=0
  def a() = normal.x
  def b() = normal.y
  def c() = normal.z
  def d() = -(normal dot pos)
  
  //return true if point is contained by plane
  def contains(point:Point):Boolean = (math.abs((normal*point)+d) < 1e-6)
  
  //return distance from point to this plane
  def distance(point:Point):Double = math.abs((normal*point)+d)/(normal.magnitude)
  
  //returns true if two points are on the same side of the plane
  //TODO determine why this is failing with camera
  def sameSide(p1:Point, p2:Point):Boolean = {
    
    //TODO check behavior when point(s) is/are contained by the plane
    val p1Side = (normal*p1)+d > 0
    val p2Side = (normal*p2)+d > 0
    
    p1Side == p2Side
  }
  
  def getIntersection(line:Line):Option[Point] = {
    //return None if line is (parallel to/contained by) the plane
    if(math.abs(line.vec dot this.normal) < 1e-6) return None
    
    //return Some(point) otherwise
    val t = (-d-(normal dot line.pos)) / (normal dot line.vec)
    Some(line.getPoint(t))
  }
  
  override def toString() = normal+" "+pos
}

//representation of a bounded plane in 3d
//TODO clarify that this is a parallelogram
//p1 is corner point, p2 & p3 are adjacent points to the corner
class Rectangle(override val p1:Point, override val p2:Point, override val p3:Point) extends Triangle(p1, p2, p3){
  def p4():Point = (p2-p1+p3).toPoint
  
  override def center() = ((p1+p2+p3+p4)/4).toPoint
  
  //check if point is contained by the rectangle
  override def contains(point:Point):Boolean = {
    if(!toPlane.contains(point)) return false
    
    //each plane contains edge
    val p12 = edgeToPlane(p1, p2)
    val p23 = edgeToPlane(p2, p3)
    val p34 = edgeToPlane(p3, p4)
    val p41 = edgeToPlane(p4, p1)
    
    //check that given point is on same side as the center of the parallelogram
    if(!withinEdge(p12, point)) return false
    if(!withinEdge(p23, point)) return false
    if(!withinEdge(p34, point)) return false
    if(!withinEdge(p41, point)) return false
    
    true
  }
  
  override def toString() = p1+" "+p2+" "+p3
}

class Triangle(val p1:Point, val p2:Point, val p3:Point){
  
  def getIntersection(line:Line):Option[Point] = {
    val intersection = toPlane.getIntersection(line)
    intersection match {
      case None => return None
      case Some(p) => {
        if(this.contains(p)) return Some(p)
        else return None
      }
    }
  }
  
  def center():Point = ((p1+p2+p3)/3).toPoint
  
  protected def edgeToPlane(pA:Point, pB:Point) = new Plane( Vector(center, ((pA+pB)/2).toPoint) , pA )
  protected def withinEdge(p:Plane, point:Point) = p.sameSide(center, point)
  
  //check if point is contained by the rectangle
  def contains(point:Point):Boolean = {
    if(!toPlane.contains(point)) return false
    
    //each plane contains edge
    val p12 = edgeToPlane(p1, p2)
    val p23 = edgeToPlane(p2, p3)
    val p31 = edgeToPlane(p3, p1)
    
    //check that given point is on same side as the center of the parallelogram
    if(!withinEdge(p12, point)) return false
    if(!withinEdge(p23, point)) return false
    if(!withinEdge(p31, point)) return false
    
    true
  }
  
  def toPlane() = new Plane(((p2-p1)x(p3-p1)).unit, center)
  
  override def toString() = p1+" "+p2+" "+p3
}

//class Sphere(pos:Point, radius:Double)

object Test{
  def main(args: Array[String]) = {
    
  }
}
