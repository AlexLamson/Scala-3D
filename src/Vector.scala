//representation of a 3d vector
class Vector(val x:Double, val y:Double, val z:Double=0) {
  def this(v:(Double, Double, Double)) = this(v._1, v._2, v._3)
  def this(v:(Double, Double)) = this(v._1, v._2)
  def this(v:Vector) = this(v.z, v.y, v.x)
  def this(p1:Point, p2:Point) = this(p2-p1)
  def this(p1:Vector, p2:Vector) = this(p2-p1)
  
  //add two vectors
  def add(vec:Vector):Vector = this + vec
  def +(vec:Vector):Vector = Vector(x+vec.x, y+vec.y, z+vec.z)
  def +(nums:(Double, Double, Double)):Vector = Vector(x+nums._1, y+nums._2, z+nums._3)
  
  //subtract two vectors
  def sub(vec:Vector):Vector = this - vec
  def -(vec:Vector):Vector = Vector(x-vec.x, y-vec.y, z-vec.z)
  def -(nums:(Double, Double, Double)):Vector = Vector(x-nums._1, y-nums._2, z-nums._3)
  
  //multiply all components of vector by given scalar
  def scale(num:Double):Vector = this * num
  def *(num:Double) = Vector(x*num, y*num, z*num)
  def /(num:Double):Vector = Vector(x/num, y/num, z/num)
  
  //multiply each component by corresponding tuple elements
  def scale(nums:(Double, Double, Double)):Vector = this * nums
  def *(nums:(Double, Double, Double)) = Vector(x*nums._1, y*nums._2, z*nums._3)
  
  //inverts direction of vector
  def flip():Vector = scale(-1)
  
  //find the dot product of two vectors
  def dot(vec:Vector):Double = this * vec
  def *(vec:Vector):Double = x*vec.x + y*vec.y + z*vec.z
  
  //find the cross product of two vectors
  def cross(vec:Vector):Vector = this x vec
  def x(vec:Vector):Vector = {
    val v1 = this
    val v2 = vec
    
    val x = v1.y*v2.z-v2.y*v1.z
    val y = v1.z*v2.x-v2.z*v1.x
    val z = v1.x*v2.y-v2.x*v1.y
    
    Vector(x, y, z)
  }
  
  //find the length of the vector
  def magnitude():Double = math.sqrt(x*x + y*y + z*z)
  def sqMagnitude():Double = x*x + y*y + z*z
  def unary_|() = magnitude
  def |() = this
  
  //find the unit vector of this vector
  def unit() = {
    val mag = this.magnitude
    Vector(x/mag, y/mag, z/mag)
  }
  
  //returns true if vector point in the same direction
  def isParallelTo(v:Vector):Boolean = (unit == v.unit)
  
  //returns true if vectors are orthogonal
  def isOrthogonalTo(v:Vector):Boolean = ((this*v) == 0)
  
  //rotate this vector about a given vector some given number of degrees
  def rotateAbout(rotateAbout:Vector, degrees:Double):Vector = {
    def cos(t:Double) = math.cos(t)
    def sin(t:Double) = math.sin(t)
    
    val k = rotateAbout.unit
    val v = this
    val t = math.toRadians(degrees)
    
    //http://en.wikipedia.org/wiki/Rodrigues'_rotation_formula
    (v*cos(t)) + ((k x v)*sin(t)) + (k*(k dot v)*(1.0 - cos(t)))
  }
  
  //converters
  def toPoint() = Point(x, y, z)
  def toTuple() = (x, y, z)
  def toLongString() = "<"+x+", "+y+", "+z+">"
  override def toString() = {
    def f(i:Double) = "%2.2f" format i
    "<"+f(x)+", "+f(y)+", "+f(z)+">"
  }
  
  override def equals(that:Any) = {
    //define doubles as equal if they are very close
    def aprox(i:Double, j:Double):Boolean = math.abs(i-j) <= 1e-6
    
    that match {
      case that:Vector => aprox(x,that.x) && aprox(y,that.y) && aprox(z,that.z)
      case _ => false
    }
  }
}
object Vector{
  def apply(x:Double, y:Double, z:Double=0) = new Vector(x, y, z)
  def apply(v:(Double, Double, Double)) = new Vector(v._1, v._2, v._3)
  def apply(v:(Double, Double)) = new Vector(v._1, v._2)
  def apply(v:Vector) = new Vector(v.z, v.y, v.x)
  def apply(p1:Point, p2:Point) = new Vector(p2-p1)
  def apply(p1:Vector, p2:Vector) = new Vector(p2-p1)
  
  val left     = Vector(-1,  0,  0)
  val right    = Vector( 1,  0,  1)
  val forward  = Vector( 0,  1,  0)
  val backward = Vector( 0, -1,  0)
  val up       = Vector( 0,  0,  1)
  val down     = Vector( 0,  0, -1)
}

//representation of a 3d point
class Point(override val x:Double, override val y:Double, override val z:Double) extends Vector(x,y,z){
  
  //add two points
  def add(p:Point):Point = add(p).toPoint
  def +(p:Point):Point = super.+(p).toPoint
  override def +(nums:(Double, Double, Double)):Point = super.+(nums).toPoint
  
  //subtract two points
  def sub(p:Point):Point = sub(p).toPoint
  def -(p:Point):Point = super.-(p).toPoint
  override def -(nums:(Double, Double, Double)):Point = super.-(nums).toPoint
  
  def closest(points:List[Point]):Option[Point] = {
    if(points.isEmpty) return None
    def closer(p1:Point, p2:Point):Point = if(distanceTo(p1) < distanceTo(p2)) p1 else p2
    Some(points.foldLeft(points(0))( (a,b) => closer(a,b) ))
  }
  
  def collinearWith(p1:Point, p2:Point) = new Line(p1, p2).contains(this)
  
  //average this point with the given point
  def avg(p:Point):Point = Point((x+p.x)/2, (y+p.y)/2, (z+p.z)/2)
  
  def distanceTo(p:Point) = {
    def sq(i:Double) = i*i
    math.sqrt(sq(x-p.x) + sq(y-p.y) + sq(z-p.z))
  }
  
  def sqDistanceTo(p:Point) = {
    def sq(i:Double) = i*i
    sq(x-p.x) + sq(y-p.y) + sq(z-p.z)
  }
  
  def toVector() = Vector(x, y, z)
  override def toLongString() = "("+x+", "+y+", "+z+")"
  override def toString() = {
    def f(i:Double) = "%2.2f" format i
    "("+f(x)+", "+f(y)+", "+f(z)+")"
  }
}
object Point{
  def apply(x:Double, y:Double, z:Double) = new Point(x, y, z)
  
  def avg(points:Point*):Point = {
    val total = points.length
    val sumPoint = points.foldLeft(Point.origin)((a:Point, b:Point) => a + b)
    val avg = sumPoint/total
    avg.toPoint
  }
  
  val origin = Point(0, 0, 0)
}
