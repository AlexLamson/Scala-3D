import java.awt.Graphics

//https://github.com/AlexLamson/3DRenderer/blob/master/src/main/World.java
object World{
  var width = 100
  var height = 100
  
  var objects = List[VisibleObject]()
  var cameras = List[Camera]()
  var selectedCamera = 0
  
  //TODO get raytracing working
  val raytracing = true //shooting line through pixel locations and checking for object intersections
  val connectingTheDots = true //older & faster method of rendering
  
  add(new Axis(1))
  val myCube = new Cube(Point(0, 0, 0), 0.1)
  add(myCube)
//  val mySphere = new Sphere(Point(0, 0, 0), 0.1)
//  add(mySphere)
  
  
  add(new Camera(Vector.forward, Point(0, -.13, 0)))
  add(new Camera(Vector(0.1, 0, 0), Point(0.1, 0, 0)))
  add(new Camera(Vector(0.1, 0.1, 0).unit*0.5, Point(0.1, 0, 0)))
  add(new Camera(Vector.forward, Point(0, 0, .13))) //TODO DEBUG
  
  //DEBUG
//  add(new Axis(10))
//  add(new Cube(Point(0, 0, 0), 0.1))
//  add(new Cube(Point(0, 0, 0), 2))
//  add(new Camera(Point(7.48113, -6.50764, 5.34367), 35/1000.0)) //35 mm focal length
//  add(new Camera(Vector.forward, Point.origin))
//  add(new Camera(Vector(0.1, 0.1, 0).unit*0.1, Point(0.1, 0, 0)))
  
  cameras(1).lookAt(Point.origin)
  var t = 0
  var r = 0.14
  
  def add(e:VisibleObject) { objects = objects :+ e }
  def add(c:Camera) { cameras = cameras :+ c }
  def remove(e:VisibleObject) { objects = objects diff List(e) }
  def remove(c:Camera) { cameras = cameras diff List(c) }
  
  def camera() = cameras(selectedCamera)
  
  def allObjects():List[VisibleObject] = objects ::: cameras.filter { _ ne camera }
  
  def tick():Unit = {
    for(obj <- objects) obj.tick()
    camera.tick()
    Camera.moveCam(camera)
    
    cameras(1).lookAt(Point.origin)
    cameras(3).lookAt(Point.origin)
    
    
    //fun with camera movement
    t += 1
    def pressed(key:String) = { Main.listening.get(key) }
    if(pressed("z")) r += -0.01
    if(pressed("x")) r += 0.01
    val deltaDegrees = 1
    val rad = ((t*deltaDegrees) % 360).toRadians
    cameras(2).pos = Point(r*math.cos(rad), r*math.sin(rad), r)
    cameras(2).lookAt(Point.origin)
    //TODO DEBUG
//    cameras(0).pos = Point(r*math.cos(rad), r*math.sin(rad), r)
//    cameras(0).lookAt(Point.origin)
  }
  
  def render(g:Graphics):Unit = {
    
    //TODO DEBUG
    val s = if(camera.useMethod1) "1" else "2"
    Main.mq.addMessage("using method "+s)
    
    if(connectingTheDots)
      for(obj <- allObjects) obj.render(g)
    if(raytracing)
      camera.renderView(g, allObjects)
  }
}
