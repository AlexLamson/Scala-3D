import java.awt.Graphics;
import java.awt.Color
import java.awt.image.BufferedImage

/*
 * To fix the duplicate methods problem
 * make the constructor as if all the calculation that would be done in the object has already been done
 * pass those arguments in directly
 * create a second constructor that actually creates those final arguments from something lower level
 * 
 */

//TODO change this to use line-sphere intersection to try to fix distortion (plus it would look cool)

//representation of a camera of 3d space
class Camera(var lookVec:Vector, localpos:Point) 
extends VisibleObject(Camera.makePoints(lookVec, localpos, Camera.lensWidth, Camera.lensHeight), Camera.makeVisibles){
  def this(pos:Point, focalLength:Double) = this(Vector(0,focalLength,0),pos)
  
  //TODO add this to second constructor after making lensWidth non-static
  //focalLength = 0.5
  //theta = 40
  //lensWidth = focalLength*2*tan(theta.toRadians/2)
  
  pos = this.localpos
  
  var useMethod1 = true
  
  def getRectangle():Rectangle = new Rectangle(topLeft, topRight, bottomLeft)
  def getPlane() = new Plane(lookVec, center.toPoint)
  
  //determine the screen coords of a point
  def getScreenPos(point:Point):Option[(Int, Int)] = {
    //if(getPlane.sameSide(point, pos)) return None
    
    if(useMethod1){
      val camHorz = right*lensWidth //the width of the lens as a vector
      val camVert = down*lensHeight //the height of the lens as a vector
      
      val line = new Line(this.pos, point)
      val intersectionPoint = getPlane.getIntersection(line).getOrElse( return None )
      
      val localIntersection = intersectionPoint-center
      val u = localIntersection dot camHorz
      val v = localIntersection dot camVert
      
      val screenX = (((u/lensWidth)*Main.screenWidth) + Main.pixel.width/2.0).round.toInt
      val screenY = (((v/lensHeight)*Main.screenHeight) + Main.pixel.height/2.0).round.toInt
      
      Some((screenX, screenY))
    }
    else{
      
      //TODO DEBUG
      
      val line = new Line(this.pos, point)
      val intersectionPoint = getPlane.getIntersection(line).getOrElse( return None )
      
      val localIntersection = intersectionPoint-center//topLeft
      val u = localIntersection dot right//camHorz
      val v = localIntersection dot down//camVert
      
      val screenX = (((u/lensWidth)*Main.screenWidth) + Main.pixel.width/2.0).round.toInt
      val screenY = (((v/lensHeight)*Main.screenHeight) + Main.pixel.height/2.0).round.toInt
      
      Some((screenX, screenY))
    }
    
  }
  
  //TODO fix this
  //convert a screen position in pixels to a point on camera's sensor
  def getWorldPos(point:(Int, Int)):Point = {
    val (screenX, screenY) = point
    
    val screenWidth = Main.pixel.width
    val screenHeight = Main.pixel.height
    
    val camHorz = right*lensWidth //the width of the lens as a vector
    val camVert = down*lensHeight //the height of the lens as a vector
//    val camHorz = right //the width of the lens as a vector
//    val camVert = down //the height of the lens as a vector
    
//    val u = (screenX-Main.pixel.width/2.0)/Main.screenWidth*lensWidth
//    val v = (screenY-Main.pixel.height/2.0)/Main.screenHeight*lensHeight
//    (pos + lookVec + camHorz*u + camVert*v).toPoint
    
    val xComp = camHorz*(1.0*screenX/screenWidth)
    val yComp = camVert*(1.0*screenY/screenHeight)
    (topLeft + xComp + yComp).toPoint
  }
  
  //shoot a  line through a given pixel
  def makePixelLine(pixel:(Int, Int)):Line = new Line(pos, getWorldPos(pixel))
  
  //TODO
  //use ray tracing to render everything that the camera is seeing
  def renderView(g:Graphics, objects:List[VisibleObject]) {
    
    val screenWidth = Main.pixel.width
    val screenHeight = Main.pixel.height
    
    var img = new BufferedImage(screenWidth, screenHeight, BufferedImage.TYPE_INT_RGB)
    
    
    //TODO make this work
    def drawCircleAt(pixel:(Int, Int), color:Color=Color.orange) = {
//      val line = makePixelLine(pixel)
//      val intersection = getPlane.getIntersection(line)
      getScreenPos(getWorldPos(pixel)) match{
        case Some((xPos,yPos)) => {
          val radius = 5
          g.setColor(color)
          g.drawOval(xPos-radius, yPos-radius, radius*2, radius*2)
        }
        case None => {}
      }
    }
    
    
    //TODO DEBUG draw a red X on the screen
//    val p1 = getScreenPos(getWorldPos(0,0)).getOrElse( (0,0) )
//    val p2 = getScreenPos(getWorldPos(screenWidth,screenHeight)).getOrElse( (0,0) )
//    val p3 = getScreenPos(getWorldPos(screenWidth,0)).getOrElse( (0,0) )
//    val p4 = getScreenPos(getWorldPos(0,screenHeight)).getOrElse( (0,0) )
    val p1 = getScreenPos(topLeft).getOrElse( (0,0) )
    val p2 = getScreenPos(bottomRight).getOrElse( (0,0) )
    val p3 = getScreenPos(topRight).getOrElse( (0,0) )
    val p4 = getScreenPos(bottomLeft).getOrElse( (0,0) )
    
    g.setColor(Color.red)
    g.drawLine(p1._1, p1._2, p2._1, p2._2)
    g.drawLine(p3._1, p3._2, p4._1, p4._2)
    
    
    drawCircleAt((0,0))
    drawCircleAt((screenWidth,0))
    drawCircleAt((0,screenHeight))
    drawCircleAt((screenWidth,screenHeight))
    drawCircleAt((screenWidth/2,screenHeight/2),Color.yellow)
    
    
//    val deltaScale = 10
//    val xDelta = (screenWidth-0)/(deltaScale-0)
//    val yDelta = (screenHeight-0)/(deltaScale-0)
//    //TODO make this use a z-buffer (where each distance is saved to a 2d array and then the colors are drawn afterward)
//    //TODO make it color regions of pixels instead of individual pixels
//    for(y <- 0 to screenHeight-1 by yDelta){
//      for(x <- 0 to screenWidth-1 by xDelta){
//        val line = makePixelLine(x, y)
//        
//        //for all the points of intersection that this object has with the line, find the closest one
//        //(might want to use a for comprehension)
//        
//        //add the intersections of all the objects to a list to find the closest point for this ray using all the objects
//        val points = (for{
//          obj <- objects
//          points = obj.getIntersections(line)
//        } yield points).flatten
////        val pointsInFrontOfCamera = points.filterNot{ p => getPlane.sameSide(pos, p) }
//        val closestPoint = pos.closest(points)
//        
//        def bound(i:Double) = math.max(math.min(255, i),0).toInt
//        def scale(i:Double) = bound((i-.1)*(10))
//        def toColor(i:Double) = new Color(scale(i), scale(i), scale(i), 80)
//        
//        val pixelColor = closestPoint match {
////            case Some(p) => toColor(pos.distanceTo(p)/1.0) //color pixel based on distance to intersection
////            case None => new Color(255, 255, 255, 255/2) //default color is white if there are no intersections
//          //TODO DEBUG
//          case Some(p) => new Color(255, 255, 255, 80)
//          case None => new Color(0, 0, 0, 50)
//        }
//        
//        if(xDelta == 1 && yDelta == 1)
//          img.setRGB(x, y, pixelColor.getRGB) //TODO make the color match the color of the object
//        else{
//          g.setColor(pixelColor)
//          g.fillRect(x, y, xDelta, yDelta)
//          //TODO just for fun
////          g.setColor(Color.green)
////          g.drawRect(x, y, xDelta, yDelta)
//        }
//        
//        
//      }
//    }
//    
//    if(xDelta == 1 && yDelta == 1) g.drawImage(img, 0, 0, null)
  }
  
  def lookAt(p:Point) { lookVec = (p-pos).unit*lookVec.magnitude }
  
  def lensWidth() = Camera.lensWidth
  def lensHeight() = Camera.lensHeight
  def speed() = Camera.speed
  
  def center()      = Camera.center(lookVec, pos)
  def topLeft()     = Camera.topLeft(lookVec, pos, lensWidth, lensHeight)
  def topRight()    = Camera.topRight(lookVec, pos, lensWidth, lensHeight)
  def bottomLeft()  = Camera.bottomLeft(lookVec, pos, lensWidth, lensHeight)
  def bottomRight() = Camera.bottomRight(lookVec, pos, lensWidth, lensHeight)
  
  def forward()  = Camera.forward(lookVec)
  def backward() = Camera.backward(lookVec)
  def left()     = Camera.left(lookVec)
  def right()    = Camera.right(lookVec)
  def up()       = Camera.up(lookVec)
  def down()     = Camera.down(lookVec)
}
object Camera{
  var (lensWidth, speed) = (0.01, 0.01)
  def lensHeight = lensWidth/Main.pixel.width*Main.pixel.height
  
  def moveCam(cam:Camera) {
    Main.mq.addMessage("Camera pos: "+cam.pos)
    
    def pressed(key:String) = { Main.listening.get(key) }
    def move(v:Vector) = { cam.pos = (cam.pos + v).toPoint }
    
    def accel() = { if(pressed("shift")) 2.0 else 1.0 }
    val acc = accel()
    
    //TODO DEBUG
    if(pressed("k")) lensWidth *= 1.1
    if(pressed("l")) lensWidth *= 0.9
    if(pressed("j")) Camera.lensWidth = 1
    
    //TODO DEBUG
    if(pressed("m")) cam.useMethod1 = !cam.useMethod1
    
    if(pressed("w")) move(cam.forward*speed*acc)
    if(pressed("a")) move(cam.left*speed*acc)
    if(pressed("s")) move(cam.backward*speed*acc)
    if(pressed("d")) move(cam.right*speed*acc)
    if(pressed("e")) move(cam.up*speed*acc)
    if(pressed("q")) move(cam.down*speed*acc)
    
    val deltaDegrees = 1.5
    if(pressed("up")) cam.lookVec = cam.lookVec.rotateAbout(cam.left, -deltaDegrees*acc)
    if(pressed("down")) cam.lookVec = cam.lookVec.rotateAbout(cam.left, deltaDegrees*acc)
    if(pressed("left")) cam.lookVec = cam.lookVec.rotateAbout(Vector.down, -deltaDegrees*acc)
    if(pressed("right")) cam.lookVec = cam.lookVec.rotateAbout(Vector.down, deltaDegrees*acc)
    
    if(pressed("f")) cam.lookVec *= 1.1
    if(pressed("g")) cam.lookVec *= 0.9
    
    if(pressed("space")) println(cam.pos+", "+cam.lookVec+", "+cam.lookVec.magnitude)
  }
  
  def center(v:Vector, p:Point) = v + p
  def topLeft(v:Vector, p:Point, w:Double, h:Double)     = (center(v,p) - right(v)*(w/2) + up(v)*(h/2)).toPoint
  def topRight(v:Vector, p:Point, w:Double, h:Double)    = (center(v,p) + right(v)*(w/2) + up(v)*(h/2)).toPoint
  def bottomLeft(v:Vector, p:Point, w:Double, h:Double)  = (center(v,p) - right(v)*(w/2) - up(v)*(h/2)).toPoint
  def bottomRight(v:Vector, p:Point, w:Double, h:Double) = (center(v,p) + right(v)*(w/2) - up(v)*(h/2)).toPoint
  
  def forward(v:Vector) = v.unit
  def backward(v:Vector) = forward(v).flip
  def right(v:Vector) = (v x Vector.up).unit
  def left(v:Vector) = right(v).flip
  def up(v:Vector) = (forward(v) x left(v)).unit
  def down(v:Vector) = up(v).flip

  def makePoints(lookVec:Vector, pos:Point, lensWidth:Double, lensHeight:Double):List[Point] = {
    List(
      topLeft(lookVec, pos, lensWidth, lensHeight),
      topRight(lookVec, pos, lensWidth, lensHeight),
      bottomLeft(lookVec, pos, lensWidth, lensHeight),
      bottomRight(lookVec, pos, lensWidth, lensHeight),
      pos,
      lookVec.toPoint
    )
  }
  
  def makeVisibles():List[Visible] = {
    val faces = List(
      (0,1), //topLeft - topRight
      (0,2), //topLeft - bottomLeft
      (2,3), //bottomLeft - bottomRight
      (1,3), //topRight - bottomRight
      (0,4), //topLeft - pos
      (1,4), //topRight - pos
      (2,4), //bottomLeft - pos
      (3,4) //bottomRight - pos
//      ,
//      (4,5)  //pos - lookVector
    )
    val list:List[Visible] = for{ face <- faces } yield new VEdge(face)
    new VEdge((4,5),Color.red) :: new VStr(4, "Camera") :: list
  }
}
