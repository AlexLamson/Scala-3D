import java.applet.Applet
import java.awt.Color
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.FontMetrics
import java.awt.geom.Rectangle2D
import java.awt.Graphics
import java.awt.Graphics
import java.awt.Image
import java.awt.Toolkit
import java.util.ArrayList
import javax.swing.JFrame


//https://github.com/AlexLamson/3DRenderer/blob/master/src/main/Main.java
class Main extends Applet with Runnable{
  var screen = createVolatileImage(Main.pixel.width, Main.pixel.height)
  
  val fixedSize = new Dimension(Main.size.width-10,Main.size.height-10)
  setPreferredSize(fixedSize)
  requestFocus()

  override def start():Unit = {
    addKeyListener(Main.listening)
    addMouseListener(Main.listening)
    addMouseMotionListener(Main.listening)
    addMouseWheelListener(Main.listening)
    
    
    //defining objects
    val worldSize = 0.5
    val centerX = (worldSize*Main.pixel.width/2)
    val centerY = (worldSize*Main.pixel.height/2)
    
    World.width = (worldSize*Main.pixel.width).toInt
    World.height = (worldSize*Main.pixel.height).toInt
    
    
    //initialize objects here
    
    
    
    //start the main loop
    Main.isRunning = true
    new Thread(this).start()
    requestFocus()
  }

  override def stop():Unit = Main.isRunning = false

  def tick():Unit = {
    World.tick()
  }

  def render():Unit = {
    var g = screen.getGraphics()
    
    def pressed(key:String) = { Main.listening.get(key) }
    if(!pressed("ctrl")){
      g.setColor(new Color(200, 200, 200))
      g.fillRect(0, 0, Main.pixel.width, Main.pixel.height)
    }
    
    World.render(g)
    
    if(Main.isPaused)
      drawTextScreen(g, 
          "Game Paused\n\n"+
          "esc - unpause\n"+
          "wasd - move on xy plane\n"+
          "eq - move up and down\n"+
          "arrow keys - rotate\n"+
          "fg - change focal length\n"+
          ""
          , 18)
    else if(Main.isGameOver)
      drawTextScreen(g, "Game Over", 18)
    
    if(!Main.isPaused)
    {
      Main.mq.addMessage(Main.listening.mouseStates(Main.listening.currMouseState))
//      Main.mq.addMessage("Keys: "+Main.listening.keysToString())
      Main.mq.render(g)
    }
    Main.mq.clear()
    
    g = getGraphics()
    
    g.drawImage(screen, 0, 0, Main.size.width, Main.size.height, 0, 0, Main.pixel.width, Main.pixel.height, null)
    g.dispose() //throw it away to avoid lag from too many graphics objects
  }
  
  def drawTextScreen(g:Graphics, message:String, fontSize:Int):Unit = {
    val lines = message.split("\n")
    
    val width = Main.pixel.width
    val height = Main.pixel.height
    
    g.setColor(new Color(100, 100, 100, 200))
    g.fillRect(0, 0, width, height)
    
    val fontSave = g.getFont()
    
    val font = new Font("Verdana", 6, fontSize)
    
    val totalTextHeight = message.split("\n").length*g.getFontMetrics(font).getHeight
    val startingHeight = height/2-totalTextHeight/2
    
    val totalTextWidth = (for{
      line <- lines
      width = g.getFontMetrics(font).stringWidth(line)
    } yield width).max
    val startingWidth = width/2-totalTextWidth/2
    
    g.setFont(font)
    g.setColor(Color.black)
    for((line,i) <- lines.zipWithIndex)
      g.drawString(line, startingWidth, startingHeight+i*25)
    
    g.setFont(fontSave)
  }
  
  def run():Unit = {
    screen = createVolatileImage(Main.pixel.width, Main.pixel.height)  //actually use the graphics card (less lag)
    
    render()
    
    while(Main.isRunning)
    {
      if(!Main.isPaused && !Main.isGameOver)
        tick()     //do math and any calculations
      render()
      
      Thread.sleep(Main.tickTime)
    }
  }
}
object Main{
  val pixelSize = 1 //change the scale the pixels are multiplied by when drawn to
  
  val tickTime = 50
  val ticksPerSecond = (1000/tickTime)
  
  val windowName = "Scala is awesome"

  val debugMode = false
  
  val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
  val screenWidth = screenSize.getWidth.toInt
  val screenHeight = screenSize.getHeight.toInt
//  val size = new Dimension(screenWidth*3/4,screenHeight*3/4) //drawable area
  val size = new Dimension(screenHeight*7/8,screenHeight*7/8) //square window
  
  val pixel = new Dimension(size.width/pixelSize, size.height/pixelSize) //"pixels" in drawable area
  
  var (isRunning, isPaused, isGameOver) = (false, false, false)
  var (isMouseLeft, isMouseMiddle, isMouseRight) = (false, false, false)
  var mq = new MessageQueue(10, 10)
  var mse = (0, 0)
  var listening = new Listening()
  var frame = new JFrame()
  
  def restart():Unit = {
    var viewer = new Main()
    viewer.start()
  }
  
  def main(args: Array[String]) = {
    var main = new Main()
    
    frame.add(main)
    frame.pack()
    
    frame.setTitle(windowName)
    frame.setResizable(false)
    frame.setLocationRelativeTo(null) //null makes it go to the center
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)
    
    main.start()
  }
}


class MessageQueue(var x:Int, var y:Int){
  var maxTextWidth = 0
  var strings = List[String]()
  
  def addMessage(s:String):Unit = {
    strings = s :: strings
//    strings.add(s)
    
    val g = Main.frame.getGraphics()
    g.getFont()
    val fm = g.getFontMetrics
    val rect = fm.getStringBounds(s, g)
    val textWidth = rect.getWidth.toInt
    
    if(textWidth > maxTextWidth)
      maxTextWidth = textWidth
  }
  
  def render(g:Graphics):Unit = {
    g.setColor(new Color(255, 255, 255, 255*3/4))
    g.fillRect(x-5, y, maxTextWidth+10, (strings.length)*20)
    
    g.setColor(Color.black)
    for(i <- 0 to strings.length-1)
      g.drawString(strings(i), x, y+(i+1)*20-5)
  }
  
  def clear():Unit = {
    strings = List[String]()
    maxTextWidth = 0
  }
}
