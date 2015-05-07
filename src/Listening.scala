//https://github.com/AlexLamson/3DRenderer/blob/master/src/main/Listening.java

import java.awt.Color
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseMotionListener
import java.awt.event.MouseWheelEvent
import java.awt.event.MouseWheelListener
import java.util.ArrayList

class Listening extends KeyListener with MouseListener with  MouseMotionListener with MouseWheelListener
{
  var keys = Set[Integer]()
  
  var currMouseState = 0
  val mouseStates = List("Camera 1", "Camera 2", "Camera 3", "Camera 4")
  
  def get(str:String):Boolean = {
    val keyCode = getKeyEvent(str)
    return keys.contains(keyCode)
  }
  
  def get(ch:Char):Boolean = {
    val keyCode = getKeyEvent(ch)
    return keys.contains(keyCode)
  }
  
  def keyPressed(e:KeyEvent):Unit = {
    val key = e.getKeyCode()
    keys = keys + key
  }

  def keyReleased(e:KeyEvent):Unit = {
    val key = e.getKeyCode()
    keys = keys - key
    
    key match {
      case KeyEvent.VK_ESCAPE | KeyEvent.VK_P => Main.isPaused = !Main.isPaused
      case _ => {}
    }
  }

  def keyTyped(e:KeyEvent):Unit = {
    
  }
  
  def getKeyEvent(ch:Char):Int = {
    if(ch >= '0' && ch <= '9') KeyEvent.VK_0+ch-'0' //if number
    else if(ch >= 'a' && ch <= 'z') KeyEvent.VK_A+ch-'a' //if lowercase letter
    else if(ch >= 'A' && ch <= 'Z') KeyEvent.VK_A+ch-'A' //if uppercase letter
    else if(ch == ' ') KeyEvent.VK_SPACE //if space 
    else{
      Console.err.println("cannot find KeyEvent for: "+ch)
      throw new IllegalStateException()
    }
  }
  
  def getKeyEvent(str:String):Int = {
    if(str.length() == 1)
      return getKeyEvent(str.charAt(0))
    
    str.toLowerCase match {
      case "ctrl" | "control" => KeyEvent.VK_CONTROL
      case "shift" => KeyEvent.VK_SHIFT
      case "space" => KeyEvent.VK_SPACE
      case "up"    => KeyEvent.VK_UP
      case "down"  => KeyEvent.VK_DOWN
      case "left"  => KeyEvent.VK_LEFT
      case "right" => KeyEvent.VK_RIGHT
      case _ => {
        Console.err.println("cannot find KeyEvent for: "+str)
        throw new IllegalStateException()
      }
    }
  }
  
  def mouseClicked(e:MouseEvent):Unit = {
    Main.mse = (e.getX()/Main.pixelSize, e.getY()/Main.pixelSize)
//    System.out.println(save)
    mouseToggle(e, true)
    mouseToggle(e, false)
  }

  def mouseDragged(e:MouseEvent):Unit = {
    Main.mse = (e.getX()/Main.pixelSize, e.getY()/Main.pixelSize)
//    System.out.println(save)
  }

  def mousePressed(e:MouseEvent):Unit = {
    mouseToggle(e, true)
    mouseChanged()
    
    val left = e.getButton() == MouseEvent.BUTTON1
    val middle = e.getButton() == MouseEvent.BUTTON2
    val right = e.getButton() == MouseEvent.BUTTON3
    
//    currMouseState match {
//      case 0 => {
//        if(left){}
//        else if(right){}
//      }
//      case 1 => {
//        if(left){}
//        else if(right){}
//      }
//      case 2 => {
//        if(left){}
//        else if(right){}
//      }
//      case 3 => {
//        if(left){}
//        else if(right){}
//      }
//      case _ => Console.err.println("Mouse state not checked for")
//    }
  }

  def mouseReleased(e:MouseEvent):Unit = {
    mouseToggle(e, false)
    mouseChanged()
  }

  def mouseToggle(e:MouseEvent, toggle:Boolean):Unit = {
    if(e.getButton() == MouseEvent.BUTTON1)     //left click
      Main.isMouseLeft = toggle
    else if(e.getButton() == MouseEvent.BUTTON2)  //middle click
      Main.isMouseMiddle = toggle
    else if(e.getButton() == MouseEvent.BUTTON3)  //right click
      Main.isMouseRight = toggle
  }
  
  def mouseChanged():Unit = {}
  
  def mouseMoved(e:MouseEvent):Unit = {
    Main.mse = (e.getX(), e.getY())
  }

  def mouseWheelMoved(e:MouseWheelEvent):Unit = {
    val max = mouseStates.length
    
    def move(i:Int) = { currMouseState = (max+currMouseState+i)%max }
    
    if(e.getWheelRotation() < 0) //scrolled up
      move(1)
    else if(e.getWheelRotation() > 0) //scrolled down
      move(-1)
    
    World.selectedCamera = currMouseState
  }

  def mouseEntered(e:MouseEvent):Unit = {}

  def mouseExited(e:MouseEvent):Unit = {}
  
  def keysToString():String = keys.mkString(" ")
}
