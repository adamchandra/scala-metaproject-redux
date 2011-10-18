import processing.core._
import spde.core._
import PConstants._
import PApplet._


object Scratch {
  def main(args: Array[String]) {
    PApplet.main(Array(classOf[Scratch].getName))
  }
}

class Scratch extends ProxiedApplet {
  lazy val px = new DrawProxy(this) {
    override def draw(): Unit = {}
    // def mousePressed() { }
    // def mouseReleased() { }
    // def mouseClicked() { }
    // def mouseDragged() { }
    // def mouseMoved() { }
    // def keyPressed() { }
    // def keyReleased() { }
    // def keyTyped() { }
    // def focusGained() { }
    // def focusLost() { }

  }
}

