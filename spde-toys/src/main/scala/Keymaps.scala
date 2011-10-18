import processing.core._
import PConstants._
import scala.collection.{mutable=>mut}

object Keymapping {
  val xCTRL = 20
  val xSHIFT = 16
  val xCAPS = 17
  val xALT = 18
  def noop():Unit = {}
}

class Keymapping(keymap:List[(Set[AnyVal], Option[()=>Unit], Option[()=>Unit]) ], fallback:Option[(Char, Int)=>Unit]) {
  var mods = mut.Set[AnyVal]()

  def keyMapping(chord:Set[AnyVal]): (Option[()=>Unit], Option[()=>Unit]) = {
    for (k <- keymap) {
      val kmapChord = k._1.toSet
      // println("trying " + chord + " == " + kmapChord)
      if (chord == kmapChord) {
        return (k._2, k._3)
      }
    }
    (None, None)
  }

  def keyPressed(key:Char, keyCode:Int):Unit = key match {
    case CODED => mods.add(keyCode);
    case _     => { 
      keyMapping(mods.toSet + keyCode) match {
        case (Some(f1), _) => f1();
        case _ => fallback.foreach (_(key, keyCode))
      }
    }
  }

  def keyReleased(key:Char, keyCode:Int):Unit = key match {
    case CODED => mods.remove(keyCode);
    case _     => { 
      keyMapping(mods.toSet + keyCode) match {
        case (_, Some(f2)) => f2();
        case _ => ;
      }
    }
  }
}
