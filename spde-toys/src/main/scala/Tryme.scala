import processing.core._
import spde.core._
import PConstants._
import PApplet._

object Tryme {
  def main(args: Array[String]) {
    PApplet.main(Array(classOf[Tryme].getName))
  }
}

class Tryme extends ProxiedApplet {
  lazy val px = new DrawProxy(this) {
    import Keymapping._

    val keymap = new Keymapping(List(
      (Set[AnyVal](xCTRL, 'Q'), Some(exit _), None),
      (Set[AnyVal](xALT, 'M'.toInt), Some(cls _), None)),
      None)

    override def keyReleased(): Unit = keymap.keyReleased(key, keyCode)
    override def keyPressed(): Unit = keymap.keyPressed(key, keyCode)

    override def setup(): Unit = {
      size(400, 400)
      smooth();
      var fontA = loadFont("CourierNew36.vlw");
      textAlign(CENTER);
      textFont(fontA, 32);
    }

    override def draw(): Unit = {
      tryme()
    }

    def cls() = background(100)

    var x = 10
    var y = 10
    def tryme():Unit = {
      background(0xaabb99)
      ellipse(x, y, 10, 10)
      x += 3
      y += 3
    }

    def drawLetterGrid(): Unit = {
      // Set the gray value of the letters
      fill(200);

      // Set the left and top margin
      val margin = 6;
      val gap = 30;

      // Create a matrix of letterforms
      var counter = 0;
      for (i <- 0 to margin - 1) {
        for (j <- 0 to margin - 1) {
          var letter: Char = 'A'

          // Select the letter
          val count = 65 + (i * margin) + j;
          if (count <= 90) {
            letter = ('A'.toInt + counter).toChar
            if (letter == 'A' || letter == 'E' || letter == 'I' ||
              letter == 'O' || letter == 'U') {
              fill(104, 204, 255);
            } else {
              fill(0.4)
            }
          } else {
            fill(153);
            letter = (48 + counter).toChar
          }

          // Draw the letter to the screen
          text(letter, 15 + j * gap, 20 + i * gap);

          // Increment the counter
          counter += 1
          if (counter >= 26) {
            counter = 0;
          }
        }
      }
    }
  }
}
