import processing.core._
import spde.core._
import PConstants._
import PApplet._
import ddf.minim._
import ddf.minim.ugens._

object Solfege {
  def main(args: Array[String]) {
    PApplet.main(Array(classOf[Solfege].getName))
  }
}

class Solfege extends ProxiedApplet {
  var minim: Minim = new Minim(this);

  lazy val px = new DrawProxy(this) {
    import Keymapping._
    
    val out = minim.getLineOut()
    def n1() = out.playNote(0.5, 2.6, Tone(587.3f, 0.9, out))

    val keymap = new Keymapping(
      List(
        (Set[AnyVal]('A'.toInt), Some(scale _), None),
        (Set[AnyVal](xCTRL, 'Q'.toInt), Some(exit _), None)),
      None)

    override def keyReleased(): Unit = keymap.keyReleased(key, keyCode)
    override def keyPressed(): Unit = keymap.keyPressed(key, keyCode)

    var player: AudioPlayer = null
    var input: AudioInput = null

    def chord(): Unit = {

    }
    def scale(): Unit = {
    }

    override def setup(): Unit = {
      size(200, 200)
      smooth();
      noLoop()
      val font = loadFont("CourierNew36.vlw");
      textAlign(CENTER);
      textFont(font, 32);
    }

    override def draw(): Unit = {}

  }

  override def stop() {
    minim.stop();
  }
}

case class Tone(frequency: Float, amplitude: Float, output: AudioOutput) extends Instrument {
  var sineOsc = new Oscil(frequency, amplitude);
  var multiplyGate = new Multiplier(0);
  sineOsc.patch(multiplyGate).patch(output);

  override def noteOn(dur: Float) = multiplyGate.setValue(1.0f);
  override def noteOff() = multiplyGate.setValue(0.0f);
}
