package scalak.metascala.test

object OneOfsTest {
  import scalak.metascala.OneOfs._
  import scalak.metascala.TLists._
  import scalak.metascala.Utils._

  val ts : OneOf[Int :: Float :: TNil] = 10
//  val ts2 : OneOf[Float :: Int :: Boolean :: TNil] = ts

//  val l : Int :: TNil = removeFirst(1f, value[Int :: Float :: TNil])

//  val x = doMatch(ts) doIf ((s : Int) => 1) apply()
}
