package scalak.metascala.test

object TListsTest {
  import scalak.metascala.Utils._
  import scalak.metascala.TLists._
  import scalak.metascala.Nats._

  type T1 = Int :: Boolean :: TNil
  type T2 = Float :: (Int, Double) :: TNil
  type T3 = T1 ::: T2
  type T4 = Equal[T3, Int :: Boolean :: Float :: (Int, Double) :: TNil]
  type T5 = Equal[T3#RemoveNth[_2], Int :: Boolean :: (Int, Double) :: TNil]
  type T6 = Equal[T3#RemoveNth[_1], Int :: Float :: (Int, Double) :: TNil]
  type t7 = Equal[T3#Reverse, (Int, Double) :: Float :: Boolean :: Int :: TNil]
}
