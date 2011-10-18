package cc.acs.commons.util

import org.junit.Assert._
import org.junit.Before
import org.scalatest.junit.JUnitSuite


class LangUtilsTest extends JUnitSuite {
  import LangUtils._

  def dictmerge:Unit = {
    val m1 = Map((1 to 10) zip (51 to 60 map {List(_)}):_*)
    val m2 = Map((1 to 3) zip (81 to 83 map {List(_)}):_*)
    
    val m12 = dictMerge(m1, m2)

    val m1str = m1.addString( new StringBuilder(), "", "\n", "\n").toString
    val m2str = m2.addString( new StringBuilder(), "", "\n", "\n").toString
    val m12str = m12.addString( new StringBuilder(), "", "\n", "\n").toString

    println(m1str)
    println(m2str)
    println(m12str)
  }
}
