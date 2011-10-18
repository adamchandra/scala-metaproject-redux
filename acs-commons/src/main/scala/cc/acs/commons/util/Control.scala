package cc.acs.commons.util


object Test {
  import scala.util.control.Breaks._

  def main(args: Array[String]) = {
    breakable {
      for (i <- 1 to 4) {
        breakable { 
          for (k <- 10 to 14) {
            println( "inner loop: i, k " + List(i, k).mkString(", "))
            if (k == 12) 
              break
          }
        }
        println( "outer loop: i: " + i )
        if (i == 8) 
          break
      }
    }
  }

}
