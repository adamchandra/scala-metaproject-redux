package cc.giraphe

object Closeables {
  type Closeable = {def close(): Unit}

  class CloseAfterTuple[A<:Product](val x: A) {
    def closeAfter[B](block: A=>B): B = {
      try {
        block(x);
      } finally {
        for (i <- 0 until x.productArity) {
          x.productElement(i) match { 
            case c:Closeable => println("closing " + c); c.close()
            case _ => 
          }
        }
      }
    }
  }

  class CloseAfterSingle[A<:Closeable](val x: A) {
    def closeAfter[B](block: A=>B): B = {
      try {
        block(x);
      } finally {
        x.close()
      }
    }
  }

  implicit def any2CloseAfter[A<:Product](x: => A): CloseAfterTuple[A] = 
    new CloseAfterTuple(x)

  implicit def wrapCloseable[A<:Closeable](x: => A): CloseAfterSingle[A] = 
    new CloseAfterSingle(x)


  def example() {
    import java.io._

    (new BufferedReader(new FileReader("in.txt")), 
     new PrintWriter("out.txt"),
     new PrintWriter("sample.txt")) closeAfter {case (in, out, other) => {
       out.println(in.readLine)
       other.println("hello world!")
     }}
  }
}

