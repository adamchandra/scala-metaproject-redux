package cc.rexa2.spark.pdfminer

import cc.rexa2.spark.core.Exe
import cc.rexa2.spark.core.Util

import spark.SparkContext
import spark.SparkContext._
import java.util.Random

import scalaz._
import Scalaz._

import java.io.BufferedReader
import java.io.{File => JFile}
import java.util.{List => JList}
import java.util.{Map => JMap}


object CountPdfs extends App {
  import Util._
  import Exe._

  override def main(args: Array[String]) {
    println("running...")
    // syntax CountPdfs local[N] root/path/to/pdfs
    try {
      val host = args(0)
      val pdfroot = args(1)
      val mappers = args(2).toInt

      val spark = new SparkContext(host, "Count pdfs")

      lazy val pdfs = for {
        f <- dirlist(file(pdfroot))
        if f.isFile && f.getName.endsWith(".pdf")
      } yield f

      var count = spark.accumulator(0)

      for (i <- spark.parallelize(pdfs, mappers)) {
        count += 1
      }
      println("Pdf count = " + count.value)

    } catch {
      case e: Exception => 
        println("ERROR: " + e) // TODO: handle exception\n}
    }
  }
}

object Sha1sums extends App {
  import scala.sys.process._
  import Util._

  override def main(args: Array[String]) {
    try {
      val host = args(0)
      val pdfroot = args(1)
      val mappers = args(2).toInt

      val spark = new SparkContext(host, "sha1sums")

      lazy val pdfs = for {
        f <- dirlist(file(pdfroot))
        if f.isFile && f.getName.endsWith(".pdf")
      } yield f

      for (pdf <- spark.parallelize(pdfs, mappers)) {
        println(Process(Seq("sha1sum", pdf.getPath)) !!)
      }
    } catch {
      case e: Exception => 
        println("ERROR: " + e)
    }
  }
}

object Pysetup extends App {
  import scala.sys.process._
  import Util._

  override def main(args: Array[String]) {
    try {
      val optsAndArgs = Util.parseargs(args)
      val opts = optsAndArgs._1

      val props = scala.sys.props
      val env = scala.sys.env
      val userdir = props("user.dir")
      val newpath = Util.pathMungeBefore(opts)

      val spark = new SparkContext("local[2]", "Pysetup")
      
      for (i <- spark.parallelize(0 to 1, 2)) {
        Process( 
          command  = Seq("which", "python"),
          cwd      = file(userdir),
          extraEnv = ("PATH", newpath)) ! printLogger
      }
    } catch {
      case e: Exception => 
        println("ERROR: " + e)
    }
  }
}




