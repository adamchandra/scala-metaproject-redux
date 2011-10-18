package cc.rexa2.spark.core

import spark.SparkContext
import spark.SparkContext._
import java.util.Random

import scalaz._
import Scalaz._

import java.io.BufferedReader
import java.io.{File => JFile}
import java.util.{List => JList}
import java.util.{Map => JMap}

import java.util.concurrent.CountDownLatch

object TimedSparkJob extends App {
  import scala.sys.process._
  import Util._

  def parseargs(args: Seq[String]): (Map[String, Any], Seq[String])  = {
    val arglist = args.toList

    def nextOption(map: Map[String, Any], list: Seq[String]): (Map[String, Any], Seq[String]) = {
      list match {
        case (o @ "--host") :: arg :: tail   => nextOption(map ++ Map(o -> arg), tail)
        case (o @ "--pdfroot") :: arg :: tail   => nextOption(map ++ Map(o -> file(arg)), tail)
        case (o @ "--mappers") :: arg :: tail   => nextOption(map ++ Map(o -> arg.toInt), tail)
        case head :: tail   => nextOption(map, tail)
        case Nil => (map, list)
      }
    }
    nextOption(Map(), arglist)
  }

  override def main(args: Array[String]) {
    try {
      val myOpts = parseargs(args)
      val opts = myOpts._1

      val spark = new SparkContext(opts("--host").asInstanceOf[String], "PdfMiner")

      for (pdf <- spark.parallelize(0 to 10, opts("--mappers").asInstanceOf[Int])) {
        val process = Process(
          command=Seq("sleep", "10")
        ).run(printLogger)

        process.exitValue()
        process.destroy()
        // ! printLogger
      }
    } catch {
      case e: Exception => 
        println("ERROR: " + e)
    }
  }
}
