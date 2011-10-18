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

object Pdfminer extends App {
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
      val pyOpts = Util.parseargs(args)
      val myOpts = parseargs(args)
      val opts = pyOpts._1 ++ myOpts._1

      val props = scala.sys.props
      val userdir = props("user.dir")
      val newpath = Util.pathMungeBefore(opts)

      val spark = new SparkContext(opts("--host").asInstanceOf[String], "PdfMiner")

      lazy val pdfs = for {
        f <- dirlist(opts("--pdfroot").asInstanceOf[JFile])
        if f.isFile
        if f.getName.endsWith(".pdf") || f.getName.endsWith(".ps")
      } yield f

      for (pdf <- spark.parallelize(pdfs, opts("--mappers").asInstanceOf[Int])) {
        val process = Process(
          command=Seq("bin/rexapdfminer.py", "--file", pdf.getPath),
          cwd=file(userdir), 
          extraEnv = ("PATH", newpath)
        ).run(printLogger)

        process.destroy()
        // ! printLogger
      }
    } catch {
      case e: Exception => 
        println("ERROR: " + e) // TODO: handle exception\n}
    }
  }
}

object PdfminerScrubDirectories extends App {
  import scala.sys.process._
  import Util._
  import org.apache.commons.io.FileUtils

  def parseargs(args: Seq[String]): (Map[String, Any], Seq[String])  = {
    val arglist = args.toList

    def nextOption(map: Map[String, Any], list: Seq[String]): (Map[String, Any], Seq[String]) = {
      list match {
        case (o @ "--host") :: arg :: tail   => nextOption(map ++ Map(o -> arg), tail)
        case (o @ "--pdfroot") :: arg :: tail   => nextOption(map ++ Map(o -> file(arg)), tail)
        case (o @ "--mappers") :: arg :: tail   => nextOption(map ++ Map(o -> arg.toInt), tail)
        case Nil => (map, list)
      }
    }
    nextOption(Map(), arglist)
  }

  override def main(args: Array[String]) {
    try {
      val myOpts = parseargs(args)
      val opts = myOpts._1 

      val host = opts("--host").asInstanceOf[String]
      val pdfroot = opts("--pdfroot").asInstanceOf[JFile]
      val mappers = opts("--mappers").asInstanceOf[Int]

      val spark = new SparkContext(host, "Remove all previous PdfMiner output")

      lazy val pdfs = for {
        f <- dirlist(pdfroot)
        if f.isDirectory
        if f.getName == "pdfminer"
      } yield f

      for (pdfminer <- spark.parallelize(pdfs, mappers)) {
        println("deleting" + pdfminer)
        FileUtils.deleteDirectory(pdfminer)
      }

    } catch {
      case e: Exception => 
        println("ERROR: " + e) // TODO: handle exception\n}
    }
  }
}


