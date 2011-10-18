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
import scala.sys.process.ProcessLogger


object Util {

  def parseargs(args: Seq[String]): (Map[String, Any], Seq[String])  = {
    val arglist = args.toList

    def nextOption(map: Map[String, Any], list: Seq[String]): (Map[String, Any], Seq[String]) = {
      list match {
        case (o @ "--python-home") :: dir :: tail   => nextOption(map ++ Map(o -> dir), tail)
        case head :: tail   => nextOption(map, tail)
        case Nil => (map, list)
      }
    }
    nextOption(Map(), arglist)
  }

  def pathMungeBefore(opts:Map[String, Any]):String = {
    val env = scala.sys.env
    cpjoin(
      pathjoin(file(opts("--python-home").asInstanceOf[String]), 
               file("bin")).getPath, 
      env("PATH")
    )
  }

  def pathjoin(f1:JFile, f2:JFile):JFile = 
    new JFile(f1, f2.getPath)

  def cpjoin(f1:String, f2:String):String = 
    f1 + scala.sys.props("path.separator") + f2


  def file(s: String) = new JFile(s)
  def stream(f:JFile) = new java.io.FileInputStream(f)
  implicit def FileShow: Show[JFile] = showBy ("<file:"+_.toString+">")

  def dirtree(root: JFile): Tree[JFile] = {
    if(root.isDirectory()) 
      node(root, root.listFiles.toStream map (dirtree(_)))
    else 
      leaf(root)
  }

  def dirlist(root: JFile): Seq[JFile] = {
    if(root.isDirectory()) 
      Seq(root) ++ (root.listFiles flatMap (dirlist(_)))
    else 
      Seq(root)
  }

  def printLogger = ProcessLogger(
    out => println("out: " + out), 
    err => println("out: " + err))
}
