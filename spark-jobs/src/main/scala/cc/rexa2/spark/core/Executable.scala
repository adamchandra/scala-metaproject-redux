package cc.rexa2.spark.core

import java.util.concurrent.CountDownLatch

object Exe {
  def spawnAndWait(command:String, out:String=>Unit, err:String=>Unit, onExit:Int=>Unit) {
    val latch = new CountDownLatch(1)

    def _exit(rcode:Int):Unit = {
      onExit(rcode)
      latch.countDown()
    }

    val exe = new Exe(command, out, err, _exit)
    exe.close()
    latch.await
  }
}

// class TimedExe(process: Process, timeoutMs: Int, onSuccess: Int=>Unit, onTimeout: =>Unit) {
//   import scala.sys.process._
//   import scala.io._
//   import java.io._
//   import scala.concurrent._
//   import scala.concurrent.ops.spawn
// 
//   val inputStream = new SyncVar[OutputStream];
//   spawn { onExit(process.exitValue()) }
// }

class TimedExeBack(command:Seq[String], out:String=>Unit, err:String=>Unit, onExit:Int=>Unit, timeoutMs:Int) {
  import scala.sys.process._
  import scala.io._
  import java.io._
  import scala.concurrent._
  import scala.concurrent.ops.spawn

  val inputStream = new SyncVar[OutputStream];

  val process = Process(
    command=command,
    cwd=new java.io.File("."),  
    extraEnv=("env1", "val1")).run(
      new ProcessIO(
        stdin => inputStream.put(stdin),
        stdout => Source.fromInputStream(stdout).getLines.foreach(out),
        stderr => Source.fromInputStream(stderr).getLines.foreach(err)))

  spawn { onExit(process.exitValue()) }

  def write(s:String):Unit = synchronized {
    inputStream.get.write((s + "\n").getBytes)
  }

  def close():Unit = {
    inputStream.get.close
  }
}

class Exe(command:String, out:String=>Unit, err:String=>Unit, onExit:Int=>Unit) {
  import scala.sys.process._
  import scala.io._
  import java.io._
  import scala.concurrent._
  import scala.concurrent.ops.spawn

  val inputStream = new SyncVar[OutputStream];

  val process = Process(command).run(
    new ProcessIO(
      stdin => inputStream.put(stdin),
      stdout => Source.fromInputStream(stdout).getLines.foreach(out),
      stderr => Source.fromInputStream(stderr).getLines.foreach(err)));

  spawn { onExit(process.exitValue()) }

  def write(s:String):Unit = synchronized {
    inputStream.get.write((s + "\n").getBytes)
  }

  def close():Unit = {
    inputStream.get.close
  }
}


