package cc.rexa2.spark.pdfminer

import akka.actor.{Actor, PoisonPill}
import Actor._
import akka.routing.{Routing, CyclicIterator}
import Routing._

import collection.mutable.{HashMap, Map}
import java.util.concurrent.CountDownLatch

object MapReduce extends App {

  countWordsInFile("src/main/resources/test.txt", 6);

  def countWordsInFile(fileName: String, nrOfWorkers: Int) {

    val source = scala.io.Source.fromFile(fileName)
    val document = source.getLines()

    val latch = new CountDownLatch(1)

    val master = actorOf(new Master(nrOfWorkers, latch)).start()

    master ! CountDocument(document)

    latch.await()

    source.close()
  }
  // Messages
  sealed trait MapReduceMessage
  case class CountDocument(document: Iterator[String]) extends MapReduceMessage
  case class CountLine(line: String) extends MapReduceMessage
  case class Result(values: Map[String, Int]) extends MapReduceMessage

  //Actor that counts the words for a single line
  class CountLineWorker extends Actor {

    def receive = {
       case CountLine(line) =>
         self reply Result(countWords(line))
    }

    def countWords(line: String):Map[String, Int] = {
      val result = new HashMap[String, Int]

      "[^A-Za-z0-9\u0020]".r.replaceAllIn(line, "")
            .split(" ")
            .foreach(word => {
              result.put(word, result.getOrElse(word, 0)+1)
            })

      result
    }
  }

  // Master Actor, creates Worker Actors, distributes work and gathers results
  class Master(nrOfWorkers: Int, latch:CountDownLatch) extends Actor {

    val workers = Vector.fill(nrOfWorkers)(actorOf[CountLineWorker].start());
    val router = Routing.loadBalancerActor(CyclicIterator(workers)).start();

    val resultMap = new HashMap[String, Int]();

    var start : Long = _
    var count : Long = 0

    def receive = {
      case CountDocument(lines : Iterator[String]) =>
        
        lines.foreach(line =>
              if (!line.isEmpty) {
                count = count+1;
                router ! CountLine(line)
              })

        //shutdown actors
        router ! Broadcast(PoisonPill)
        router ! PoisonPill

      case Result(values: Map[String, Int]) =>
        
        for ((key, value) <- values) {
          resultMap.put(key, resultMap.getOrElse(key, 0)+value)
        }
        count = count - 1;
        if (count <= 0) self.stop()
    }

    override def preStart() {
      start = System.currentTimeMillis
    }

    override def postStop() {
      val end = System.currentTimeMillis-start
      println("Result after %s ms :".format(end))
      for((key, value) <- resultMap.toList.sortBy(_._2).reverse) {
        println("%s: %s".format(value, key))
      }
      latch.countDown()
    }
  }
}
