package cc.glis

import org.specs.Specification

import cc.giraphe.{Giraphe => G}
import G._


object GraphlabInScalaSpec extends Specification("GraphLab style operations") {
  import cc.giraphe.TestGirapheServer._
  import cc.giraphe.TraversalRules._

  def createSampleGraph() {
    startAutoIndexingNodes(Seq("name"))
    neo4j.exec { implicit gdb => {
      def v(label: String) = G.node("label" -> label)

      val row1 = "a b c d".split(" ").toSeq map (v(_))
      val row2 = "e f g h".split(" ").toSeq map (v(_))

      val r1 = row1.sliding(2) map {case Seq(v1, v2) => v1 ~edge~> v2}
      val r2 = row2.sliding(2) map {case Seq(v1, v2) => v1 ~edge~> v2}

      row1 zip row2 map {case (v1, v2) => v1 ~edge~> v2}


      // row1 foldl ((_:Giraphe.Node) ~edge~> (_:Giraphe.Node))
      // {(v1, v2) => v1 ~edge~> v2}

    }}
  }

  case object edge extends Edge 

  "let's make a graph" should {
    "render a sample graph" in {
      createSampleGraph()
      val graph = """
      a -> b -> c -> d
      |  X |    |    |
      e -> f    g    h
      """
      //renderGraph() must_= graph
    }
  }

  "update functions" should {
    "have access to a vertex and its scope" in { }
    "emit continuations to its neighbors" in {}
    "write the new value to its vertex"  in {}
    
  }

  "folds over datasets" should {
    import scalaz._
    import Scalaz._

    "use iteratees" in {
      
    }

  }


  // "PageRank" should {
  //   def pageRank: Giraphe.Node => Giraphe.Node = 
  //     vertex => {
  //       val oldRank = vertex[Float]("rank")
  //       val degree = vertex.edgeCount
  //       val alpha = 1
  //       val rank = vertex.edges fold (a/degree) {
  //         (acc, edge) => {
  //           val w = edge[Float]("weight") 
  //           val r = (edge opposite vertex)[Float]("rank")  
  //           acc + (1 - alpha) * w * r
  //         }
  //       }
  // 
  //       (node("rank" -> rank), {
  //         if (math.abs(oldRank - rank) > eps) 
  //           vertex.edges map (e => (e, pageRank))
  //         else Seq()
  //       })
  //     }
  // 
  // 
  //   "run on small graph" in {
  //     val graph = createSampleGraph()
  //     
  //     
  //   }
  // }

}



