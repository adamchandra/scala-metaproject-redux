package cc.rexa2.graphdb

import org.specs.Specification

import org.neo4j.graphdb.Direction
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Relationship
import org.neo4j.graphdb.RelationshipType
import org.neo4j.graphdb.ReturnableEvaluator
import org.neo4j.graphdb.StopEvaluator
import org.neo4j.graphdb.Transaction
import org.neo4j.graphdb.TraversalPosition
import org.neo4j.graphdb.Traverser
import org.neo4j.graphdb.Traverser.Order
import org.neo4j.graphdb.index.IndexHits

import org.neo4j.kernel.EmbeddedGraphDatabase

import java.io.StringReader
import java.io.Reader

import scalaj.collection.Imports._
import Neo4jOperations._
import Neo4jBuilders._
import Neo4jJson._

object Dev {
  def streamString: String => Reader =
    str => new StringReader(str)

  val neo4j: Neo4jServer = new Neo4jServer(streamString(
    """
    |org.neo4j.server.database.location=data/graph.db
    |# org.neo4j.server.webserver.port=7474
    |# org.neo4j.server.webadmin.rrdb.location=data/graph.db/../rrd
    |# REST endpoint for the data API
    |# Note the / in the end is mandatory
    |org.neo4j.server.webadmin.data.uri=/db/data/
    |# REST endpoint of the administration API (used by Webadmin)
    |org.neo4j.server.webadmin.management.uri=/db/manage/
    |# Low-level graph engine tuning file
    |# org.neo4j.server.db.tuning.properties=conf/neo4j.properties
    |
    |# DANGER! Only for testing environments:
    |neo4j.destroy_on_startup=true
    |neo4j.skip.shutdown.hook=true
    |
    |# neo4j.allow_store_upgrade
    |# neo4j.array_block_size
    |# neo4j.neostore.propertystore.db.arrays.mapped_memory
    |# neo4j.backup_slave
    |# neo4j.cache_type
    |# neo4j.nioneodb
    |# neo4j.dump_configuration
    |# neo4j.enable_online_backup
    |# neo4j.enable_remote_shell
    |# neo4j.keep_logical_logs
    |# neo4j.logical_log
    |# neo4j.neo_store
    |
    |neo4j.node_auto_indexing=true
    |neo4j.node_keys_indexable=name
    |neo4j.relationship_auto_indexing=false
    |# neo4j.relationship_keys_indexable
    |
    |# neo4j.neostore.nodestore.db.mapped_memory
    |# neo4j.neostore.propertystore.db.index.keys.mapped_memory
    |# neo4j.neostore.propertystore.db.index.mapped_memory
    |# neo4j.neostore.propertystore.db.mapped_memory
    |# neo4j.read_only
    |# neo4j.rebuild_idgenerators_fast
    |# neo4j.neostore.relationshipstore.db.mapped_memory
    |# neo4j.store_dir
    |# neo4j.string_block_size
    |# neo4j.neostore.propertystore.db.strings.mapped_memory
    |# neo4j.tx_manager_impl
    |# neo4j.use_memory_mapped_buffers
    |
    |# neo4j.allow_store_upgrade
    |# neo4j.array_block_size
    |# neo4j.neostore.propertystore.db.arrays.mapped_memory
    |# neo4j.backup_slave
    |# neo4j.cache_type
    |# neo4j.nioneodb
    |# neo4j.dump_configuration
    |# neo4j.enable_online_backup
    |# neo4j.enable_remote_shell
    |# neo4j.keep_logical_logs
    |# neo4j.logical_log
    |# neo4j.neo_store
    |# neo4j.node_auto_indexing
    |# neo4j.node_keys_indexable
    |# neo4j.neostore.nodestore.db.mapped_memory
    |# neo4j.neostore.propertystore.db.index.keys.mapped_memory
    |# neo4j.neostore.propertystore.db.index.mapped_memory
    |# neo4j.neostore.propertystore.db.mapped_memory
    |# neo4j.read_only
    |# neo4j.rebuild_idgenerators_fast
    |# neo4j.relationship_auto_indexing
    |# neo4j.relationship_keys_indexable
    |# neo4j.neostore.relationshipstore.db.mapped_memory
    |# neo4j.store_dir
    |# neo4j.string_block_size
    |# neo4j.neostore.propertystore.db.strings.mapped_memory
    |# neo4j.tx_manager_impl
    |# neo4j.use_memory_mapped_buffers
    """.stripMargin))

  def resetGDB {
    neo4j.exec { implicit gdb =>
      gdb.allNodes map { n =>
        n.allEdges map { e =>
          e.delete
        }
        n.delete
      }
    }
  }

  trait Edge extends RelationshipType {
    def name = this.getClass.getName // TODO use lift's camel case to hyphen conversion for naming
  }

  def startAutoIndexingNodes(keys: Seq[String]) = {
    neo4j.exec { implicit gdb =>
      {
        val indexer = gdb.index().getNodeAutoIndexer()
        indexer.setEnabled(true)
        keys foreach (indexer.startAutoIndexingProperty(_))
      }
    }
  }

  def createSampleGraph() {
    startAutoIndexingNodes(Seq("name"))

    neo4j.exec { implicit gdb =>
      {
        def thing(category: String, name: String) = node("category" -> category, "name" -> name)
        def person(name: String) = node("category" -> "person", "name" -> name)

        val (alice, bob, chris) = (person("alice"), person("bob"), person("chris"))

        val (dog, cat, horse) = (thing("dog", "spot"), thing("cat", "fluffy"), thing("horse", "trigger"))

        alice ~ hasA ~> dog
        alice ~ hasA ~> cat
        (horse <~ hasA <~ bob) ~ hasA ~> cat
        chris ~ hasA ~> horse
        alice ~ knows ~> bob ~ knows ~> chris
      }
    }
  }

  case object hasA extends Edge // TODO use lift's camel case to hyphen conversion for naming
  case object knows extends Edge

}

object GraphDBSpec extends Specification("Graph Primitives") {
  import Dev._
  import TraversalRules._

  def findByName(neo4j: Neo4jServer, s: String): Node = {
    val who = neo4j.gdb.findOne(neo4j.gdb.autoNodeIndex)("name", s)
    who must not be null
    who
  }

  doAfterSpec { resetGDB }

  "neo4j database operations" should {
    // doFirst { resetGDB }
    // doAfter { resetGDB }    
    // doBefore { resetGDB }
    // doLast { resetGDB }

    doBefore { resetGDB }

    "use class-based relationships" in {
      case object Arc extends RelationshipType { lazy val name = "arc" }

      neo4j.exec { gdb =>
        {
          val firstNode: Node = gdb.createNode
          firstNode.setProperty("name", "Hello")
          val secondNode: Node = gdb.createNode
          secondNode.setProperty("name", "World")

          firstNode ~ Arc ~> secondNode

          val greeting = firstNode.getProperty("name") + " " + secondNode.getProperty("name");
          greeting must_== "Hello World"
          firstNode.getSingleRelationship(Arc, Direction.OUTGOING).delete();
          firstNode.delete();
          secondNode.delete();
        }
      }
    }

    "implicitly convert json objects to nodes" in {
      neo4j.exec { implicit gdb =>
        {
          val n1: Node = json("name" -> "Hello")
          val n2: Node = json("name" -> "World")

          n1 ~> "rel" ~> n2

          val greeting = n1.getProperty("name") + " " + n2.getProperty("name");
          greeting must_== "Hello World"

          n1.getSingleRelationship("rel", Direction.OUTGOING).delete();
          n1.delete();
          n2.delete();
        }
      }
    }

    "express both in/out relationships" in {
      neo4j.exec { implicit gdb =>
        {
          val n1: Node = json("name" -> "Hello")
          val n2: Node = json("name" -> "World")
          val n3: Node = json("name" -> "Cruel")

          n1 ~ "rel" ~> n2 <~ "rel" <~ n3
          n1 ~> "rel" ~> n2
          n2 <~ "rel" <~ n3
        }
      }
    }

    "construct edges w/attributes" in {
      neo4j.exec { implicit gdb =>
        {
          node("name" -> "Hello") ~ "nodeType" ~>
            node("name" -> "World")
        }
      }
    }

    "index and find nodes" in {
      startAutoIndexingNodes(Seq("name"))

      neo4j.exec { implicit gdb =>
        {
          node("name" -> "cain") ~ hasA ~> node("name" -> "abel")
        }
      }

      findByName(neo4j, "cain")
    }

    "traverse something more complex" in {
      createSampleGraph()

      val alice = findByName(neo4j, "alice")

      println("alice = " + alice("name"))

      TraversalRules(
        order = Traverser.Order.BREADTH_FIRST,
        stop = { pos => true },
        returnable = { pos => true },
        follow = Seq(hasA~>, knows<~>, knows<~)).follow(alice).foreach {
          case (tinfo, cnode) => {
            val depth = tinfo.currentPosition().depth()
            System.out.println("At depth " + depth + " => " + cnode("name"))
          }
        }
    }

    "traverse using a finer-granularity specification" in {
      import org.neo4j.kernel.Traversal

      createSampleGraph()
      val alice = findByName(neo4j, "alice")

      val desc = Traversal.description()

      // node expanders, e.g., 
      // follow = Seq(hasA~>, knows<~>, knows<~)).follow(alice).foreach {
      Traversal.emptyExpander()
      Traversal.expanderForAllTypes()

      // a.mapValues(Seq(_)) |+| b.mapValues(Seq(_))
      // res: Map[Int,Seq[java.lang.String]] = Map(1 -> List(one, un), 2 -> List(two, deux), 3 -> List(three, trois))

    }

    "spit out good error messages on exception" in {
      // Failed to commit, transaction rolledback ---> org.neo4j.kernel.impl.nioneo.store.InvalidRecordException: Node record NodeRecord[2,false,1,1] still has relationships
      // Still has relationships:
      // val n1:Node = json(Seq("name" -> "Hello"))
      // val n2:Node = json(Seq("name" -> "World"))
      // n1 --|Arc|-> n2
      // n1 --|"anotherArc"|-> n2
      // n1.delete();
      // n2.delete();
    }
  }

  "graph loading primitives" should {

    "create textual mention nodes" in {
      val nodes = "a b c d e".split(" ") map (c =>
        neo4j.exec(implicit gdb => node("name" -> c)))
      // println(nodes.mkString("[", ", ", "]"))
    }

    "search over..." in {

    }
  }

}

