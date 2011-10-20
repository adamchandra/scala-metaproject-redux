package cc.giraphe

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
import Giraphe._

object TestGirapheServer {
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

}


