package cc.giraphe

import org.neo4j.graphdb._
import scalaj.collection.Imports._

import org.neo4j.graphdb.index.ReadableIndex
import Closeables._

trait Neo4jEnrichments {
  implicit def enrich(gdb: GraphDatabaseService) = new Neo4jOperations(gdb)
  implicit def enrich(node: Node) = new Neo4jNodeOperations(node)
}

class Neo4jOperations(gdb: GraphDatabaseService) {
  def allNodes = gdb.getAllNodes.asScala

  def autoNodeIndex = gdb.index().getNodeAutoIndexer().getAutoIndex()
  def autoEdgeIndex = gdb.index().getRelationshipAutoIndexer().getAutoIndex()
  def nodeIndex(name:String) = gdb.index().forNodes(name)
  def relationshipIndex(name:String) = gdb.index().forRelationships(name)
  

  def findOne(index: ReadableIndex[Node])(key:String, value:String) = 
    index.get(key, value) closeAfter (_.getSingle)
    
}

class Neo4jNodeOperations(node: Node) {
  def allEdges = node.getRelationships().asScala
  def apply(key:String): Any = {
    node.getProperty(key)
  }
}
