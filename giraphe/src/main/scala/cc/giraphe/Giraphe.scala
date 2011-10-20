package cc.giraphe

object Giraphe extends Neo4jJson
  with Neo4jBuilders 
  with Neo4jEnrichments
{
  object Type {
    type Direction                  = org.neo4j.graphdb.Direction
    type GraphDatabaseService       = org.neo4j.graphdb.GraphDatabaseService
    type Node                       = org.neo4j.graphdb.Node
    type Edge                       = org.neo4j.graphdb.Relationship
    type EdgeType                   = org.neo4j.graphdb.RelationshipType
    type ReturnableEvaluator        = org.neo4j.graphdb.ReturnableEvaluator
    type StopEvaluator              = org.neo4j.graphdb.StopEvaluator
    type Transaction                = org.neo4j.graphdb.Transaction
    type TraversalPosition          = org.neo4j.graphdb.TraversalPosition
    type Traverser                  = org.neo4j.graphdb.Traverser
    // type IndexHits                  = org.neo4j.graphdb.index.IndexHits           
  }

  import org.neo4j.graphdb._
  import org.codehaus.jettison.json.{JSONObject, JSONArray, JSONException}

  def node(seq: (String, Any)*)(implicit neo: GraphDatabaseService): Node = {
    jsonToNeo(json(seq:_*), neo, null)
  }


}
