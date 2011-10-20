package cc.giraphe

import org.neo4j.graphdb._
import scalaj.collection.Imports._


object TraversalRules {
  implicit def rel2relBuilder(rel: RelationshipType) = new DirectionBuilderMethods(rel)

  class DirectionBuilderMethods(rel: RelationshipType) {
    def ~> = (rel, Direction.OUTGOING)
    def <~ = (rel, Direction.INCOMING)
    def <~> = (rel, Direction.BOTH)
  }
}

case class TraversalRules(
  order: Traverser.Order,
  stop: (TraversalPosition) => Boolean,
  returnable: (TraversalPosition) => Boolean,
  follow: Seq[(RelationshipType, Direction)]) 
{

  def repeat[T](t: T):Stream[T] = {
    import scalaz._
    import Scalaz._
    t.repeat[Stream]
  }

  def follow(start: Node):Seq[(Traverser, Node)] = {
    val t = traverse(start)
    (repeat(t) zip t.iterator.asScala.toSeq)
  }

  private def traverse(start: Node):Traverser = {
    start.traverse(
      order,
      new StopEvaluator() {
        def isStopNode(position: TraversalPosition): Boolean =
          stop(position)
      },
      new ReturnableEvaluator() {
        def isReturnableNode(position: TraversalPosition): Boolean =
          returnable(position)
      },
      (follow flatMap { case (r, d) => Seq(r, d) }):_*)
  }
}
