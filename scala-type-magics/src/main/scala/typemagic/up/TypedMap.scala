package scalak.up

import collection.{mutable => mut}

trait TypeMapping[A, B] 

abstract class TypeMap {
	val backed:mut.HashMap[Any, Any] = new mut.HashMap[Any, Any]()

	def update[I, O](i: I, out: O)(implicit ev: TypeMapping[I, O]) { backed.put((i,ev), out) }
	def apply[I, O](i: I)(implicit ev: TypeMapping[I, O]): O = backed((i,ev)).asInstanceOf[O]
	def getOr[I, O](i: I, or: => O)(implicit ev: TypeMapping[I, O]): O = backed.getOrElse((i,ev), or).asInstanceOf[O]

  override def toString = backed.toString
}

object TypeMap {
  def apply(m: mut.HashMap[Any, Any]): TypeMap = new TypeMap() {
	  override val backed:mut.HashMap[Any, Any] = m
  }

  def apply(): TypeMap = new TypeMap() {
  }
}
