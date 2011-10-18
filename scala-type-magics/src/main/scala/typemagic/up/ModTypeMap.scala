package scalak.up

import collection.{mutable => mut}

abstract class ModTypeMap(immutables: TypeMap)  {
	var mutables:mut.HashMap[Any, Any] = new mut.HashMap[Any, Any]()

	def update[I, O](i: I, out: O)(implicit ev: TypeMapping[I, O]) { 
    mutables.put((i,ev), out)
  }
	def apply[I, O](i: I)(implicit ev: TypeMapping[I, O]): O = {
    (mutables orElse immutables.backed) ((i,ev)).asInstanceOf[O]
  }

  def isModified:Boolean = ! mutables.isEmpty

  override def toString = mutables.toString + immutables.backed.toString 
}
