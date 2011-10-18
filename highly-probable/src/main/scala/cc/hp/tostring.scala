package cc.hp

object tostring {
  trait NamedElementsProduct extends Product {
    def namedElements: Iterator[(String, Any)]
  }

  trait HaskellLikeToString extends NamedElementsProduct {
    override def toString: String = namedElements.map((p) => p._1 + "=" + p._2).mkString(productPrefix + "(", ",", ")")
  }

  case class A(a: Int, b: Int) extends NamedElementsProduct with HaskellLikeToString {
    def namedElements = List(("a", a), ("b", b)).iterator
  }
}
