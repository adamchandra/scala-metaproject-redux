package cc.acs.commons.util

import org.jdom._
import org.jdom.xpath._
import java.util.{List => JList}
import scala.collection.Traversable
import scalaj.collection.Imports._

object EnrichedJDom {

  def xpathNodes(xpath: String, elem: Element): Seq[Element] = {
    val q = new NodesQuery(xpath)
    q(elem)
  }

  def xpathNode(xpath: String, elem: Element): Element = {
    val q = new SingleNodeQuery(xpath)
    q(elem)
  }

  def xpathValue(xpath: String, elem: Element): String = {
    val q = new ValueQuery(xpath)
    q(elem)
  }

  def xpathInt(xpath: String, elem: Element): Int = {
    val q = new NumberValueQuery(xpath)
    q(elem)
  }

  object XMLNamespace {
    def apply(prefix: String, uri: String) = Namespace.getNamespace(prefix, uri)
    def unapply(x: Namespace) = Some((x.getPrefix, x.getURI))
  }

  object XMLElement {
    implicit def wrap(e: Element) = new XMLElement(e)
    def unapply(x: Element) = Some((x.getName, x.getNamespace))
  }

  class XMLElement(underlying: Element) {
    def attributes: Seq[Attribute] = underlying.getAttributes.asInstanceOf[JList[Attribute]].asScala
    def children: Seq[Element] = underlying.getChildren.asInstanceOf[JList[Element]].asScala
    def children(name: String): Seq[Element] = underlying.getChildren(name).asInstanceOf[JList[Element]].asScala
    def children(name: String, ns: Namespace): Seq[Element] = underlying.getChildren(name, ns).asInstanceOf[JList[Element]].asScala
  }

  //instances of these classes are not thread safe when xpath variables are used

  class XPathQuery(val expression: String, nss: Traversable[Namespace]) {
    val compiled = XPath.newInstance(expression)

    if (nss != null) 
      for (ns <- nss) 
        compiled.addNamespace(ns.getPrefix, ns.getURI)

    def setVariables(variables: (String, String)*):Unit =
      variables.foreach { x => compiled.setVariable(x._1, x._2) }
  }

  class SingleNodeQuery[NType](expression: String)(implicit nss: Traversable[Namespace] = null) 
  extends XPathQuery(expression, nss) {
    def apply(startFrom: Any, variables: (String, String)*):NType = {
      setVariables(variables:_*)
      compiled.selectSingleNode(startFrom).asInstanceOf[NType]
    }
  }

  class NodesQuery[NType](expression: String)(implicit nss: Traversable[Namespace] = null) 
  extends XPathQuery(expression, nss) {
    def apply(startFrom: Any, variables: (String, String)*):Seq[NType] = {
      setVariables(variables:_*)
      compiled.selectNodes(startFrom).asInstanceOf[JList[NType]].asScala
    }
  }

  class NumberValueQuery(expression: String)(implicit nss: Traversable[Namespace] = null) 
  extends XPathQuery(expression, nss) {
    def apply(startFrom: Any, variables: (String, String)*):Int = {
      setVariables(variables:_*)
      compiled.numberValueOf(startFrom).intValue
    }
  }

  class ValueQuery(expression: String)(implicit nss: Traversable[Namespace] = null) 
  extends XPathQuery(expression, nss) {
    def apply(startFrom: Any, variables: (String, String)*) = {
      setVariables(variables:_*)
      compiled.valueOf(startFrom)
    }
  }
}
