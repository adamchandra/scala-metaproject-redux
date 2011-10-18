import processing.core._
import spde.core._
import PConstants._
import PApplet._

import scala.collection.{ mutable => mut }

object Tree {
  def children[T](els: (Point, T)*): Option[(Children[T])] = {
    var ccs = new Children[T]()
    for (el <- els) el match {
      case (p, v) => ccs += p -> Tree[T](Some(v), None, None)
    }
    Some(ccs)
  }

  case class Point(x: Int, y: Int)
  case class Tree[T](var v: Option[T],
    var parent: Option[Tree[T]],
    var children: Option[Children[T]]) {
    override def toString = "Tree(" + v.map(_.toString) + ")"
    def parents(): List[Tree[T]] = parent match {
      case None => Nil
      case Some(p) => p :: p.parents
    }
  }
  class Children[T] extends mut.HashMap[Point, Tree[T]]

  def makeTree[T](v: T, children: Option[Children[T]]): Tree[T] = {
    var tree = Tree(Some(v), None, children)
    for (
      child <- children;
      (p, t) <- child
    ) {
      t.parent = Some(tree)
    }
    tree
  }

  type Crumb[T] = List[(Option[Point], Tree[T])]

  def forall[T](tree: Tree[T])(f: Crumb[T] => Unit): Unit = forall(List((None, tree)))(f)

  def forall[T](crumbs: Crumb[T])(f: Crumb[T] => Unit): Unit = {
    f(crumbs)
    val tree = crumbs.head._2
    for (
      c <- tree.children;
      (p, t) <- c
    ) forall((Some(p), t) :: crumbs.drop(1))(f)
  }
}

import Tree._

object Typo {
  def main(args: Array[String]) {
    PApplet.main(Array(classOf[Typo].getName))
  }

  case class Cursor(t: Tree[_])
  trait Direction
  case object Depth extends Direction
  case object Breadth extends Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Right extends Direction
  case object Left extends Direction
}

class Typo extends ProxiedApplet {
  import Typo._

  lazy val px = new DrawProxy(this) {
    import Keymapping._

    var tree = makeTree('x', children(
      Point(10, 10) -> 'Y',
      Point(21, 30) -> 'z',
      Point(40, 11) -> '*'))

    var cursor = Cursor(tree);

    var insertDirection: Direction = Right
    def setInsertDir(d: Direction)(): Unit = insertDirection = d

    val keymap = new Keymapping(
      List((Set[AnyVal](xCTRL, 'P'.toInt), Some(setInsertDir(Up) _), None),
        (Set[AnyVal](xCTRL, 'N'.toInt), Some(setInsertDir(Down) _), None),
        (Set[AnyVal](xCTRL, 'J'.toInt), Some(setInsertDir(Right) _), None),
        (Set[AnyVal](xCTRL, 'K'.toInt), Some(setInsertDir(Left) _), None),
        (Set[AnyVal](xCTRL, ' '), Some(cls _), None),
        (Set[AnyVal](xALT, 'M'.toInt), Some(cls _), None)),
      Some(selfInsert _))

    override def keyReleased(): Unit = keymap.keyReleased(key, keyCode)
    override def keyPressed(): Unit = keymap.keyPressed(key, keyCode)

    val canvasHW = (400, 400)

    override def setup(): Unit = {
      size(canvasHW._1, canvasHW._2)
      smooth();
      noLoop()
      var fontA = loadFont("CourierNew36.vlw");
      textAlign(CENTER);
      textFont(fontA, 32);
    }
    
    def treeExtents[T](tree: Tree[T]):(Int, Int) = {
      
      (0, 0)
    }

    override def draw(): Unit = {
      cls()
      fill(1.0)
      val extents = treeExtents(tree)
      println(extents)
      forall(tree) { crumbs =>
        crumbs match {
          case (Some(pt), t: Tree[_]) :: etal =>
            text(t.v.toString, pt.x, pt.y)
          case (None, t: Tree[_]) :: etal => 
        }
      }
    }

    def moveCursor(): Unit = insertDirection match {
      case Breadth =>
      case Depth =>
      case Up =>
      case Down =>
      case Right =>
      case Left =>
    }

    def cls() = background(100)

    def selfInsert(k: Char, code: Int) {
      // grid(cursor.x, cursor.y) = k
      draw()
      moveCursor()
    }
  }
}
