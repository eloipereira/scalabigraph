package bigraph

import scalaz._
import scalaz.Tree._
import scalaz.std.stream.{streamInstance, streamMonoid}


case class Site(id: Int) {
  override def toString = "$" + id
}

sealed trait PlaceGraph[A]{
  def forest: Stream[Stream[Tree[Site \/ A]]]
  implicit def sh: Show[Site \/ A] = new Show[Site \/ A] {
    override def shows(n: Site \/ A): String = {
      n match {
        case -\/(s) => s.toString
        case \/-(a) => a.toString
      }
    }
  }

  def map[B](f: A => B):PlaceGraph[B] = {
    val fs = forest.map ((region: Stream[Tree[Site \/ A]]) =>
    region.map((tree: Tree[Site \/ A]) =>
    tree.map((n: Site \/ A) =>
    n.map(f)
  )
)
)
PlaceGraph(fs)
}

def drawPlaceGraph: String = drawRegions(forest)
private def drawRegions(rs: Stream[Stream[Tree[Site\/A]]]): String = rs.length match{
  case 0 => ""
  case n => drawRegions(rs.init) + drawTrees(n-1)(rs.last)
}
private def drawTrees(region: Int)(ts: Stream[Tree[Site\/A]]): String = "\n#" + region + "[\n" + ts.map(_.drawTree).foldLeft("")(_+_) + "]\n"
}

object PlaceGraph extends PlaceGraphFunctions{
  def apply[A](s: Stream[Stream[Tree[Site \/ A]]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val forest = s
    override def toString = "<placeGraph>"
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Stream[Tree[Site \/ A]]]] = Some(pg.forest)
}

trait PlaceGraphFunctions {
  implicit val s: Site = Site(0)
  def empty[A]: PlaceGraph[A] = PlaceGraph(Stream(Stream.empty))
  def permutation[A]: PlaceGraph[A] = PlaceGraph(Stream(Stream(Leaf(-\/(Site(1)))),Stream(Leaf(-\/(Site(0))))))
  def join[A]: PlaceGraph[A] = PlaceGraph(Stream(Stream(Leaf(-\/(Site(0))),Leaf(-\/(Site(1))))))
  def atom[A](n: => A): PlaceGraph[A] = PlaceGraph(Stream(Stream(Node(\/-(n), Stream.empty))))
  def ion[A](n: => A)(implicit s: Site): PlaceGraph[A] = PlaceGraph(Stream(Stream(Node(\/-(n),Stream(Leaf(-\/(s)))))))
  def juxtaposition[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = PlaceGraph(p0.forest #::: p1.forest)

  private def findASite[A](loc: TreeLoc[Site \/ A]):Option[TreeLoc[Site \/ A]] = {
    loc.find { (_:TreeLoc[Site \/ A]).getLabel match {
      case -\/(Site(i)) => true
      case _ => false
    }
  }
}

private def insertRegionInLocation[A](loc: TreeLoc[Site \/ A],rg0: Stream[Tree[Site \/ A]]):TreeLoc[Site \/ A] = {
  rg0.length match {
    case 0 => loc
    case n => {
      insertRegionInLocation(loc.insertLeft(rg0.head).right.get,rg0.tail)
    }
  }
}

private def composeRegionsInTree[A](tree: Tree[Site \/ A],rs: Stream[Stream[Tree[Site \/ A]]]): Tree[Site \/ A] = {
  findASite(tree.loc) match {
    case None => tree
    case Some(p) => {
      p.getLabel match {
        case -\/(Site(i)) => {
          if (rs.length > i){
            val newTree = insertRegionInLocation(p, rs(i)).delete.get.toTree
            composeRegionsInTree(newTree,rs)
          } else {
            tree
          }
        }
        case _ => tree
      }
    }
  }
}

def composition[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = {
      val fs = p0.forest.map(region =>
        region.map(t =>
          composeRegionsInTree(t, p1.forest)
        )
      )
      PlaceGraph(fs)
}

}
