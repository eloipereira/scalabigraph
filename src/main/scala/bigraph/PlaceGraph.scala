package bigraph

import scalaz._
import Scalaz._
import scalaz.Tree._
import scalaz.std.stream.{streamInstance, streamMonoid}
import scalaz.Order._
import scalaz.Equal._

case class Site(id: Int) {
  override def toString = "$" + id
}

object Site {
  implicit def orderById: Order[Site] = orderBy((_:Site).id)
  implicit def orderingById = orderById.toScalaOrdering
  implicit def equalById: Equal[Site] = equalBy((_:Site).id)
}


 trait PlaceGraph[+A] {
   self =>
  import Site._

  def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]]

  implicit def sh: Show[Site \/ Any] = new Show[Site \/ Any] {
    override def shows(n: Site \/ Any): String = {
      n match {
        case -\/(s) => s.toString
        case \/-(a) => a.toString
      }
    }
  }

  def map[B](f: A => B):PlaceGraph[B] = {
    PlaceGraph(
      forest.map((_:Stream[Tree[Site \/ A]]).map(_.map(_.map(f))))
    )
  }

  lazy val innerFace: Int = sites.size

  lazy val outerFace: Int = forest.size

  lazy val sites: Stream[Site] = forest.flatten.map((t)=> t.flatten).flatten.filter((_).isLeft).flatMap(s=> s.swap.toOption)

  def drawPlaceGraph: String = drawRegions(forest)

  private def drawRegions(rs: Stream[Stream[Tree[Site \/ Any]]]): String =
    rs.length match{
      case 0 => ""
      case n => drawRegions(rs.init) + drawTrees(n-1,rs.last)
    }

   private def drawTrees(region: Int,ts: Stream[Tree[Site \/ Any]]): String = "\n#" + region + "[\n" + ts.map(_.drawTree).foldLeft("")(_+_) + "]\n"

  def juxtapose[U >: A](p: PlaceGraph[U]): PlaceGraph[U] = {
    val currentForest: Stream[Stream[Tree[Site \/ U]]] = self.forest
    val rs = currentForest #::: {if (self.sites.isEmpty) {p.forest} else {incrementSites(p)(self.sites.max.id).forest}}
    PlaceGraph(rs)
  }

  private def incrementSites[U >: A](p: PlaceGraph[U])(inc: Int): PlaceGraph[U] = {
    if (p.sites.max.id > inc) {p} else {
      val rs = p.forest.map((region:Stream[Tree[Site \/ U]]) =>
        region.map(tree =>
          tree.map(n =>
            n match {
              case -\/(Site(i)) => -\/(Site(i+inc+1))
              case n => n
            }
          )
        )
      )
      PlaceGraph(rs)
    }
  }

def compose[U >: A]:PartialFunction[PlaceGraph[U],PlaceGraph[U]] = {
  case p:PlaceGraph[U] if p.outerFace == self.innerFace => PlaceGraph(
    forest.map((region:Stream[Tree[Site \/ U]]) =>
      region.flatMap(t => t match {
        case Leaf(-\/(Site(i))) => p.forest(i)
        case tree => Stream(composeRegionsInSites(tree, p, sites))
      }
    )
  )
)
}

private def insertRegionInLocation[U >: A](loc: TreeLoc[Site \/ U],rg0: Stream[Tree[Site \/ U]]):TreeLoc[Site \/ U] = {
  rg0.length match {
    case 0 => loc
    case n => {
      insertRegionInLocation(loc.insertLeft(rg0.head).right.get,rg0.tail)
    }
  }
}

private def composeRegionsInSites[U >: A](tree: Tree[Site \/ U],pg: PlaceGraph[U],ss: Stream[Site]): Tree[Site \/ U] = {
  ss.size match {
    case 0 => tree
    case n => {
      tree.loc.find((_:TreeLoc[Site\/U]).getLabel == -\/(ss.head)) match {
        case None => composeRegionsInSites(tree,pg,ss.tail)
        case Some(loc) => {
          val newTree = insertRegionInLocation(loc, pg.forest(ss.head.id)).delete.get.toTree
          composeRegionsInSites(newTree,pg,ss.tail)
        }
      }
    }
  }
}

}

object PlaceGraph extends PlaceGraphFunctions {
   def apply[A](s: Stream[Stream[Tree[Site \/ A]]]): PlaceGraph[A] = new PlaceGraph[A]{
     def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = s.asInstanceOf[Stream[Stream[Tree[Site \/ U]]]]
     override def toString = "<placeGraph>"
   }
   def unapply[A](pg: PlaceGraph[A]): Option[Stream[Stream[Tree[Site \/ A]]]] = Some(pg.forest)
 }

case class Ion[A](n: A) extends PlaceGraph[A] with PlaceGraphFunctions{
   def forest[U >: A]:Stream[Stream[Tree[Site \/ U]]] = ion(n).forest
}

case class Atom[A](n:A) extends PlaceGraph[A]  with PlaceGraphFunctions{
   def forest[U >: A]:Stream[Stream[Tree[Site \/ U]]] = atom(n).forest
}

case object Empty extends PlaceGraph[Nothing]  with PlaceGraphFunctions{
   def forest[U >: Nothing]: Stream[Stream[Tree[Site \/ U]]] = empty.forest
}

case object Permute extends PlaceGraph[Nothing]  with PlaceGraphFunctions{
   def forest[U >: Nothing]: Stream[Stream[Tree[Site \/ U]]] = permute.forest
}

case object Join extends PlaceGraph[Nothing] with PlaceGraphFunctions{
   def forest[U >: Nothing]: Stream[Stream[Tree[Site \/ U]]] = join.forest
}


trait PlaceGraphFunctions{
  private def leaf0[A]: Tree[Site \/ A] = Leaf(-\/(Site(0)))
  private def leaf1[A]: Tree[Site \/ A] = Leaf(-\/(Site(1)))
  private def leaf[A](n: => A): Tree[Site\/A] = Leaf(\/-(n))
  private def nodeWithSite[A](n: => A): Tree[Site\/A] = Node(\/-(n),Stream(leaf0))

  def empty[A]: PlaceGraph[A] = new PlaceGraph[A]{
     def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = Stream(Stream.empty)
  }
  def permute[A]: PlaceGraph[A] = new PlaceGraph[A]{
     def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = Stream(Stream(leaf1[U]),Stream(leaf0[U]))
  }
  def join[A]: PlaceGraph[A] = new PlaceGraph[A]{
     def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = Stream(Stream(leaf0[U],leaf1[U]))
  }
  def atom[A](n: => A): PlaceGraph[A] = new PlaceGraph[A]{
     def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = Stream(Stream(leaf(n)))
  }
  def ion[A](n: => A): PlaceGraph[A] = new PlaceGraph[A]{
     def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = Stream(Stream(nodeWithSite(n)))
  }
}
