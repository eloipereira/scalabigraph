package bigraph

package object bigraph{
  import scalaz._
  type Region[A] = Stream[Tree[Site \/ A]]
}

import scalaz._
import Scalaz._
import scalaz.Tree._
import scalaz.std.stream.{streamInstance, streamMonoid}
import scalaz.Order._
import scalaz.Equal._

case class Site(id: Int)(implicit val open: Boolean = true) {
  override def toString = "$" + id
}

object Site {
  implicit def orderById: Order[Site] = orderBy((_:Site).id)
  implicit def orderingById = orderById.toScalaOrdering
  implicit def equalById: Equal[Site] = equalBy((_:Site).id)
}

object Site_ {
  def unapply(s: Site) = Some((s.id,s.open))
}


sealed trait PlaceGraph[A] {
  import bigraph._
  import Site._

  def forest: Stream[Region[A]]
  implicit def sh: Show[Site \/ A] = new Show[Site \/ A] {
    override def shows(n: Site \/ A): String = {
      n match {
        case -\/(s) => s.toString
        case \/-(a) => a.toString
      }
    }
  }

  def map[B](f: A => B):PlaceGraph[B] = {
    PlaceGraph(
      forest.map((region: Region[A]) =>
        region.map((tree: Tree[Site \/ A]) =>
          tree.map((n: Site \/ A) =>
            n.map(f)
          )
        )
      )
    )
  }

  lazy val innerFace: Int = sites.size

  lazy val outerFace: Int = forest.size

  lazy val sites: Stream[Site] = forest.flatten.map((t:Tree[Site \/ A])=> t.flatten).flatten.filter((_:Site \/ A).isLeft).flatMap(s=> s.swap.toOption)

  def drawPlaceGraph: String = drawRegions(forest)
  private def drawRegions(rs: Stream[Region[A]]): String = rs.length match{
    case 0 => ""
    case n => drawRegions(rs.init) + drawTrees(n-1)(rs.last)
  }
  private def drawTrees(region: Int)(ts: Region[A]): String = "\n#" + region + "[\n" + ts.map(_.drawTree).foldLeft("")(_+_) + "]\n"

  def juxtapose(p: PlaceGraph[A]): PlaceGraph[A] = {
    val rs = forest #::: {if (sites.isEmpty) {p.forest} else {incrementSites(p)(sites.max.id).forest}}
    PlaceGraph(rs)
  }

  private def incrementSites(p: PlaceGraph[A])(inc: Int): PlaceGraph[A] = {
    if (p.sites.max.id > inc) {p} else {
      val rs = p.forest.map(region =>
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

  def compose(p: PlaceGraph[A]): Option[PlaceGraph[A]] = {
    if (innerFace != p.outerFace) {
      None
    } else {
      PlaceGraph(
        forest.map(region =>
          region.map(t =>
            composeRegionsInTree(t, p.closeAllSites)
          )
        )
      ).openAllSites.some
    }
  }

  private def closeAllSites: PlaceGraph[A] = {
    PlaceGraph(
      forest.map(region =>
        region.map(tree =>
          tree.map(n =>
            n match {
              case -\/(Site_(i,true)) => -\/(Site(i)(false))
              case n => n
            }
          )
        )
      )
    )
  }

  private def openAllSites: PlaceGraph[A] = {
    PlaceGraph(
      forest.map(region =>
        region.map(tree =>
          tree.map(n =>
            n match {
              case -\/(Site_(i,false)) => -\/(Site(i)(true))
              case n => n
            }
          )
        )
      )
    )
  }

 def findASite(loc: TreeLoc[Site \/ A]):Option[TreeLoc[Site \/ A]] = {
    loc.find { (_:TreeLoc[Site \/ A]).getLabel match {
      case -\/(Site(i)) => true
      case _ => false
    }
  }
  }

  private def insertRegionInLocation(loc: TreeLoc[Site \/ A],rg0: Region[A]):TreeLoc[Site \/ A] = {
    rg0.length match {
      case 0 => loc
      case n => {
        insertRegionInLocation(loc.insertLeft(rg0.head).right.get,rg0.tail)
      }
    }
  }

  private def composeRegionsInTree(tree: Tree[Site \/ A],pg: PlaceGraph[A]): Tree[Site \/ A] = {
    findASite(tree.loc) match {
      case None => tree
      case Some(p) => {
        p.getLabel match {
          case -\/(Site_(i,true)) => {
            if (pg.outerFace > i){
              val newTree = insertRegionInLocation(p, pg.forest(i)).delete.get.toTree
              composeRegionsInTree(newTree,pg)
            } else {
              tree
            }
          }
          case _ => tree
        }
      }
    }
  }
}

object PlaceGraph extends PlaceGraphInstances{
  import bigraph._
  def apply[A](s: Stream[Region[A]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val forest = s
    override def toString = "<placeGraph>"
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Region[A]]] = Some(pg.forest)
}

trait PlaceGraphInstances {
  implicit val s: Site = Site(0)
  def empty[A]: PlaceGraph[A] = PlaceGraph(Stream(Stream.empty))
  def permutation[A]: PlaceGraph[A] = PlaceGraph(Stream(Stream(Leaf(-\/(Site(1)))),Stream(Leaf(-\/(Site(0))))))
  def join[A]: PlaceGraph[A] = PlaceGraph(Stream(Stream(Leaf(-\/(Site(0))),Leaf(-\/(Site(1))))))
  def atom[A](n: => A): PlaceGraph[A] = PlaceGraph(Stream(Stream(Node(\/-(n), Stream.empty))))
  def ion[A](n: => A)(implicit s: Site): PlaceGraph[A] = PlaceGraph(Stream(Stream(Node(\/-(n),Stream(Leaf(-\/(s)))))))
}
