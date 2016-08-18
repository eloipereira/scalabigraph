package bigraph

import scalaz.{\/, _}
import Scalaz._
import scalaz.Tree._
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



trait PlaceGraph[+A] extends PlaceGraphFunctions{
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

  lazy val sites: Stream[Site] = {
    forest.flatten.flatMap(t => t.flatten).filter(_.isLeft).flatMap(s => s.swap.toOption)
  }

  def drawPlaceGraph: String = drawRegions(forest)

  private def drawRegions(rs: Stream[Stream[Tree[Site \/ Any]]]): String =
    rs.length match{
      case 0 => ""
      case n => drawRegions(rs.init) + drawTrees(n-1,rs.last)
    }

  private def drawTrees(region: Int,ts: Stream[Tree[Site \/ Any]]): String = "\n#" + region + "[\n" + ts.map(_.drawTree).foldLeft("")(_+_) + "]\n"

  def juxtapose[U >: A](p: PlaceGraph[U]): PlaceGraph[U] = {
    val currentForest: Stream[Stream[Tree[Site \/ U]]] = self.forest
    val rs = currentForest #::: {
      if (self.sites.isEmpty || p.sites.isEmpty) {
        p.forest
      } else if(self.sites.max.id >= p.sites.min.id) {
        incrementSites(p,self.sites.max.id+1).forest
      } else {
        p.forest
      }
    }
    PlaceGraph(rs)
  }

  def incrementSites[U >: A](p: PlaceGraph[U],inc: Int): PlaceGraph[U] = {
    p match {
      case x if x.sites.isEmpty => x
      case _ =>
        val rs = p.forest.map((region:Stream[Tree[Site \/ U]]) =>
          region.map(tree =>
            tree.map {
              case -\/(Site(i)) => -\/(Site(i + inc))
              case n => n
            }
          )
        )
        PlaceGraph(rs)
    }
  }



  def compose[U >: A]:PartialFunction[PlaceGraph[U],PlaceGraph[U]] = {
    case p:PlaceGraph[U] if p.outerFace == self.innerFace =>
      val pInc = incrementSites(p,self.innerFace)
      val f = self.forest.map((region: Stream[Tree[Site \/ U]]) =>
        region.flatMap {
          case Leaf(-\/(Site(i))) => pInc.forest(i)
          case tree => Stream(composeRegionsInSites(tree, pInc, self.sites))
        }
      )
      incrementSites(PlaceGraph(f),-self.innerFace)
  }

  private def insertRegionInLocation[U >: A](loc: TreeLoc[Site \/ U],rg0: Stream[Tree[Site \/ U]]):TreeLoc[Site \/ U] = {
    rg0.length match {
      case 0 => loc
      case n =>
        insertRegionInLocation(loc.insertLeft(rg0.head).right.get,rg0.tail)
    }
  }

  private def composeRegionsInSites[U >: A](tree: Tree[Site \/ U],pg: PlaceGraph[U],ss: Stream[Site]): Tree[Site \/ U] = {
    ss.size match {
      case 0 => tree
      case n =>
        tree.loc.find((_:TreeLoc[Site\/U]).getLabel == -\/(ss.head)) match {
          case None => composeRegionsInSites(tree,pg,ss.tail)
          case Some(loc) =>
            val newTree = insertRegionInLocation(loc, pg.forest(ss.head.id)).delete.get.toTree
            composeRegionsInSites(newTree,pg,ss.tail)
        }
    }
  }

  def | [U >: A](p: PlaceGraph[U]) = merge(2) compose (this || p)

  def || [U >: A](p: PlaceGraph[U]) = this juxtapose p

  def <> [U >: A](p: PlaceGraph[U]) = this compose p
}

object PlaceGraph extends PlaceGraphFunctions {
  def apply[A](s: Stream[Stream[Tree[Site \/ A]]]): PlaceGraph[A] = new PlaceGraph[A]{
    def forest[U >: A]: Stream[Stream[Tree[Site \/ U]]] = s.asInstanceOf[Stream[Stream[Tree[Site \/ U]]]]
    override def toString = "Interface: " + innerFace + " -> " + outerFace + "\n" + drawPlaceGraph
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Stream[Tree[Site \/ A]]]] = Some(pg.forest)
}

case class Ion[A](n: A) extends PlaceGraph[A] with PlaceGraphFunctions{
  def forest[U >: A]:Stream[Stream[Tree[Site \/ U]]] = ion(n).forest
}

case class Atom[A](n:A) extends PlaceGraph[A]  with PlaceGraphFunctions{
  def forest[U >: A]:Stream[Stream[Tree[Site \/ U]]] = atom(n).forest
}

case object Unit extends PlaceGraph[Nothing]  with PlaceGraphFunctions{
  def forest[U >: Nothing]: Stream[Stream[Tree[Site \/ U]]] = unit.forest
}

case object Permute extends PlaceGraph[Nothing]  with PlaceGraphFunctions{
  def forest[U >: Nothing]: Stream[Stream[Tree[Site \/ U]]] = permute.forest
}

case object Join extends PlaceGraph[Nothing] with PlaceGraphFunctions{
  def forest[U >: Nothing]: Stream[Stream[Tree[Site \/ U]]] = join.forest
}

case class PlaceId(n: Int) extends PlaceGraph[Nothing]  with PlaceGraphFunctions{
  def forest[U >: Nothing]:Stream[Stream[Tree[Site \/ U]]] = id(n).forest
}

trait PlaceGraphFunctions{
  private def leaf0[A]: Tree[Site \/ A] = Leaf(-\/(Site(0)))
  private def leaf1[A]: Tree[Site \/ A] = Leaf(-\/(Site(1)))
  private def leaf[A](n: => A): Tree[Site\/A] = Leaf(\/-(n))
  private def nodeWithSite[A](n: => A): Tree[Site\/A] = Node(\/-(n),Stream(leaf0))

  def id[A](m: Int): PlaceGraph[A] = {
    m match {
      case 0 => PlaceGraph(Stream.empty)
      case 1 => PlaceGraph(Stream(Stream(leaf0)))
      case _ => id(1) juxtapose id(m-1)
    }
  }

  def merge[A](n: Int):PlaceGraph[A] = {
    n match {
      case 0 => unit
      case _ => join compose (merge(n-1) juxtapose id(1))
    }
  }

  def unit[A]: PlaceGraph[A] = new PlaceGraph[A]{
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

  implicit def eitherEqual[A]: Equal[Site \/ A] = new Equal[Site \/ A]{
    def equal(x0: Site \/ A, x1: Site \/ A): Boolean = x0 == x1
  }


  implicit def treeEqual[A]: Equal[Tree[Site \/ A]] = new Equal[Tree[Site \/ A]]{
    def equal(s0: Tree[Site \/ A], s1: Tree[Site \/ A]): Boolean = Tree.treeEqual[Site \/ A].equal(s0,s1)
  } //TODO - this needs to be tested. probably it fails since subforest must be treated as sets of trees

  implicit def streamAsSetEqual[A]: Equal[Stream[Tree[Site \/ A]]] = new Equal[Stream[Tree[Site \/ A]]]{
    def equal(s0: Stream[Tree[Site \/ A]], s1: Stream[Tree[Site \/ A]]): Boolean = {(s0: Stream[Tree[Site \/ A]],s1: Stream[Tree[Site \/ A]]) match {
      case (Stream(), Stream()) => true
      case (Stream(a), Stream(b)) => a === b
      case (s0,s1) if s0.size != s1.size => false
      case (s0,s1) => {
        val i = s0.indexWhere(_ === s1.head)
        if (i == -1)
          false
        else if (i == (s0.size - 1))
          equal(s0.dropRight(1), s1.tail)
        else if (i == 0)
          equal(s0.drop(1), s1.tail)
        else
          equal(s0.dropRight(s0.size - i) ++ s0.drop(i+1), s1.tail)
      }
    }
    }
  }

  implicit def forestEqual[A]: Equal[Stream[Stream[Tree[Site \/ A]]]] = new Equal[Stream[Stream[Tree[Site \/ A]]]]{
    def equal(s0: Stream[Stream[Tree[Site \/ A]]], s1: Stream[Stream[Tree[Site \/ A]]]): Boolean = {
      if (s0.size != s1.size)
        false
      else if (s0.isEmpty)
        true
      else if (s0.size == 1)
        s0.head === s1.head
      else
        (s0.head === s1.head) && forestEqual.equal(s0.tail,s1.tail)
    } // TODO - still not working!
  }

  implicit def placeGraphEqual[A]: Equal[PlaceGraph[A]] = new Equal[PlaceGraph[A]]{
    override def equal(a1: PlaceGraph[A], a2: PlaceGraph[A]): Boolean = a1.forest === a2.forest
  }

  implicit def ionEqual[A]: Equal[Ion[A]] = new Equal[Ion[A]] {
    override def equal(a1: Ion[A], a2: Ion[A]): Boolean = placeGraphEqual.equal(a1,a2)
  }

  implicit def atomEqual[A]: Equal[Atom[A]] = new Equal[Atom[A]] {
    override def equal(a1: Atom[A], a2: Atom[A]): Boolean = placeGraphEqual.equal(a1,a2)
  }

  implicit def joinEqual: Equal[Join.type] = new Equal[Join.type] {
    override def equal(a1: Join.type, a2: Join.type): Boolean = placeGraphEqual.equal(a1,a2)
  }

  implicit def permuteEqual: Equal[Permute.type] = new Equal[Permute.type] {
    override def equal(a1: Permute.type, a2: Permute.type): Boolean = placeGraphEqual.equal(a1,a2)
  }

  implicit def unitEqual: Equal[Unit.type] = new Equal[Unit.type] {
    override def equal(a1: Unit.type, a2: Unit.type): Boolean = placeGraphEqual.equal(a1,a2)
  }

  implicit def idEqual: Equal[PlaceId] = new Equal[PlaceId] {
    override def equal(a1: PlaceId, a2: PlaceId): Boolean = placeGraphEqual.equal(a1,a2)
  }
}

