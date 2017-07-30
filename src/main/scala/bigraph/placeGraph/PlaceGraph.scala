package bigraph.placeGraph

import scalaz.Tree.{Leaf, Node}
import scalaz.{-\/, Equal, Show, Tree, TreeLoc, \/, \/-}
import scalaz._
import Scalaz._

/**
  * Created by eloipereira on 8/22/16.
  */
trait PlaceGraph[+A] extends PlaceGraphInstances with PlaceGraphTypeAliases{
  self =>

  def forest[U >: A]: Stream[Region[U]]

  def map[B](f: A => B):PlaceGraph[B] = {
    PlaceGraph(
      forest.map((_:Region[A]).map(_.map(_.map(f))))
    )
  }

  lazy val placeInnerFace: Int = sites.size

  lazy val placeOuterFace: Int = forest.size

  lazy val sites: Stream[Site] = forest.flatten.flatMap(t => t.flatten).filter(_.isLeft).flatMap(s => s.swap.toOption)

  def support[U >: A]: Set[U] = forest.flatten.flatMap(t => t.toStream).flatMap(o => o.toOption).toSet

  def juxtapose[U >: A]:PartialFunction[PlaceGraph[U],PlaceGraph[U]] = {
    case p:PlaceGraph[U] if self.support.intersect(p.support) == Set.empty =>
      val currentForest: Stream[Region[U]] = self.forest
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

  private def incrementSites[U >: A](p: PlaceGraph[U],inc: Int): PlaceGraph[U] = {
    p match {
      case x if x.sites.isEmpty => x
      case _ =>
        val rs = p.forest.map((region:Region[U]) =>
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



  def compose[U >: A]:PartialFunction[PlaceGraph[U],PlaceGraph[U]] = { //TODO - not sure about this... compose must be cleaned
    case p:PlaceGraph[U] if (self.placeInnerFace == p.placeOuterFace) && (self.support.intersect(p.support) == Set.empty) =>
      val pInc = incrementSites(p,self.placeInnerFace)
      val f = self.forest.map((region: Region[U]) =>
        region.flatMap( (t: Tree[Site \/ U]) =>
          t match {
           case Leaf(-\/(Site(i))) => pInc.forest(i)
           case Leaf(\/-(a)) => Stream(t)
           case _ => Stream(composeRegionsInSites(t, pInc, self.sites))
          }
        )
      )
      incrementSites(PlaceGraph(f),-self.placeInnerFace)
  }

  private def insertRegionInLocation[U >: A](loc: TreeLoc[Site \/ U],rg0: Region[U]):TreeLoc[Site \/ U] = {
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

  protected def leafI[A](i: Int): Tree[Site \/ A] = Leaf(-\/(Site(i)))
  protected def leaf[A](n: => A): Tree[Site\/A] = Leaf(\/-(n))


  protected def sitePerm(m: Int, n: Int): (Int) => Int = (j: Int) => {
    if(j <= n-1)
      m+j
    else
      j-n
  }

  def | [U >: A](p: PlaceGraph[U]) = Merge(2) compose (self || p)

  def || [U >: A](p: PlaceGraph[U]) = self juxtapose p

  def <> [U >: A](p: PlaceGraph[U]) = self compose p

}

object PlaceGraph extends PlaceGraphInstances with PlaceGraphTypeAliases{
  def apply[A](s: Stream[Region[A]]): PlaceGraph[A] = new PlaceGraph[A]{
    def forest[U >: A]: Stream[Region[U]] = s.asInstanceOf[Stream[Region[U]]]
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Region[A]]] = Some(pg.forest)
}

case class PlaceIon[A](n: A) extends PlaceGraph[A] with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: A]:Stream[Region[U]] = Stream(Stream(Node(\/-(n),Stream(leafI(0)))))
}

case class Atom[A](n:A) extends PlaceGraph[A]  with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: A]:Stream[Region[U]] = Stream(Stream(leaf(n)))
}

case object PlaceUnit extends PlaceGraph[Nothing]  with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: Nothing]: Stream[Region[U]] = Stream(Stream.empty)
}

case class Permute(m: Int, n:Int) extends PlaceGraph[Nothing]  with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: Nothing]: Stream[Region[U]] = {
    (0 until(m+n)).foldLeft[Stream[Region[U]]](
      Stream[Region[U]]()
    )(
      (ss: Stream[Region[U]], i: Int) => ss ++ Stream[Region[U]](Stream(leafI(sitePerm(m,n)(i))))
    )
  }
}

case object Join extends PlaceGraph[Nothing] with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: Nothing]: Stream[Region[U]] = Stream(Stream(leafI[U](0),leafI[U](1)))
}

case class Merge(n: Int) extends PlaceGraph[Nothing] with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: Nothing]: Stream[Region[U]] = {
    n match {
      case 0 => PlaceUnit.forest
      case _ => (Join compose (Merge(n-1) juxtapose PlaceId(1))).forest
    }
  }
}

case class PlaceId(n: Int) extends PlaceGraph[Nothing]  with PlaceGraphInstances with PlaceGraphTypeAliases{
  def forest[U >: Nothing]:Stream[Region[U]] = {
    n match {
      case 0 => Stream.empty
      case 1 => Stream(Stream(leafI(0)))
      case _ => (PlaceId(1) juxtapose PlaceId(n-1)).forest
    }
  }
}

trait PlaceGraphTypeAliases{
  type Region[A] = Stream[Tree[Site \/ A]]
}

trait PlaceGraphInstances extends PlaceGraphTypeAliases{

  implicit def eitherEqual[A: Equal]: Equal[Site \/ A] = (x0: Site \/ A, x1: Site \/ A) => x0 === x1

  implicit def treeEqual[A: Equal]: Equal[Tree[Site \/ A]] = (s0: Tree[Site \/ A], s1: Tree[Site \/ A]) => Tree.treeEqual[Site \/ A].equal(s0, s1) //TODO - this needs to be tested. probably it fails since subforest must be treated as sets of trees

  implicit def streamAsSetEqual[A: Equal]: Equal[Region[A]] = new Equal[Region[A]]{
    def equal(s0: Region[A], s1: Region[A]): Boolean = {(s0: Region[A],s1: Region[A]) match {
      case (Stream(), Stream()) => true
      case (Stream(a), Stream(b)) => a === b
      case (x0,x1) if x0.size != x1.size => false
      case (x0,x1) =>
        val i = s0.indexWhere(_ === x1.head)
        if (i == -1)
          false
        else if (i == (x0.size - 1)) {
          equal(x0.dropRight(1), x1.tail)
        }
        else if (i == 0) {
          equal(x0.drop(1), x1.tail)
        }
        else {
          equal(x0.dropRight(s0.size - i) ++ x0.drop(i + 1), x1.tail)
        }
    }
    }
  }

  implicit def forestEqual[A: Equal]: Equal[Stream[Region[A]]] =
    (s0: Stream[Region[A]], s1: Stream[Region[A]]) => {
    if (s0.size != s1.size)
      false
    else if (s0.isEmpty)
      true
    else if (s0.size == 1)
      s0.head === s1.head
    else
      (s0.head === s1.head) && forestEqual[A].equal(s0.tail, s1.tail)
  }

  implicit def placeGraphEqual[A: Equal]: Equal[PlaceGraph[A]] = Equal.equalBy(_.forest)

  implicit def ionEqual[A: Equal]: Equal[PlaceIon[A]] = (a1: PlaceIon[A], a2: PlaceIon[A]) => placeGraphEqual[A].equal(a1, a2)

  implicit def atomEqual[A: Equal]: Equal[Atom[A]] = (a1: Atom[A], a2: Atom[A]) => placeGraphEqual[A].equal(a1, a2)

  implicit def joinEqual: Equal[Join.type] = (a1: Join.type, a2: Join.type) => placeGraphEqual[Nothing].equal(a1, a2)

  implicit def permuteEqual: Equal[Permute] = (a1: Permute, a2: Permute) => placeGraphEqual[Nothing].equal(a1, a2)

  implicit def unitEqual: Equal[PlaceUnit.type] = (a1: PlaceUnit.type, a2: PlaceUnit.type) => placeGraphEqual[Nothing].equal(a1, a2)

  implicit def idEqual: Equal[PlaceId] = (a1: PlaceId, a2: PlaceId) => placeGraphEqual[Nothing].equal(a1, a2)

  implicit def sh: Show[Site \/ Any] = new Show[Site \/ Any] {
    override def shows(n: Site \/ Any): String = {
      n match {
        case -\/(s) => s.toString
        case \/-(a) => a.toString
      }
    }
  }

  implicit def placeGraphShow[A: Show]: Show[PlaceGraph[A]] = Show.show(a => drawRegions(a.forest))

  private def drawRegions(rs: Stream[Region[Any]]): String =
    rs.length match{
      case 0 => ""
      case n => drawRegions(rs.init) + drawTrees(n-1,rs.last)
    }

  private def drawTrees(region: Int,ts: Region[Any]): String = "\n#" + region + "[\n" + ts.map(_.drawTree).foldLeft("")(_+_) + "]\n"

}
