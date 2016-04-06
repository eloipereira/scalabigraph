package bigraph

import scalaz._
import scalaz.Tree._

sealed trait Region{
  def id: Int
}

object Region{
  def apply(i: => Int): Region = new Region{
    lazy val id = i
    override def toString = "#" + i
  }

  def unapply(r: Region): Option[Int] = Some(r.id)
}

sealed trait Site{
  def id: Int
}

object Site{
  def apply(i: => Int) = new Site{
    lazy val id = i
  }
  def unapply(s: Site): Option[Int] = Some(s.id)
}

sealed trait PlaceGraph[A]{
  def region: Region
  def graph: Stream[Tree[A \/ Site]]
}

object PlaceGraph extends PlaceGraphFunctions{
  def apply[A](r: Region, forest: => Stream[Tree[A \/ Site]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val region = r
    lazy val graph = forest
    override def toString = "<placeGraph>"
  }
  def unapply[A](p: PlaceGraph[A]): Option[(Region,Stream[Tree[A \/ Site]])] = Some((p.region,p.graph))
}

trait PlaceGraphFunctions {
  implicit val r: Region = Region(0)
  implicit val s: Site = Site(0)
  def empty[A](implicit r: Region): PlaceGraph[A] = PlaceGraph(r,Stream.empty)
  def atom[A](n: => A)(implicit r: Region): PlaceGraph[A] = PlaceGraph(r,Stream(Leaf(-\/(n))))
  def ion[A](n: => A)(implicit r: Region, s: Site): PlaceGraph[A] = PlaceGraph(r,Stream(Node(-\/(n), Stream(Leaf(\/-(s))))))
  def compose[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = ???
}
