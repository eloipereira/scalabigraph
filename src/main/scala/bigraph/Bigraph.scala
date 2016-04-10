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
    override def toString = "$" + i
  }
  def unapply(s: Site): Option[Int] = Some(s.id)
}

sealed trait Place[A]{
  def region: Region
  def graph: Stream[Tree[Site \/ A]]
}

object Place{
  def apply[A](r: Region, forest: => Stream[Tree[Site \/ A]]): Place[A] = new Place[A]{
    lazy val region = r
    lazy val graph = forest
    override def toString = "<place>"
  }
  def unapply[A](p: Place[A]): Option[(Region,Stream[Tree[Site \/ A]])] = Some((p.region,p.graph))
}

sealed trait PlaceGraph[A]{
  def places: Stream[Place[A]]
}

object PlaceGraph extends PlaceGraphFunctions{
  def apply[A](ps: Stream[Place[A]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val places = ps
    override def toString = "<placeGraph>"
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Place[A]]] = Some(pg.places)
}

trait PlaceGraphFunctions {
  implicit val r: Region = Region(0)
  implicit val s: Site = Site(0)
  def empty[A](implicit r: Region): PlaceGraph[A] = PlaceGraph(Stream(Place(r,Stream.empty)))
  def atom[A](n: => A)(implicit r: Region): PlaceGraph[A] = PlaceGraph(Stream(Place(r,Stream(Leaf(\/-(n))))))
  def ion[A](n: => A)(implicit r: Region, s: Site): PlaceGraph[A] = PlaceGraph(Stream(Place(r,Stream(Node(\/-(n), Stream(Leaf(-\/(s))))))))
  def compose[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = ???
  def product[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = ???
}
