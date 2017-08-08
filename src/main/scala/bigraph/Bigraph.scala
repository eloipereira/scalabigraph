package bigraph


import scalaz._, Scalaz._
import placeGraph._
import linkGraph._
import LinkGraph._

/**
  * Created by eloipereira on 8/18/16.
  */
trait Bigraph[+A]{self =>
  def placeGraph: PlaceGraph[A]
  def linkGraph: LinkGraph[A]
  lazy val innerFace: (Int, Set[Symbol]) = (placeGraph.placeInnerFace,linkGraph.linkInnerFace)
  lazy val outerFace: (Int, Set[Symbol]) = (placeGraph.placeOuterFace,linkGraph.linkOuterFace)

  def compose[U >: A]: PartialFunction[Bigraph[U], Bigraph[U]] = {
    case b: Bigraph[U] if b.outerFace == self.innerFace =>
    new Bigraph[U]{
      override def placeGraph = self.placeGraph compose b.placeGraph
      override def linkGraph = self.linkGraph compose b.linkGraph
    }
  }

  def juxtapose[U >: A]: PartialFunction[Bigraph[U], Bigraph[U]] = {
    case b: Bigraph[U]
      if self.linkGraph.juxtapose.isDefinedAt(b.linkGraph) =>
        new Bigraph[U]{
          override def placeGraph = self.placeGraph juxtapose  b.placeGraph
          override def linkGraph = self.linkGraph juxtapose b.linkGraph
        }
  }

  def || [U >: A](b: Bigraph[U]) = new Bigraph[U]{
    override def placeGraph = self.placeGraph || b.placeGraph
    override def linkGraph = self.linkGraph juxtaposeWithSharing b.linkGraph
  }

  def | [U >: A](b: Bigraph[U]) = {
    val bPar = self || b
    val merge = new Bigraph[U] {
      override def placeGraph = Merge(2)
      override def linkGraph = LinkUnit
    }
    (merge juxtapose Id(0,bPar.linkGraph.linkOuterFace.toStream)) compose bPar
  }

  def <> [U >: A](b: Bigraph[U]) = (self || Id(0,b.linkGraph.linkOuterFace.toStream)) compose b

  def map[B](f: A => B):Bigraph[B] = new Bigraph[B]{
    override def placeGraph = self.placeGraph.map(f)
    override def linkGraph = self.linkGraph.map(f)
  }
}

object Bigraph extends BigraphInstances

case class Ion[A](node: A, links: Stream[Option[OuterName]]) extends Bigraph[A] {
  def placeGraph: PlaceGraph[A] = PlaceIon(node)
  def linkGraph: LinkGraph[A] = LinkIon(node,links)
}

case class Id(i: Int, names: Stream[Symbol]) extends Bigraph[Nothing] {
  def linkGraph: LinkGraph[Nothing] = LinkId(names)
  def placeGraph: PlaceGraph[Nothing] = PlaceId(i)
}

case object Unit extends Bigraph[Nothing] {
  def linkGraph: LinkGraph[Nothing] = LinkUnit
  def placeGraph: PlaceGraph[Nothing] = PlaceUnit
}

trait BigraphInstances extends PlaceGraphInstances with LinkGraphInstances{
  implicit def bigraphEqual[A: Equal]: Equal[Bigraph[A]] = (b0: Bigraph[A], b1: Bigraph[A]) => placeGraphEqual[A].equal(b0.placeGraph, b1.placeGraph) && linkGraphEqual[A].equal(b0.linkGraph,b1.linkGraph)
  implicit def ionEqual[A: Equal]: Equal[Ion[A]] = (i0: Ion[A], i1: Ion[A]) => bigraphEqual[A].equal(i0,i1)
  implicit def idEqual: Equal[Id] = (i0: Id, i1: Id) => bigraphEqual[Nothing].equal(i0,i1)
  implicit def unitEqual: Equal[Unit.type] = (u0: Unit.type, u1: Unit.type) => true
  implicit def bigraphShow[A: Show]: Show[Bigraph[A]] = Show.show(b => PlaceGraph.placeGraphToTerm(b.placeGraph)(b.linkGraph))
  implicit val bigraphMonoid: Monoid[Bigraph[Any]] = new Monoid[Bigraph[Any]]{
    def zero: Bigraph[Any] = Unit
    def append(b0: Bigraph[Any], b1: => Bigraph[Any]) = b0 || b1
  }
  implicit val bigraphFunctor = new Functor[Bigraph]{
    def map[A,B](b: Bigraph[A])(f: A => B):Bigraph[B] = b.map(f)
  }
}
