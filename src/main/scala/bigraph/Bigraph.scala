package bigraph


import scalaz.{-\/, Tree, \/, \/-}
import placeGraph._
import linkGraph._

/**
  * Created by eloipereira on 8/18/16.
  */
trait Bigraph[+A,+B]{self =>
  def placeGraph: PlaceGraph[A]
  def linkGraph: LinkGraph[A,B]
  lazy val innerFace: (Int, Set[Symbol]) = (placeGraph.placeInnerFace,linkGraph.linkInnerFace)
  lazy val outerFace: (Int, Set[Symbol]) = (placeGraph.placeOuterFace,linkGraph.linkOuterFace)

  def compose[U0 >: A, U1 >: B]: PartialFunction[Bigraph[U0, U1], Bigraph[U0, U1]] = {
    case b: Bigraph[U0,U1] if b.outerFace == self.innerFace =>
      Bigraph(self.placeGraph compose b.placeGraph, self.linkGraph compose b.linkGraph)
  }
  def juxtapose[U0 >: A, U1 >: B]: PartialFunction[Bigraph[U0, U1], Bigraph[U0, U1]] = {
    case b: Bigraph[U0,U1]
      if self.linkGraph.juxtapose.isDefinedAt(b.linkGraph) =>
        Bigraph(self.placeGraph juxtapose  b.placeGraph, self.linkGraph juxtapose b.linkGraph)
  }
}

object Bigraph{
  def apply[A,B](p: PlaceGraph[A], l: LinkGraph[A,B]): Bigraph[A, B] with Object = new Bigraph[A,B]{
    override def placeGraph = p
    override def linkGraph = l
  }
  def unapply[A,B](arg: Bigraph[A,B]): Option[(PlaceGraph[A],LinkGraph[A,B])] = Some((arg.placeGraph,arg.linkGraph))
}

case class Ion[A](node: A, names: Stream[Symbol]) extends Bigraph[A,Nothing] {
  def placeGraph: PlaceGraph[A] = PlaceIon(node)
  def linkGraph: LinkGraph[A,Nothing] = LinkGraph(hypergraph)
  def hypergraph[U0 >: Nothing, U1 >: Nothing] =
    names.foldLeft(Map(): Map[Option[\/[Symbol, Port[U0]]], Option[\/[Symbol, Edge[U1]]]])(
      (acc, n) =>
        acc ++ Map(Some(\/-(Port[U0](node.asInstanceOf[U0],names.indexOf(n)))) -> Some(-\/(n)))
    )
}

case class Id(i: Int, names: Stream[Symbol]) extends Bigraph[Nothing,Nothing] {
  def linkGraph: LinkGraph[Nothing,Nothing] = LinkId(names)
  def placeGraph: PlaceGraph[Nothing] = PlaceId(i)
}

case object Unit extends Bigraph[Nothing,Nothing] {
  def linkGraph: LinkGraph[Nothing,Nothing] = LinkUnit
  def placeGraph: PlaceGraph[Nothing] = PlaceUnit
}

