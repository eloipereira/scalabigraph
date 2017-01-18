package bigraph


import scalaz.{\/, \/-}
import placeGraph._
import linkGraph._

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
      Bigraph(self.placeGraph compose b.placeGraph, self.linkGraph compose b.linkGraph)
  }
  def juxtapose[U >: A]: PartialFunction[Bigraph[U], Bigraph[U]] = {
    case b: Bigraph[U]
      if self.linkGraph.juxtapose.isDefinedAt(b.linkGraph) =>
        Bigraph(self.placeGraph juxtapose  b.placeGraph, self.linkGraph juxtapose b.linkGraph)
  }
  def || [U >: A](b: Bigraph[U]) = Bigraph(self.placeGraph || b.placeGraph, self.linkGraph juxtaposeWithSharing b.linkGraph)
  def | [U >: A](b: Bigraph[U]) = {
    val bPar = self || b
    (Bigraph(PlaceGraph.merge(2),LinkUnit) juxtapose Id(0,bPar.linkGraph.linkOuterFace.toStream)) compose bPar
  }
  def <> [U >: A](b: Bigraph[U]) = (self || Id(0,b.linkGraph.linkOuterFace.toStream)) compose b
}

object Bigraph{
  def apply[A](p: PlaceGraph[A], l: LinkGraph[A]): Bigraph[A] with Object = new Bigraph[A]{
    override def placeGraph = p
    override def linkGraph = l
  }
  def unapply[A](arg: Bigraph[A]): Option[(PlaceGraph[A],LinkGraph[A])] = Some((arg.placeGraph,arg.linkGraph))
}

case class Ion[A](node: A, names: Stream[Symbol]) extends Bigraph[A] {
  def placeGraph: PlaceGraph[A] = PlaceIon(node)
  def linkGraph: LinkGraph[A] = LinkGraph(hypergraph)
  def hypergraph[U >: Nothing] =
    names.foldLeft(Map(): Map[Option[Symbol] \/ Port[U], Option[Symbol]])(
      (acc, n) =>
        acc ++ Map(\/-(Port[U](node.asInstanceOf[U],names.indexOf(n))) -> Some(n))
    )
}

case class Id(i: Int, names: Stream[Symbol]) extends Bigraph[Nothing] {
  def linkGraph: LinkGraph[Nothing] = LinkId(names)
  def placeGraph: PlaceGraph[Nothing] = PlaceId(i)
}

case object Unit extends Bigraph[Nothing] {
  def linkGraph: LinkGraph[Nothing] = LinkUnit
  def placeGraph: PlaceGraph[Nothing] = PlaceUnit
}

