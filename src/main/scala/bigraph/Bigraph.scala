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
    (Bigraph(PlaceGraph.merge(2),LinkUnit) juxtapose BigraphId(0,bPar.linkGraph.linkOuterFace.toStream)) compose bPar
  }
  def <> [U >: A](b: Bigraph[U]) = (self || BigraphId(0,b.linkGraph.linkOuterFace.toStream)) compose b
}

object Bigraph{
  def apply[A](p: PlaceGraph[A], l: LinkGraph[A]): Bigraph[A] with Object = new Bigraph[A]{
    override def placeGraph = p
    override def linkGraph = l
  }
  def unapply[A](arg: Bigraph[A]): Option[(PlaceGraph[A],LinkGraph[A])] = Some((arg.placeGraph,arg.linkGraph))
}

case class Ion[A](node: A, links: Map[Port,Option[OuterName]]) extends Bigraph[A] {
  def placeGraph: PlaceGraph[A] = PlaceIon(node)
  def linkGraph: LinkGraph[A] = LinkIon(node,links)
}

case class BigraphId(i: Int, names: Stream[Symbol]) extends Bigraph[Nothing] {
  def linkGraph: LinkGraph[Nothing] = LinkId(names)
  def placeGraph: PlaceGraph[Nothing] = PlaceId(i)
}

case object Unit extends Bigraph[Nothing] {
  def linkGraph: LinkGraph[Nothing] = LinkUnit
  def placeGraph: PlaceGraph[Nothing] = PlaceUnit
}


