package bigraph.linkGraph

import scalaz._
import Scalaz._

import LinkGraph._

/**
  * Created by eloipereira on 8/22/16.
  */





trait LinkGraph[+A] {
  self =>
  def hypergraph[U >: A]: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]]

  def linkInnerFace[U >: A]: Set[InnerName] = for{
    k: \/[Option[InnerName], NodePortPair[U]] <- hypergraph[U].keySet
    i: Option[InnerName] <- k.swap.toOption
    j: InnerName <- i
    if k.isLeft
  } yield j

  def ports[U >: A]: Set[NodePortPair[U]] = for{
    k: \/[Option[Symbol], NodePortPair[U]] <- hypergraph[U].keySet
    i: NodePortPair[U] <- k.toOption
    if k.isRight
  } yield i

  def linkOuterFace[U >: A]: Set[OuterName] = hypergraph[U].values.toSet.flatten

  def compose[U >: A]: PartialFunction[LinkGraph[U], LinkGraph[U]] = {
    case l: LinkGraph[U]
      if self.linkInnerFace == l.linkOuterFace =>
      val h: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] = {
        val inners0: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] =
          self.hypergraph[U].filter(a => a._1 match {
            case -\/(Some(s)) => true
            case _ => false
          })
        val rest0: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] =
          self.hypergraph[U].filter(a => a._1 match {
            case -\/(Some(s)) => false
            case _ => true
          })
        val outers1: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] =
          l.hypergraph[U].filter(a => a._2 match {
            case Some(s) => true
            case None => false
          })
        val rest1: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] =
          l.hypergraph[U].filter(a => a._2 match {
            case Some(s) => false
            case None => true
          })
        if (outers1.isEmpty)
          rest0 ++ rest1
        else
          rest0 ++ rest1 ++ (outers1 map {
            case (a, Some(s)) => (a, inners0(-\/(Some(s))))
            case p => p
          })
      }
      LinkGraph(h)
  }


  def juxtapose[U >: A]: PartialFunction[LinkGraph[U], LinkGraph[U]] = {
    case l: LinkGraph[U]
      if l.linkInnerFace.intersect(self.linkInnerFace) == Set.empty && l.linkOuterFace.intersect(self.linkOuterFace) == Set.empty =>
      LinkGraph[U](self.hypergraph[U] ++ l.hypergraph)
  }

  def juxtaposeWithSharing[U >: A]: PartialFunction[LinkGraph[U], LinkGraph[U]] = {
    case l: LinkGraph[U]
      if l.linkInnerFace.intersect(self.linkInnerFace).forall(a => self.hypergraph(-\/(Some(a))) == l.hypergraph(-\/(Some(a)))) =>
      LinkGraph[U](self.hypergraph[U] ++ l.hypergraph)
  }

}

object LinkGraph extends LinkGraphFunctions{
  def apply[A](h: Map[Option[InnerName] \/ NodePortPair[A], Option[OuterName]]): LinkGraph[A] = new LinkGraph[A] {
    def hypergraph[U >: A]: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] = h.asInstanceOf[Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]]]
  }

  def unapply[A](arg: LinkGraph[A]): Option[Map[Option[InnerName] \/ NodePortPair[A], Option[OuterName]]] = Some(arg.hypergraph)

  type NodePortPair[A] = (A,Port)
  type InnerName = Symbol
  type OuterName = Symbol
  type Port = Int
}


case class Closure(innerName: InnerName) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = closure(innerName).hypergraph
}

case class Substitution(innerNames: Stream[InnerName], outerName: OuterName) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = substitution(innerNames, outerName).hypergraph
}

case class Renaming(rename: Map[InnerName,OuterName]) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing]: Map[Option[InnerName] \/ NodePortPair[U], Option[OuterName]] = for {
    r <- rename
  } yield (-\/(Some(r._1)) -> Some(r._2))
}

case class LinkId(names: Stream[InnerName]) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = id(names).hypergraph
}

case class LinkIon[A](node: A, links: Map[Port,Option[OuterName]]) extends LinkGraph[A] with LinkGraphFunctions {
  def hypergraph[U >: A]: Map[Option[InnerName] \/ (U, Port), Option[OuterName]] = for{
    l <- links
  } yield (\/-(node,l._1) -> l._2)
}

case object LinkUnit extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = Map()
}



trait LinkGraphFunctions {
  def substitution(innerNames: Stream[Symbol], outerName: Symbol): LinkGraph[Nothing] =
    new LinkGraph[Nothing] {
      def hypergraph[U >: Nothing] = innerNames.size match {
        case 0 => Map(-\/(None) -> Some(outerName))
        case 1 => Map(-\/(Some(innerNames.head)) -> Some(outerName))
        case n => Map(-\/(Some(innerNames.head)) -> Some(outerName)) ++ substitution(innerNames.tail, outerName).hypergraph[U]
      }
    }

  def closure(innerName: Symbol): LinkGraph[Nothing] =
    new LinkGraph[Nothing] {
      def hypergraph[U >: Nothing] = Map(-\/(Some(innerName)) -> None)
    }

  def id(names: Stream[Symbol]): LinkGraph[Nothing] =
    new LinkGraph[Nothing]{
      def hypergraph[U >: Nothing] = names match {
        case Stream() => Map(-\/(None) -> None)
        case ns => ns.foldLeft(Map(): Map[Option[Symbol] \/ NodePortPair[U], Option[Symbol]])(
          (acc, n) => acc ++ Map(-\/(Some(n)) -> Some(n)))
      }
    }

  implicit def linkGraphEqual[A]: Equal[LinkGraph[A]] = (a1: LinkGraph[A], a2: LinkGraph[A]) => a1.hypergraph == a2.hypergraph
  implicit def substitutionEqual: Equal[Substitution] = (a1: Substitution, a2: Substitution) => linkGraphEqual.equal(a1, a2)
  implicit def closureEqual: Equal[Closure] = (a1: Closure, a2: Closure) => linkGraphEqual.equal(a1, a2)
  implicit def idEqual: Equal[LinkId] = (a1: LinkId, a2: LinkId) => linkGraphEqual.equal(a1, a2)
  implicit def LinkGraphShows[A]: Show[LinkGraph[A]] = Show.shows{ //TOOD - Still incomplete
    case Closure(s) => "/" + s.name
    case Substitution(is,o) => o.toString + "/" + {
      if(is.size == 1)
        is.head.name
      else
        "(" + is.head.name + is.tail.foldLeft("")((s,sym)=> s + "," + sym.name) + ")"
    }
    case Renaming(m) => {
      val h = (m.head._1.name, m.head._2.name)
      val t = m.tail.foldLeft(("","")){
            case (p, (i,o)) => (p._1 + "," + i.name, p._2 + "," + o.name)
          }
      "[" + h._2 + t._2 + "/" + h._1 + t._1 + "]"
    }
    case LinkId(ns) =>
      if (ns.size == 1)
        ns.head.name + "/" + ns.head.name
      else{
        val nsStr = "(" + ns.head.name + ns.tail.foldLeft("")((s,sym)=> s + "," + sym.name) + ")"
        nsStr + "/" + nsStr
      }
    case LinkIon(n,ls) =>
      val ns: Stream[Symbol] = ls.values.toStream.flatten
      n.toString + "[" + ns.head.name + ns.tail.foldLeft("")((s,sym)=> s + "," + sym.name) + "]"
  }
}