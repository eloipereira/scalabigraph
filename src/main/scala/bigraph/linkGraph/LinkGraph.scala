package bigraph.linkGraph

import scalaz.{-\/, \/, \/-, Equal}

/**
  * Created by eloipereira on 8/22/16.
  */
trait LinkGraph[+A] {
  self =>
  def hypergraph[U >: A]: Map[Option[Symbol \/ Port[U]], Option[Symbol]]

  def linkInnerFace[U >: A]: Set[Symbol] = hypergraph[U].keySet.flatMap {
    (x: Option[Symbol \/ Port[U]]) => x match {
      case Some(-\/(n)) => Some(n)
      case _ => None
    }
  }

  def ports[U >: A]: Set[Port[U]] = hypergraph[U].keySet.flatMap {
    (x: Option[Symbol \/ Port[U]]) => x match {
      case Some(\/-(n)) => Some(n)
      case _ => None
    }
  }

  def linkOuterFace[U >: A]: Set[Symbol] = hypergraph[U].values.toSet.flatten

  def compose[U >: A]: PartialFunction[LinkGraph[U], LinkGraph[U]] = {
    case l: LinkGraph[U]
      if self.linkInnerFace == l.linkOuterFace =>
      val h: Map[Option[Symbol \/ Port[U]], Option[Symbol]] = {
        val inners0: Map[Option[Symbol \/ Port[U]], Option[Symbol]] =
          self.hypergraph[U].filter(a => a._1 match {
            case Some(-\/(s)) => true
            case _ => false
          })
        val rest0: Map[Option[Symbol \/ Port[U]], Option[Symbol]] =
          self.hypergraph[U].filter(a => a._1 match {
            case Some(-\/(s)) => false
            case _ => true
          })
        val outers1: Map[Option[Symbol \/ Port[U]], Option[Symbol]] =
          l.hypergraph[U].filter(a => a._2 match {
            case Some(s) => true
            case None => false
          })
        val rest1: Map[Option[Symbol \/ Port[U]], Option[Symbol]] =
          l.hypergraph[U].filter(a => a._2 match {
            case Some(s) => false
            case None => true
          })
        if (outers1.isEmpty)
          rest0 ++ rest1
        else
          rest0 ++ rest1 ++ (outers1 map {
            case (a, Some(s)) => (a, inners0(Some(-\/(s))))
            case p => p
          })
      }
      LinkGraph(h)
  }


  def juxtapose[U >: A]: PartialFunction[LinkGraph[U], LinkGraph[U]] = {
    case l: LinkGraph[U]
      if l.linkInnerFace.intersect(self.linkInnerFace) == Set.empty && l.linkOuterFace.intersect(self.linkOuterFace) == Set.empty =>
      LinkGraph(self.hypergraph[U] ++ l.hypergraph)
  }

  def juxtaposeWithSharing[U >: A]: PartialFunction[LinkGraph[U], LinkGraph[U]] = {
    case l: LinkGraph[U]
      if l.linkInnerFace.intersect(self.linkInnerFace).forall(a => self.hypergraph(Some(-\/(a))) == l.hypergraph(Some(-\/(a)))) =>
      LinkGraph(self.hypergraph[U] ++ l.hypergraph)
  }

}

object LinkGraph extends LinkGraphFunctions{
  def apply[A](h: Map[Option[Symbol \/ Port[A]], Option[Symbol]]): LinkGraph[A] = new LinkGraph[A] {
    def hypergraph[U >: A]: Map[Option[Symbol \/ Port[U]], Option[Symbol]] = h.asInstanceOf[Map[Option[Symbol \/ Port[U]], Option[Symbol]]]
  }

  def unapply[A](arg: LinkGraph[A]): Option[Map[Option[Symbol \/ Port[A]], Option[Symbol]]] = Some(arg.hypergraph)
}


case class Closure(innerName: Symbol) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = closure(innerName).hypergraph
}

case class Substitution(innerNames: Stream[Symbol], outerName: Symbol) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = substitution(innerNames, outerName).hypergraph
}

case class LinkId(names: Option[Stream[Symbol]]) extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = id(names).hypergraph
}

case object LinkUnit extends LinkGraph[Nothing] with LinkGraphFunctions {
  def hypergraph[U >: Nothing] = Map()
}

trait LinkGraphFunctions {
  def substitution(innerNames: Stream[Symbol], outerName: Symbol): LinkGraph[Nothing] =
    new LinkGraph[Nothing] {
      def hypergraph[U >: Nothing] = innerNames.size match {
        case 0 => Map(None -> Some(outerName))
        case 1 => Map(Some(-\/(innerNames.head)) -> Some(outerName))
        case n => Map(Some(-\/(innerNames.head)) -> Some(outerName)) ++ substitution(innerNames.tail, outerName).hypergraph[U]
      }
    }

  def closure(innerName: Symbol): LinkGraph[Nothing] =
    new LinkGraph[Nothing] {
      def hypergraph[U >: Nothing] = Map(Some(-\/(innerName)) -> None)
    }

  def id(names: Option[Stream[Symbol]]): LinkGraph[Nothing] =
    new LinkGraph[Nothing]{
      def hypergraph[U >: Nothing] = names match {
        case Some(ns) => ns.foldLeft(Map(): Map[Option[\/[Symbol, Port[U]]], Option[Symbol]])(
          (acc, n) => acc ++ Map(Some(-\/(n)) -> Some(n)))
        case None => Map(None -> None)
      }


    }

  implicit def linkGraphEqual[A]: Equal[LinkGraph[A]] = new Equal[LinkGraph[A]] {
    override def equal(a1: LinkGraph[A], a2: LinkGraph[A]): Boolean = a1.hypergraph == a2.hypergraph
  }
  implicit def substitutionEqual: Equal[Substitution] = new Equal[Substitution] {
    override def equal(a1: Substitution, a2: Substitution): Boolean = linkGraphEqual.equal(a1,a2)
  }
  implicit def closureEqual: Equal[Closure] = new Equal[Closure] {
    override def equal(a1: Closure, a2: Closure): Boolean = linkGraphEqual.equal(a1,a2)
  }
  implicit def idEqual: Equal[LinkId] = new Equal[LinkId] {
    override def equal(a1: LinkId, a2: LinkId): Boolean = linkGraphEqual.equal(a1,a2)
  }
}