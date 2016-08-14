package bigraph

import scalaz._

case class Port[A](n: A, id: Int)

case class Edge[A](id: A)

trait LinkGraph[+A,+B]{ self =>
  def hypergraph[U0 >: A, U1 >: B]: Map[Option[Symbol \/ Port[U0]], Option[Symbol \/ Edge[U1]]]
  def innerFace[U0 >: A, U1 >: B]: Set[Symbol] = hypergraph[U0, U1].keySet.flatMap {
    (x: Option[Symbol \/ Port[U0]]) => x match {
      case Some(-\/(n)) => Some(n)
      case _ => None
    }
  }
  def ports[U0 >: A, U1 >: B]: Set[Port[U0]] = hypergraph[U0,U1].keySet.flatMap {
    (x: Option[Symbol \/ Port[U0]]) => x match {
      case Some(\/-(n)) => Some(n)
      case _ => None
    }
  }
  def outerFace[U0 >: A, U1 >: B]: Set[Symbol] = hypergraph[U0,U1].values.toSet.flatMap {
    (x: Option[Symbol \/ Edge[U1]]) => x match {
      case Some(-\/(n)) => Some(n)
      case _ => None
    }
  }
  def edges[U0 >: A, U1 >: B]: Set[Edge[U1]] = hypergraph[U0,U1].values.toSet.flatMap {
    (x: Option[Symbol \/ Edge[U1]]) => x match {
      case Some(\/-(n)) => Some(n)
      case _ => None
    }
  }

//  def compose[U0 >: A, U1 >: B]: PartialFunction[LinkGraph[U0,U1],LinkGraph[U0,U1]] = {
//    case l: LinkGraph[U0, U1]
//      if self.outerFace.map((o: OuterName) => o.id) == l.innerFace.map((i: InnerName) => i.id) =>{
//      val h: Map[Option[Point[U0]], Option[Link[U1]]] = ???
//      LinkGraph(h)
//    }
//  }
  def juxtapose[U0 >: A, U1 >: B]: PartialFunction[LinkGraph[U0, U1],LinkGraph[U0,U1]] = {
    case l: LinkGraph[U0, U1]
      if l.innerFace.intersect(self.innerFace) == Set.empty && l.outerFace.intersect(self.outerFace) == Set.empty =>
        LinkGraph(self.hypergraph[U0,U1] ++ l.hypergraph)
  }

}

object LinkGraph {
  def apply[A,B](h: Map[Option[Symbol \/ Port[A]], Option[Symbol \/ Edge[B]]]): LinkGraph[A,B] = new LinkGraph[A,B] {
    def hypergraph[U0 >: A, U1 >: B]: Map[Option[Symbol \/ Port[U0]], Option[Symbol \/ Edge[U1]]] = h.asInstanceOf[Map[Option[Symbol \/ Port[U0]], Option[Symbol \/ Edge[U1]]]]
  }
  def unapply[A,B](arg: LinkGraph[A,B]): Option[Map[Option[Symbol \/ Port[A]], Option[Symbol \/ Edge[B]]]] = Some(arg.hypergraph)
}

case class Closure(innerName: Symbol) extends LinkGraph[Nothing,Nothing] with LinkGraphFunctions {
  def hypergraph[U0 >: Nothing, U1 >: Nothing] = closure(innerName).hypergraph
}

case class Substitution(innerNames: List[Symbol], outerName: Symbol) extends LinkGraph[Nothing,Nothing] with LinkGraphFunctions {
  def hypergraph[U0 >: Nothing, U1 >: Nothing] = substitution(innerNames,outerName).hypergraph
}

trait LinkGraphFunctions{
  def substitution(innerNames: List[Symbol], outerName: Symbol): LinkGraph[Nothing,Nothing] =
    new LinkGraph[Nothing,Nothing] {
      def hypergraph[U0 >: Nothing, U1 >: Nothing] = innerNames.size match {
        case 0 => Map()
        case n => Map(Some(-\/(innerNames.head)) -> Some(-\/(outerName))) ++ substitution(innerNames.tail,outerName).hypergraph[U0,U1]
      }
    }

  def closure(innerName: Symbol): LinkGraph[Nothing,Nothing] =
    new LinkGraph[Nothing,Nothing] {
      def hypergraph[U0 >: Nothing, U1 >: Nothing] = Map(Some(-\/(innerName)) -> None)
    }
}

