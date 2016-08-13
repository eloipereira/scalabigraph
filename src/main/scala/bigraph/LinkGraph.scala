package bigraph

sealed trait Point[+A]{
  def isInnerName: Boolean =
    this match {
      case InnerName(_) => true
      case _ => false
    }
  def isPort: Boolean =
    this match {
      case Port(_,_) => true
      case _ => false
    }
}
case class InnerName(id: Symbol) extends Point[Nothing]
case class Port[A](n: A, id: Int) extends Point[A]

sealed trait Link[+A]{
  def isEdge: Boolean =
    this match {
      case Edge(_) => true
      case _ => false
    }
  def isOuterName: Boolean =
    this match {
      case OuterName(_) => true
      case _ => false
    }
}
case class Edge[A](id: A) extends Link[A]
case class OuterName(id: Symbol) extends Link[Nothing]


trait LinkGraph[+A,+B] {
  def hypergraph[U0 >: A, U1 >: B]: Map[Option[Point[U0]], Option[Link[U1]]]
  def innerFace[U0 >: A, U1 >: B]: Set[Point[U0]] = hypergraph[U0,U1].keySet.filter(x => x.get.isInnerName).flatten
  def ports[U0 >: A, U1 >: B]: Set[Point[U0]] = hypergraph[U0,U1].keySet.filter(x => x.get.isPort).flatten
  def outerFace[U0 >: A, U1 >: B]: Set[Link[U1]] = hypergraph[U0,U1].values.toSet.filter(x => x.get.isOuterName).flatten
  def edges[U0 >: A, U1 >: B]: Set[Link[U1]] = hypergraph[U0,U1].values.toSet.filter(x => x.get.isEdge).flatten

  def compose[U0 >: A, U1 >: B]: PartialFunction[LinkGraph[U0, U1],LinkGraph[U0,U1]] = ???
  def juxtapose[U0 >: A, U1 >: B]: PartialFunction[LinkGraph[U0, U1],LinkGraph[U0,U1]] = ???
}

object LinkGraph{
  def apply[A,B](h: Map[Option[Point[A]], Option[Link[B]]]): LinkGraph[A,B] = new LinkGraph[A,B] {
    def hypergraph[U0 >: A, U1 >: B]: Map[Option[Point[U0]], Option[Link[U1]]] = h.asInstanceOf[Map[Option[Point[U0]], Option[Link[U1]]]]
  }
  def unapply[A,B](arg: LinkGraph[A,B]): Option[Map[Option[Point[A]], Option[Link[B]]]] = Some(arg.hypergraph)
}

case class Closure(innerName: InnerName) extends LinkGraph[Nothing,Nothing] with LinkGraphFunctions {
  def hypergraph[U0 >: Nothing, U1 >: Nothing] = closure(innerName).hypergraph
}

trait LinkGraphFunctions{
  def substitution(innerNames: List[InnerName], outerName: OuterName): LinkGraph[Nothing,Nothing] =
    new LinkGraph[Nothing,Nothing] {
      def hypergraph[U0 >: Nothing, U1 >: Nothing] = innerNames.size match {
        case 0 => Map()
        case n => Map(Some(innerNames.head) -> Some(outerName)) ++ substitution(innerNames.tail,outerName).hypergraph
      }
    }

  def closure(innerName: InnerName): LinkGraph[Nothing,Nothing] =
    new LinkGraph[Nothing,Nothing] {
      def hypergraph[U0 >: Nothing, U1 >: Nothing] = Map(Some(innerName) -> None)
    }
}