package bigraph

import scalaz._
import scalaz.Tree._
import scalaz.std.stream.{streamInstance, streamMonoid}

sealed trait Site

case class Open(id: Int) extends Site {
  override def toString = "$" + id
}
case object Closed extends Site{
  override def toString = "(-)"
}

sealed trait PlaceGraph[A]{
  def forest: Stream[Tree[Site \/ A]]
  implicit def sh: Show[Site \/ A] = new Show[Site \/ A] {
    override def shows(n: Site \/ A): String = {
      n match {
        case -\/(s) => s.toString
        case \/-(a) => a.toString
      }
    }
  }
  def drawPlaceGraph: String = forest.map(_.drawTree).foldLeft("")(_+_)
}

object PlaceGraph extends PlaceGraphFunctions{
  def apply[A](s: Stream[Tree[Site \/ A]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val forest = s
    override def toString = "<placeGraph>"
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Tree[Site \/ A]]] = Some(pg.forest)
}

trait PlaceGraphFunctions {
  implicit val s: Site = Open(0)
  def empty[A]: PlaceGraph[A] = PlaceGraph(Stream(Leaf(-\/(Closed))))
  def permutation[A]: PlaceGraph[A] = PlaceGraph(Stream(Leaf(-\/(Open(1))),Leaf(-\/(Open(0)))))
  //def join[A]: PlaceGraph[A] = ??? TODO - join doesnt work because a region cannot take two trees. must change the whole definition.
  def atom[A](n: => A): PlaceGraph[A] = PlaceGraph(Stream(Node(\/-(n), Stream(Leaf(-\/(Closed))))))
  def ion[A](n: => A)(implicit s: Site): PlaceGraph[A] = PlaceGraph(Stream(Node(\/-(n),Stream(Leaf(-\/(s))))))
  def juxtaposition[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = PlaceGraph(p0.forest #::: p1.forest)
  def composition[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = {
    PlaceGraph(
      p1.forest map ((t: Tree[Site \/ A]) => t flatMap ((n: Site \/ A) => n match {
        case -\/(Open(i)) => p0.forest(i)
        case _ => Leaf(n)
      })))
  }
}
