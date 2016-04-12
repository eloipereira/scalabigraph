package bigraph

import scalaz._
import scalaz.Tree._
import scalaz.std.stream.{streamInstance, streamMonoid}

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

sealed trait PlaceGraph[A]{
  def forest: Stream[Tree[Site \/ A]]
  //def drawPlaceGraph: String = forest.map(_.drawTree).foldLeft("")(_+_)
}

object PlaceGraph extends PlaceGraphFunctions{
  def apply[A](s: Stream[Tree[Site \/ A]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val forest = s
    override def toString = "<placeGraph>"
  }
  def unapply[A](pg: PlaceGraph[A]): Option[Stream[Tree[Site \/ A]]] = Some(pg.forest)
}

trait PlaceGraphFunctions {
  implicit val s: Site = Site(0)
  def empty[A]: PlaceGraph[A] = PlaceGraph(Stream.empty)
  def atom[A](n: => A): PlaceGraph[A] = PlaceGraph(Stream(Leaf(\/-(n))))
  def ion[A](n: => A)(implicit s: Site): PlaceGraph[A] = PlaceGraph(Stream(Node(\/-(n),Stream(Leaf(-\/(s))))))
  def juxtaposition[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = PlaceGraph(p0.forest #::: p1.forest)
  def composition[A](p0: PlaceGraph[A], p1: PlaceGraph[A]): PlaceGraph[A] = {
    PlaceGraph(
      p1.forest map ((t: Tree[Site \/ A]) => t flatMap ((n: Site \/ A) => n match {
        case -\/(Site(i)) => p0.forest(i)
        case _ => Leaf(n)
      }
      )
    ))
  }
}
