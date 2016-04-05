import scalaz._
import scalaz.Tree._
import Scalaz._


sealed trait Region{
  def id: Int
}

object Region{
  def region(i: => Int): Region = new Region{
    lazy val id = i
  }
}

sealed trait Site{
  def id: Int
}

object Site{
  def site(i: => Int): Site = new Site{
    lazy val id = i
  }
}

sealed trait PlaceGraph[A]{
  def region: Region
  def graph: Stream[Tree[A \/ Site]]
}

object PlaceGraph extends PlaceGraphFunctions{
  def apply[A](n: => A): PlaceGraph[A] = ion(n)
}

trait PlaceGraphFunctions {
  def place[A](r: Region, forest: => Stream[Tree[A \/ Site]]): PlaceGraph[A] = new PlaceGraph[A]{
    lazy val region = r
    lazy val graph = forest
    override def toString = "<placeGraph>"
  }
  implicit val r: Region = Region.region(0)
  def ion[A](n: => A)(implicit r: Region): PlaceGraph[A] = place(r,Stream(Leaf(n.left)))
}
