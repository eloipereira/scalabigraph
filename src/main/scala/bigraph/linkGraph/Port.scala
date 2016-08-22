package bigraph.linkGraph

import scalaz.Equal._
import scalaz.{Equal, Order}
import scalaz.Order._

/**
  * Created by eloipereira on 8/22/16.
  */
case class Port[+A](n: A, id: Int)

object Port{
  implicit def equalById[A]: Equal[Port[A]] = new Equal[Port[A]] {
    override def equal(a1: Port[A], a2: Port[A]): Boolean = (a1.n == a2.n) && (a1.id == a1.id)
  }
}