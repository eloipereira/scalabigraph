package bigraph.placeGraph

import scalaz._
import Scalaz._
import scalaz.Order._
import scalaz.Equal._

/**
  * Created by eloipereira on 8/22/16.
  */
case class Site(id: Int) {
  override def toString = "$" + id
}

object Site {
  implicit def orderById: Order[Site] = orderBy((_:Site).id)
  implicit def orderingById = orderById.toScalaOrdering
  implicit def equalById: Equal[Site] = equalBy((_:Site).id)
}