package bigraph

import scalaz._

sealed trait LinkGraph[A]{

}

case class Edge[A](id: A)

case class InnerName(id: String)

case class OuterName(id: String)