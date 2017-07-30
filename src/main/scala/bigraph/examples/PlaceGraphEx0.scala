package bigraph

import placeGraph._
import PlaceGraph._
import scalaz._
import Scalaz._

object Example0 extends App{
    val p0: PlaceGraph[String] = PlaceIon("Hello") <> PlaceIon("World")
    val p1: PlaceGraph[String] = PlaceIon("Hello") | PlaceIon("World")
    val p2: PlaceGraph[String] = PlaceIon("Hello") || PlaceIon("World")
    p0.println
    p1.println
    p2.println
}
