package bigraph

import placeGraph._
import PlaceGraph._
import bigraph._
import Bigraph._
import scalaz._
import Scalaz._

object Example0 extends App{
    val p0: PlaceGraph[String] = PlaceIon("Hello") <> (PlaceIon("Bigraphical") | PlaceIon("World"))
    p0.println

    val p1: PlaceGraph[String] = PlaceIon("Hello") | PlaceIon("World")
    p1.println

    val p2: PlaceGraph[String] = PlaceIon("Hello") || PlaceIon("World")
    p2.println

    val b0: Bigraph[String] = Ion("Hello", Stream(Some('x), None, Some('y)))
    b0.println
}
