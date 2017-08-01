package bigraph

import placeGraph._
import PlaceGraph._
import scalaz._
import Scalaz._

object Example0 extends App{
    val p0: PlaceGraph[String] = PlaceIon("Hello") <> (PlaceIon("Bigraphical") | PlaceIon("World"))
    p0.println
    println(placeGraphToTerm(p0))

    val p1: PlaceGraph[String] = PlaceIon("Hello") | PlaceIon("World")
    p1.println
    println(placeGraphToTerm(p1))

    val p2: PlaceGraph[String] = PlaceIon("Hello") || PlaceIon("World")
    p2.println
    println(placeGraphToTerm(p2))


}
