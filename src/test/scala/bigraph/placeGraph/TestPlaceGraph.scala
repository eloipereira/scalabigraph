package bigraph.placeGraph

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Properties, _}


trait PlaceGraphGenerator {
  val ints = Gen.choose(-100, 100)

  def unit: Gen[PlaceGraph[Int]] = Gen.const(PlaceUnit)

  def ions: Gen[PlaceGraph[Int]] = for{
    x <- ints
  } yield PlaceIon(x)

  def nestings: Gen[PlaceGraph[Int]] = for {
    x <- ints
    i <- ions
    r <- regions
  } yield i <> r

  def siblings: Gen[PlaceGraph[Int]] = for {
    r0 <- nestings
    r1 <- regions
  } yield r0 | r1

  def regions: Gen[PlaceGraph[Int]] = Gen.oneOf(unit,ions,siblings)

  def juxtaposes: Gen[PlaceGraph[Int]] = for {
    r0 <- regions
    r1 <- places
  } yield r0 || r1

  def places: Gen[PlaceGraph[Int]] = Gen.oneOf(regions, juxtaposes)

}

 object PlaceGraphSpecification extends Properties("PlaceGraph") with PlaceGraphGenerator{
   property("composition innerFace") = forAll(places,places) { (p0:PlaceGraph[Int],p1:PlaceGraph[Int]) =>
     p0.compose.isDefinedAt(p1) ==> {
       val p = p0 compose p1
       ("inner face of result == inner face of #2" |: (p.placeInnerFace == p1.placeInnerFace)) &&
         ("outer face of result == outer face of #1" |: (p.placeOuterFace == p0.placeOuterFace))
     }
   }
 }

