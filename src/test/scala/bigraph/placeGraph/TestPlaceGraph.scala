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
    if i.compose.isDefinedAt(r)
  } yield i <> r

  def siblings: Gen[PlaceGraph[Int]] = for {
    r0 <- nestings
    r1 <- regions
    if r0.juxtapose.isDefinedAt(r1)
  } yield r0 | r1

  def regions: Gen[PlaceGraph[Int]] = Gen.oneOf(unit,ions,siblings)

  def juxtaposes: Gen[PlaceGraph[Int]] = for {
    r0 <- regions
    r1 <- places
    if r0.juxtapose.isDefinedAt(r1)
  } yield r0 || r1

  def places: Gen[PlaceGraph[Int]] = Gen.oneOf(regions, juxtaposes)

}

 object PlaceGraphSpecification extends Properties("PlaceGraph") with PlaceGraphGenerator{
   property("COMPOSITION") =
     forAll(places,places) { (p0:PlaceGraph[Int],p1:PlaceGraph[Int]) =>
       (p0.compose.isDefinedAt(p1) && p0 != PlaceUnit) ==> {
         val p = p0 <> p1
         ("support of #1 disjoint of support of #2" |: (p0.support.intersect(p1.support) == Set.empty)) &&
         ("inner face of #1 == outer face of #2" |: (p0.placeInnerFace == p1.placeOuterFace)) &&
         ("inner face of result == inner face of #2" |: (p.placeInnerFace == p1.placeInnerFace)) &&
         ("outer face of result == outer face of #1" |: (p.placeOuterFace == p0.placeOuterFace))
     }
   }
 }

