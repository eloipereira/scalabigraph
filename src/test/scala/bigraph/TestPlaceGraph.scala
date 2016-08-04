package bigraph
import org.scalacheck.Properties
import org.scalacheck._
import Prop.forAll


trait PlaceGraphGenerator {
  val ints = Gen.choose(-100, 100)

  def unit: Gen[PlaceGraph[Int]] = Gen.const(Unit)

  def ions: Gen[PlaceGraph[Int]] = for{
    x <- ints
  } yield Ion(x)

  def nestings: Gen[PlaceGraph[Int]] = for {
    x <- ints
    i <- ions
    r <- regions
  } yield (i <> r)

  def siblings: Gen[PlaceGraph[Int]] = for {
    r0 <- nestings
    r1 <- regions
  } yield (r0 | r1)

  def regions: Gen[PlaceGraph[Int]] = Gen.oneOf(unit,ions,siblings)

  def juxtaposes: Gen[PlaceGraph[Int]] = for {
    r0 <- regions
    r1 <- places
  } yield (r0 || r1)

  def places: Gen[PlaceGraph[Int]] = Gen.oneOf(regions, juxtaposes)

}

 object PlaceGraphSpecification extends Properties("PlaceGraph") with PlaceGraphGenerator{
   property("composition innerFace") = forAll(places,places) { (p0:PlaceGraph[Int],p1:PlaceGraph[Int]) =>
     if (p0.compose.isDefinedAt(p1)) {
       (p0 compose p1).innerFace == p1.innerFace
     } else true
   }
 }
