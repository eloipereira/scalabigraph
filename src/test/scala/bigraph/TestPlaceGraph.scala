package bigraph
import org.scalacheck._
import Prop.forAll

val ints = Gen.choose(-100, 100)

def ions: Gen[Ion] = for{
  x <- ints
} yield Ion(x)




// object PlaceGraphSpecification extends Properties("PlaceGraph"){
//   property("composition innerFace") = forAll{ (p0:PlaceGraph[Int],p1:PlaceGraph[Int]) =>
//     (p0 compose p1).get.innerFace == p1.innerFace
//   }
// }
