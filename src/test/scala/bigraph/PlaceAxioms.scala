package bigraph

import org.scalatest.FunSuite

import scalaz._
import Scalaz._
/**
  * Created by eloipereira on 8/18/16.
  */
class PlaceAxioms extends FunSuite{ //FIXME - "===" from scalaz collides with "===" of scalatest. implicit conversions are not applicable because they are ambiguous
  test("Place Axiom 1: "){
    assert((Join compose Permute) === Join)
  }
  test("Place Axiom 2: "){
    assert((Join compose (Unit juxtapose PlaceId(1))) === PlaceId(1))
  }
  test("Place Axiom 3: "){
    assert( (Join compose (Join juxtapose PlaceId(1))) === (Join compose (PlaceId(1) juxtapose Join)))
  }
}