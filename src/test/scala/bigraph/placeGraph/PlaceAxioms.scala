package bigraph.placeGraph

import org.scalatest.FunSuite

import scalaz.Equal
/**
  * Created by eloipereira on 8/18/16.
  */
class PlaceAxioms extends FunSuite{
  test("Place Axiom 1: "){
    val a = Join compose Permute
    val b = Join
    assert(Equal[PlaceGraph[Nothing]].equal(a,b))
  }
  test("Place Axiom 2: "){
    val a = Join compose (PlaceUnit juxtapose PlaceId(1))
    val b = PlaceId(1)
    assert(Equal[PlaceGraph[Nothing]].equal(a,b))
  }
  test("Place Axiom 3: "){
    val a = Join compose (Join juxtapose PlaceId(1))
    val b = Join compose (PlaceId(1) juxtapose Join)
    assert(Equal[PlaceGraph[Nothing]].equal(a,b))
  }
}