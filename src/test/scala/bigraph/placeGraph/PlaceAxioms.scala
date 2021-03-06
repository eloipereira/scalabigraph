package bigraph.placeGraph

import org.scalatest.{FunSuite}

import scalaz.Equal

import scalaz._, Scalaz._

/**
  * Created by eloipereira on 8/18/16.
  */
class PlaceAxioms extends FunSuite{
  test("Place Axiom 1: "){
    val a = Join compose Permute(1,1)
    val b = Join
    assert(Equal[PlaceGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
  test("Place Axiom 2: "){
    val a = Join compose (PlaceUnit juxtapose PlaceId(1))
    val b = PlaceId(1)
    assert(Equal[PlaceGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
  test("Place Axiom 3: "){
    val a = Join compose (Join juxtapose PlaceId(1))
    val b = Join compose (PlaceId(1) juxtapose Join)
    assert(Equal[PlaceGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
}