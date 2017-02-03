package bigraph.linkGraph

import org.scalatest.{FunSuite}

import scalaz.Equal

import scalaz._, Scalaz._

/**
  * Created by eloipereira on 8/22/16.
  */
class LinkAxioms extends FunSuite{
  test("Link Axiom 1: "){
    val a = Substitution(Stream('x),'x)
    val b = LinkId(Stream('x))
    assert(Equal[LinkGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
  test("Link Axiom 2: "){
    val a = Closure('x) compose Substitution(Stream(), 'x)
    val b = LinkId(Stream())
    assert(Equal[LinkGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
  test("Link Axiom 3: "){
    val a = Closure('y) compose Substitution(Stream('x), 'y)
    val b = Closure('x)
    assert(Equal[LinkGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
  test("Link Axiom 4: "){
    val a = Substitution(Stream('Y, 'y), 'z) compose (LinkId(Stream('Y)) juxtapose Substitution(Stream('X), 'y))
    val b = Substitution(Stream('X, 'Y), 'z)
    assert(Equal[LinkGraph[Int]].equal(a,b)) //FIX - should be Nothing and not Int
  }
  test("Node Axiom: "){
    val a = Renaming(Map('x -> 'z, 'y -> 'w)) compose LinkIon("Elef",Map(0 -> Some('x), 1 -> Some('y)))
    val b = LinkIon("Elef",Map(0 -> Some('z), 1 -> Some('w)))
    assert(Equal[LinkGraph[String]].equal(a,b))
  }
}
