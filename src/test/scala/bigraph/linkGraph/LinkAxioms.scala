package bigraph.linkGraph

import org.scalatest.{FunSuite}

import scalaz.Equal

/**
  * Created by eloipereira on 8/22/16.
  */
class LinkAxioms extends FunSuite{
  test("Link Axiom 1: "){
    val a = Substitution(Stream('x),'x).asInstanceOf[LinkGraph[Nothing]]
    val b = LinkId(Some(Stream('x))).asInstanceOf[LinkGraph[Nothing]]
    assert(Equal[LinkGraph[Nothing]].equal(a,b))
  }
  test("Link Axiom 2: "){
    val a = Closure('x) compose Substitution(Stream(), 'x)
    val b = LinkId(None)
    assert(Equal[LinkGraph[Nothing]].equal(a,b))
  }
  test("Link Axiom 3: "){
    val a = Closure('y) compose Substitution(Stream('x), 'y)
    val b = Closure('x)
    assert(Equal[LinkGraph[Nothing]].equal(a,b))
  }
  test("Link Axiom 4: "){
    val a = Substitution(Stream('Y, 'y), 'z) compose (LinkId(Some(Stream('Y))) juxtapose Substitution(Stream('X), 'y))
    val b = Substitution(Stream('X, 'Y), 'z)
    assert(Equal[LinkGraph[Nothing]].equal(a,b))
  }
}
