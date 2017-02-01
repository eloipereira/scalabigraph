import scalaz._
import Scalaz._
import bigraph._
import placeGraph._
import linkGraph._
import PlaceGraph._
import LinkGraph._

val c: LinkGraph[Any] = Closure('x)

c.show

val ids: LinkGraph[Any] = LinkId(Stream('x,'y,'z))

ids.show

val subs: LinkGraph[Any] = Substitution(Stream('x,'y,'z),'w)

subs.show

subs.linkOuterFace

subs.linkInnerFace

val linkIon: LinkGraph[String]
  = LinkIon("Elef",Map(0 -> Some('x), 1 -> Some('y)))

linkIon.show

val renames: LinkGraph[String] = Renaming(Map('x -> 'z, 'y -> 'w))

renames.show

val linkIon2: LinkGraph[String] = renames compose linkIon


val b: LinkGraph[String] = LinkIon("Elef",Map(0 -> Some('z), 1 -> Some('w)))

b.hypergraph
linkIon2.hypergraph

b === linkIon2
