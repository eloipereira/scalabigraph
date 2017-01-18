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