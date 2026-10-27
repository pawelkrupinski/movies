package services.scrapes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RealHttpFetch

import scala.util.Try

/** The real JDK HTTP client's failure on a name that does not resolve, read as closure by [[OwnSiteEvidence]]; the rest of its rules are [[OwnSiteEvidenceSpec]]. */
class OwnSiteEvidenceIntegrationSpec extends AnyFlatSpec with Matchers {

  // What production actually throws: the JDK HttpClient reports a name that does not
  // resolve as a ConnectException CAUSED BY UnresolvedAddressException, never an
  // UnknownHostException. `.invalid` is reserved never to resolve (RFC 6761).
  "A venue's own site" should "show closure when the real HTTP client cannot resolve its host" in {
    val dead = "http://kino-closed.invalid/"
    OwnSiteEvidence.read(dead, Try(new RealHttpFetch().get(dead))).observed should
      (include ("does not resolve") and include ("kino-closed.invalid"))
  }
}
