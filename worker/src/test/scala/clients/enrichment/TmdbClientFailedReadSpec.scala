package clients.enrichment

import clients.tools.FailingHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.TmdbClient

/** A TMDB read that FAILED is not "TMDB has none". These five answered a blocked or broken
 *  request with an empty set: the director walk then found no film and concluded the row
 *  unresolvable, and Metacritic resolved with no director to tell same-titled films apart.
 *  Each now throws; the callers that deliberately treat "unknown" as "no evidence" say so. */
class TmdbClientFailedReadSpec extends AnyFlatSpec with Matchers {

  // 403: a real failure (a block), and not transient, so the client does not retry it.
  private val client = new TmdbClient(new FailingHttpFetch(403), apiKey = Some("test-key"))

  "TmdbClient" should "throw, not answer empty, when the credits read fails" in {
    an[Exception] should be thrownBy client.crewIds(1)
    an[Exception] should be thrownBy client.directorsFor(1)
  }

  it should "throw, not answer empty, when a person search or filmography read fails" in {
    an[Exception] should be thrownBy client.findPersonCandidates("Denis Villeneuve")
    an[Exception] should be thrownBy client.personDirectorCredits(137427)
    an[Exception] should be thrownBy client.personWriterCredits(137427)
  }

  it should "throw, not answer empty, when the images read fails" in {
    an[Exception] should be thrownBy client.posters(1)
  }
}
