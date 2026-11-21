package clients.enrichment

import clients.tools.FailingHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.HttpTmdbClient
/** A TMDB read that FAILED is not "TMDB has none". These five answered a blocked or broken
 *  request with an empty set: the director walk then found no film and concluded the row
 *  unresolvable, and Metacritic resolved with no director to tell same-titled films apart.
 *  Each now throws; the callers that deliberately treat "unknown" as "no evidence" say so. */
class TmdbClientFailedReadSpec extends AnyFlatSpec with Matchers {

  // 403: a real failure (a block), and not transient, so the client does not retry it.
  private val client = new HttpTmdbClient(new FailingHttpFetch(403), apiKey = Some(settings.TmdbApiKey("test-key")))

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

  // The rating sites probe under TMDB's year and English title: a failed details read answered None,
  // the sites searched without them, and the miss (or a page the year guard would have refused) sat in
  // their 24 h link cache.
  it should "throw, not answer none, when the details read fails, and answer none for an id TMDB does not know" in {
    an[Exception] should be thrownBy client.details(1)
    new HttpTmdbClient(new FailingHttpFetch(404), apiKey = Some(settings.TmdbApiKey("test-key"))).details(1) shouldBe None
  }
}
