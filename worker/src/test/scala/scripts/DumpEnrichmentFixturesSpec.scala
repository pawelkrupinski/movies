package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.CachedResponse

import java.nio.file.Paths

/**
 * The dump is only useful if the path it writes is the path `FakeHttpFetch` looks
 * for. Both sides derive it independently — the cache from `METHOD <masked url>`,
 * the reader from the live URL — so this pins that they agree.
 */
class DumpEnrichmentFixturesSpec extends AnyFlatSpec with Matchers {

  private val root = Paths.get("test", "resources", "fixtures", "enrichment-pl")

  "the dumped path" should "match what FakeHttpFetch derives from the live URL" in {
    // What the cache stored (credentials masked by RedactedUrl)...
    val key = "GET https://api.themoviedb.org/3/search/movie?query=Cicha+noc&api_key=***"
    val dumped = DumpEnrichmentFixtures.pathFor(root, key).get

    // ...and what the reader will look for, given the REAL url.
    val fingerprint = clients.tools.RecordingHttpFetch.stableQueryFingerprint(
      "query=Cicha+noc&api_key=realsecret", foldYear = false)

    dumped.toString shouldBe root.resolve(s"api.themoviedb.org/3/search/movie.$fingerprint").toString
  }

  it should "separate the year-scoped search from the yearless one" in {
    val yearless = DumpEnrichmentFixtures.pathFor(root,
      "GET https://api.themoviedb.org/3/search/movie?query=Cicha+noc&api_key=***").get
    val scoped = DumpEnrichmentFixtures.pathFor(root,
      "GET https://api.themoviedb.org/3/search/movie?query=Cicha+noc&year=2026&api_key=***").get

    withClue("folding the year here would collapse 0-results onto 16-results: ") {
      yearless should not be scoped
    }
  }

  // IMDb's GraphQL calls all share one URL; the body hash is what tells them apart,
  // and the cache stores the same hash the recorder appends.
  it should "carry a POST body hash through to the filename" in {
    val path = DumpEnrichmentFixtures.pathFor(root, "POST https://caching.graphql.imdb.com/ 1a2b3c").get
    path.toString should endWith (".1a2b3c")
  }

  it should "skip a key it cannot parse rather than guess" in {
    DumpEnrichmentFixtures.pathFor(root, "GARBAGE") shouldBe None
    DumpEnrichmentFixtures.pathFor(root, "GET not-a-url") shouldBe None
  }

  // A cached failure has no body. Writing one would fabricate an answer the service
  // never gave; leaving it out lets the live leg re-ask and re-decide.
  it should "have no body for a cached failure" in {
    DumpEnrichmentFixtures.bodyOf(CachedResponse.Failed(Some(404), "GET", "HTTP 404")) shouldBe None
    DumpEnrichmentFixtures.bodyOf(CachedResponse.Body("hi")).map(new String(_)) shouldBe Some("hi")
  }
}
