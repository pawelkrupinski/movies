package clients.enrichment

import clients.TmdbClient
import modules.wiring.HttpWiring
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HttpFetch, HttpStatusException, InMemoryFleetHostPace, MutableClock, RetryingHttpFetch}

import java.time.Instant
import scala.concurrent.duration._

/**
 * TMDB asked through the chain production asks it through (`HttpWiring.pacedWire`): a transient failure is retried
 * there, on the spot ([[RetryingHttpFetch]]), and a 429 is waited out by the gate (`ThrottledHttpFetch`) — the client
 * itself retries nothing, so a 5xx costs [[RetryingHttpFetch.Attempts]] requests, not that many squared. A 404, TMDB's
 * "no such film", fails at once.
 */
class TmdbClientWireSpec extends AnyFlatSpec with Matchers {

  private val ExternalIds = """{"imdb_id":"tt1375666"}"""

  /** A fake wire whose Nth request is decided by `onCall(N)` — return a body, or throw. */
  private class FakeWire(onCall: Int => String) extends HttpFetch {
    var calls = 0
    override def get(url: String): String = get(url, Map.empty)
    override def get(url: String, headers: Map[String, String]): String = { calls += 1; onCall(calls) }
    override def post(url: String, body: String, contentType: String): String =
      throw new UnsupportedOperationException("TmdbClient never POSTs")
  }
  private def status(code: Int, retryAfter: Option[FiniteDuration] = None) =
    new HttpStatusException(code, "GET", "https://api.themoviedb.org/3/movie/1/external_ids", retryAfter)

  /** A TMDB client over the production chain on `wire`; a wait moves the clock instead of the thread. */
  private def client(wire: HttpFetch) = {
    val clock = new MutableClock(Instant.parse("2026-10-24T00:00:00Z"))
    val chain = HttpWiring.pacedWire(wire, new settings.ProcessConfiguration(tools.Env.of()), new InMemoryFleetHostPace,
      clock, ms => clock.advanceMillis(ms))
    new TmdbClient(chain, apiKey = Some(settings.TmdbApiKey("test-key")))
  }

  "TmdbClient" should "have a transient TMDB 500 asked again by the chain, and succeed on the next try" in {
    val wire = new FakeWire(n => if (n == 1) throw status(500) else ExternalIds)
    client(wire).imdbId(27205) shouldBe Some("tt1375666")
    wire.calls shouldBe 2
  }

  it should "have a request timeout asked again by the chain" in {
    val wire = new FakeWire(n => if (n == 1) throw new java.net.http.HttpTimeoutException("request timed out") else ExternalIds)
    client(wire).imdbId(27205) shouldBe Some("tt1375666")
    wire.calls shouldBe 2
  }

  it should "send a 5xx that persists the chain's tries only, not a retry of its own around them" in {
    val wire = new FakeWire(_ => throw status(500))
    a[HttpStatusException] should be thrownBy client(wire).imdbId(1)
    wire.calls shouldBe RetryingHttpFetch.Attempts
  }

  it should "NOT retry a 404 — TMDB's no-such-film fails at once" in {
    val wire = new FakeWire(_ => throw status(404))
    a[HttpStatusException] should be thrownBy client(wire).imdbId(1)
    wire.calls shouldBe 1
  }

  it should "NOT retry a FileNotFoundException — a fixture-replay miss is permanent, not transient" in {
    val wire = new FakeWire(_ => throw new java.io.FileNotFoundException("fixtures/.../images.content"))
    a[java.io.FileNotFoundException] should be thrownBy client(wire).imdbId(1)
    wire.calls shouldBe 1
  }

  it should "wait out a 429 at the gate and read the answer after it" in {
    val wire = new FakeWire(n => if (n == 1) throw status(429, Some(2.seconds)) else ExternalIds)
    client(wire).imdbId(27205) shouldBe Some("tt1375666")
    wire.calls shouldBe 2
  }

  // ── external_ids: capture wikidata_id alongside imdb_id ─────────────────────

  it should "parse both imdb_id and wikidata_id from one /external_ids call" in {
    val wire = new FakeWire(_ => """{"id":603,"imdb_id":"tt0133093","wikidata_id":"Q83495"}""")
    client(wire).externalIds(603) shouldBe TmdbClient.ExternalIds(Some("tt0133093"), Some("Q83495"))
    wire.calls shouldBe 1 // both ids from the SAME round-trip
  }

  it should "leave wikidata_id None when TMDB records no Wikidata cross-reference" in {
    val wire = new FakeWire(_ => """{"id":1,"imdb_id":"tt0000001"}""")
    client(wire).externalIds(1) shouldBe TmdbClient.ExternalIds(Some("tt0000001"), None)
  }
}
