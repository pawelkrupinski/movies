package clients.enrichment

import clients.TmdbClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HttpFetch, HttpStatusException}

/**
 * TMDB's API 5xxs and times out for a few minutes now and then — it took down a
 * CI integration run on `/movie/{id}/external_ids`. `TmdbClient` routes every call
 * through a bounded retry-with-backoff so a transient blip is absorbed, while a
 * permanent 4xx (a 404 for an id TMDB doesn't know) still fails fast.
 */
class TmdbClientRetrySpec extends AnyFlatSpec with Matchers {

  private val ExternalIds = """{"imdb_id":"tt1375666"}"""

  /** A fake fetch whose Nth call is decided by `onCall(N)` — return a body, or throw. */
  private class FakeHttp(onCall: Int => String) extends HttpFetch {
    var calls = 0
    override def get(url: String): String = get(url, Map.empty)
    override def get(url: String, headers: Map[String, String]): String = { calls += 1; onCall(calls) }
    override def post(url: String, body: String, contentType: String): String =
      throw new UnsupportedOperationException("TmdbClient never POSTs")
  }
  private def status(code: Int) =
    new HttpStatusException(code, "GET", "https://api.themoviedb.org/3/movie/1/external_ids", None)
  private def client(http: HttpFetch) = new TmdbClient(http, apiKey = Some("test-key"))

  "TmdbClient" should "retry a transient TMDB 500 and succeed on the next attempt" in {
    val http = new FakeHttp(n => if (n == 1) throw status(500) else ExternalIds)
    client(http).imdbId(27205) shouldBe Some("tt1375666")
    http.calls shouldBe 2 // failed once, retried, succeeded
  }

  it should "retry a request timeout (HttpTimeoutException is an IOException)" in {
    val http = new FakeHttp(n =>
      if (n == 1) throw new java.net.http.HttpTimeoutException("request timed out") else ExternalIds)
    client(http).imdbId(27205) shouldBe Some("tt1375666")
    http.calls shouldBe 2
  }

  it should "give up after the attempt budget when TMDB stays down (rethrows the last failure)" in {
    val http = new FakeHttp(_ => throw status(503))
    a[HttpStatusException] should be thrownBy client(http).imdbId(1)
    http.calls shouldBe 3 // maxAttempts — no more
  }

  it should "NOT retry a 404 — a permanent no-match fails fast without burning attempts" in {
    val http = new FakeHttp(_ => throw status(404))
    a[HttpStatusException] should be thrownBy client(http).imdbId(1)
    http.calls shouldBe 1 // fired once, not retried
  }

  it should "NOT retry a FileNotFoundException — a fixture-replay miss is permanent, not transient" in {
    // FakeHttpFetch throws this on a missing fixture. It's an IOException, but a
    // PERMANENT miss — retrying it stormed the e2e replay harness with thousands of
    // backoff sleeps. Prod's RealHttpFetch never throws it.
    val http = new FakeHttp(_ => throw new java.io.FileNotFoundException("fixtures/.../images.content"))
    a[java.io.FileNotFoundException] should be thrownBy client(http).imdbId(1)
    http.calls shouldBe 1 // fired once, not retried
  }
}
