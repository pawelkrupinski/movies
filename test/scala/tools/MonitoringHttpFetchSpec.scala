package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor

import java.io.IOException
import java.net.ConnectException

class MonitoringHttpFetchSpec extends AnyFlatSpec with Matchers {

  private class StubHttpFetch extends HttpFetch {
    var nextError: Option[Exception] = None
    def get(url: String): String = nextError.map(throw _).getOrElse("ok")
    def post(url: String, body: String, contentType: String): String = get(url)
  }

  private def fixture = {
    val monitor = new UptimeMonitor()
    val delegate = new StubHttpFetch
    val fetch = new MonitoringHttpFetch(delegate, monitor)
    (fetch, delegate, monitor)
  }

  "classify" should "map TMDB domain to TMDB" in {
    val (fetch, _, _) = fixture
    fetch.classify("https://api.themoviedb.org/3/search/movie?query=test") shouldBe Some("TMDB")
  }

  it should "map IMDb domains to IMDb" in {
    val (fetch, _, _) = fixture
    fetch.classify("https://caching.graphql.imdb.com/graphql") shouldBe Some("IMDb")
    fetch.classify("https://v3.sg.media-imdb.com/suggestion/a/test.json") shouldBe Some("IMDb")
  }

  it should "map Filmweb to Filmweb" in {
    val (fetch, _, _) = fixture
    fetch.classify("https://www.filmweb.pl/api/v1/film/123") shouldBe Some("Filmweb")
  }

  it should "skip cinema domains" in {
    val (fetch, _, _) = fixture
    fetch.classify("https://www.multikino.pl/api/repertoire") shouldBe None
    fetch.classify("https://helios.pl/api/showtimes") shouldBe None
    fetch.classify("https://www.cinema-city.pl/api/schedule") shouldBe None
    fetch.classify("https://www.kinomuza.pl/repertuar") shouldBe None
  }

  it should "use raw hostname for unknown domains" in {
    val (fetch, _, _) = fixture
    fetch.classify("https://api.example.com/test") shouldBe Some("api.example.com")
  }

  "isConnectionFailure" should "classify IOException as failure" in {
    val (fetch, _, _) = fixture
    fetch.isConnectionFailure(new IOException("Connection refused")) shouldBe true
  }

  it should "classify ConnectException as failure" in {
    val (fetch, _, _) = fixture
    fetch.isConnectionFailure(new ConnectException("Connection refused")) shouldBe true
  }

  it should "classify HTTP 5xx as failure" in {
    val (fetch, _, _) = fixture
    fetch.isConnectionFailure(new RuntimeException("HTTP 503 for GET https://example.com")) shouldBe true
  }

  it should "not classify HTTP 4xx as failure" in {
    val (fetch, _, _) = fixture
    fetch.isConnectionFailure(new RuntimeException("HTTP 404 for GET https://example.com")) shouldBe false
  }

  "get" should "record success for enrichment service calls" in {
    val (fetch, _, monitor) = fixture
    fetch.get("https://api.themoviedb.org/3/search/movie?query=test")
    monitor.history("TMDB").head.successes shouldBe 1
    monitor.history("TMDB").head.failures shouldBe 0
  }

  it should "record failure on connection error" in {
    val (fetch, delegate, monitor) = fixture
    delegate.nextError = Some(new IOException("Connection refused"))
    intercept[IOException] { fetch.get("https://api.themoviedb.org/3/search") }
    monitor.history("TMDB").head.failures shouldBe 1
  }

  it should "record success on HTTP 404 (service reachable)" in {
    val (fetch, delegate, monitor) = fixture
    delegate.nextError = Some(new RuntimeException("HTTP 404 for GET https://api.themoviedb.org/x"))
    intercept[RuntimeException] { fetch.get("https://api.themoviedb.org/x") }
    monitor.history("TMDB").head.successes shouldBe 1
    monitor.history("TMDB").head.failures shouldBe 0
  }

  it should "record failure on HTTP 503" in {
    val (fetch, delegate, monitor) = fixture
    delegate.nextError = Some(new RuntimeException("HTTP 503 for GET https://www.filmweb.pl/x"))
    intercept[RuntimeException] { fetch.get("https://www.filmweb.pl/x") }
    monitor.history("Filmweb").head.failures shouldBe 1
  }

  it should "not record cinema domain calls" in {
    val (fetch, _, monitor) = fixture
    fetch.get("https://www.multikino.pl/api/repertoire")
    monitor.services shouldBe empty
  }
}
