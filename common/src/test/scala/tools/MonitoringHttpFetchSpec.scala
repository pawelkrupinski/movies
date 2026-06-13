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

  // Cinema-host suppression is INJECTED now (the worker derives it from
  // CinemaScraperCatalog.scrapeHosts), not hardcoded. Tests that exercise
  // suppression pass the set explicitly; the rest use the empty default.
  private def fixture(cinemaHosts: Set[String] = Set.empty) = {
    val monitor = new UptimeMonitor()
    val delegate = new StubHttpFetch
    val fetch = new MonitoringHttpFetch(delegate, monitor, cinemaHosts)
    (fetch, delegate, monitor)
  }

  "classify" should "map TMDB domain to TMDB" in {
    val (fetch, _, _) = fixture()
    fetch.classify("https://api.themoviedb.org/3/search/movie?query=test") shouldBe Some("TMDB")
  }

  it should "map IMDb domains to IMDb" in {
    val (fetch, _, _) = fixture()
    fetch.classify("https://caching.graphql.imdb.com/graphql") shouldBe Some("IMDb")
    fetch.classify("https://v3.sg.media-imdb.com/suggestion/a/test.json") shouldBe Some("IMDb")
  }

  it should "map Filmweb to Filmweb" in {
    val (fetch, _, _) = fixture()
    fetch.classify("https://www.filmweb.pl/api/v1/film/123") shouldBe Some("Filmweb")
  }

  it should "suppress hosts in the injected cinema-host set" in {
    val (fetch, _, _) = fixture(Set("www.multikino.pl", "restapi.helios.pl", "kinomuranow.pl"))
    fetch.classify("https://www.multikino.pl/api/repertoire") shouldBe None
    fetch.classify("https://restapi.helios.pl/api/showtimes") shouldBe None
    fetch.classify("https://kinomuranow.pl/repertuar/") shouldBe None
  }

  it should "record a cinema host that is NOT in the suppressed set (default empty)" in {
    // Suppression is opt-in via the injected set; without it the host is a
    // plain catch-all row. This is what makes a NEW cinema leak into "Other"
    // until its scraper declares the host — the bug this change fixes upstream.
    val (fetch, _, _) = fixture()
    fetch.classify("https://kinomuranow.pl/repertuar/") shouldBe Some("kinomuranow.pl")
  }

  it should "let a named enrichment row win over cinema-host suppression" in {
    // filmweb.pl is both scraped (FilmwebShowtimesClient) and read for ratings;
    // its enrichment health is what the page wants, so enrichment beats suppress.
    val (fetch, _, _) = fixture(Set("www.filmweb.pl"))
    fetch.classify("https://www.filmweb.pl/api/v1/cinema/2305/seances") shouldBe Some("Filmweb")
  }

  it should "use raw hostname for unknown domains" in {
    val (fetch, _, _) = fixture()
    fetch.classify("https://api.example.com/test") shouldBe Some("api.example.com")
  }

  it should "force the by-name cinemaHosts thunk only once across many calls" in {
    var evaluations = 0
    val monitor = new UptimeMonitor()
    val fetch = new MonitoringHttpFetch(new StubHttpFetch, monitor, {
      evaluations += 1; Set("kinomuranow.pl")
    })
    fetch.classify("https://kinomuranow.pl/a")
    fetch.classify("https://kinomuranow.pl/b")
    fetch.classify("https://api.example.com/c")
    evaluations shouldBe 1
  }

  "isConnectionFailure" should "classify IOException as failure" in {
    val (fetch, _, _) = fixture()
    fetch.isConnectionFailure(new IOException("Connection refused")) shouldBe true
  }

  it should "classify ConnectException as failure" in {
    val (fetch, _, _) = fixture()
    fetch.isConnectionFailure(new ConnectException("Connection refused")) shouldBe true
  }

  it should "classify HTTP 5xx as failure" in {
    val (fetch, _, _) = fixture()
    fetch.isConnectionFailure(new RuntimeException("HTTP 503 for GET https://example.com")) shouldBe true
  }

  it should "not classify HTTP 4xx as failure" in {
    val (fetch, _, _) = fixture()
    fetch.isConnectionFailure(new RuntimeException("HTTP 404 for GET https://example.com")) shouldBe false
  }

  "get" should "record success for enrichment service calls" in {
    val (fetch, _, monitor) = fixture()
    fetch.get("https://api.themoviedb.org/3/search/movie?query=test")
    monitor.history("TMDB").head.successes shouldBe 1
    monitor.history("TMDB").head.failures shouldBe 0
  }

  it should "record failure with error message on connection error" in {
    val (fetch, delegate, monitor) = fixture()
    delegate.nextError = Some(new IOException("Connection refused"))
    intercept[IOException] { fetch.get("https://api.themoviedb.org/3/search") }
    val bucket = monitor.history("TMDB").head
    bucket.failures shouldBe 1
    bucket.errors.head should include ("Connection refused")
  }

  it should "record success on HTTP 404 (service reachable)" in {
    val (fetch, delegate, monitor) = fixture()
    delegate.nextError = Some(new RuntimeException("HTTP 404 for GET https://api.themoviedb.org/x"))
    intercept[RuntimeException] { fetch.get("https://api.themoviedb.org/x") }
    monitor.history("TMDB").head.successes shouldBe 1
    monitor.history("TMDB").head.failures shouldBe 0
    monitor.history("TMDB").head.errors shouldBe empty
  }

  it should "record failure with error message on HTTP 503" in {
    val (fetch, delegate, monitor) = fixture()
    delegate.nextError = Some(new RuntimeException("HTTP 503 for GET https://www.filmweb.pl/x"))
    intercept[RuntimeException] { fetch.get("https://www.filmweb.pl/x") }
    val bucket = monitor.history("Filmweb").head
    bucket.failures shouldBe 1
    bucket.errors.head should include ("HTTP 503")
  }

  it should "not record calls to a suppressed cinema host" in {
    val (fetch, _, monitor) = fixture(Set("www.multikino.pl"))
    fetch.get("https://www.multikino.pl/api/repertoire")
    monitor.services shouldBe empty
  }

  // The charset-mojibake regression: a legacy single-byte site (Kino Pod
  // Baranami / Charlie serve raw ISO-8859-2 with no charset declaration) is
  // fetched through MonitoringHttpFetch(RealHttpFetch) in prod. The client calls
  // `getBytes` to decode with the right charset. If the wrapper inherits the
  // lossy base default (`get(url).getBytes(UTF_8)`), the wire bytes get
  // UTF-8-decoded → re-encoded → "ż" becomes "ďż˝". getBytes MUST pass the
  // delegate's RAW bytes straight through.
  "getBytes" should "pass the delegate's raw wire bytes through, not re-encode via get" in {
    // "Dzień" in ISO-8859-2 (ń = 0xF1) — a UTF-8 decode of these bytes is lossy.
    val rawIso88592 = Array[Byte](0x44, 0x7A, 0x69, 0x65, 0xF1.toByte)
    val monitor  = new UptimeMonitor()
    val delegate = new HttpFetch {
      def get(url: String): String = "MANGLED"  // would lose the byte if re-encoded
      override def getBytes(url: String): Array[Byte] = rawIso88592
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val fetch = new MonitoringHttpFetch(delegate, monitor)
    fetch.getBytes("https://bilety.example.pl/repertuar") shouldBe rawIso88592
  }

  it should "record getBytes outcomes on the monitor like get does" in {
    val (fetch, delegate, monitor) = fixture()
    delegate.nextError = Some(new IOException("Connection refused"))
    intercept[IOException] { fetch.getBytes("https://api.themoviedb.org/3/x") }
    monitor.history("TMDB").head.failures shouldBe 1
  }
}
