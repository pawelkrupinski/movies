package services.identity

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The resolver's TMDB lookups over RECORDED answers: a film's identity record is read by the
 *  calibration's own parser from the two answers the client already asks for it, and a request
 *  the recording cannot answer is `Unknown`, never "no film". */
class TmdbIdentityLookupsSpec extends AnyFlatSpec with Matchers {

  private val fetch   = new FakeHttpFetch("08-06-2026", strict = true)
  private val tmdb    = new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("replay")), retrySleep = (_: Long) => ())
  private val lookups = new TmdbIdentityLookups(tmdb, new services.enrichment.ImdbClient(fetch), Nil)

  "a film's identity record" should "carry what the calibrated measures read: title, year, runtime, directors, countries" in {
    val film = lookups.film(1018).toOption.flatten.get
    film.title should not be empty
    film.year shouldBe defined
    film.runtime shouldBe defined
    film.directors.get should not be empty
    film.countries.get.foreach(_ should fullyMatch regex "[A-Z]{2}")
  }

  it should "be Unknown when the recording holds no answer for it" in {
    lookups.film(999999999) shouldBe Answer.Unknown
  }

  "a title search" should "keep TMDB's own result order, the order the calibration fitted `search.rank` on" in {
    // A recorded answer whose order is NOT popularity order (946306 first, at popularity 0.04).
    val body = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(
      "test/resources/fixtures/08-06-2026/api.themoviedb.org/3/search/movie.f25d5a92")), java.nio.charset.StandardCharsets.UTF_8)
    val recorded = (play.api.libs.json.Json.parse(body) \ "results").as[Seq[play.api.libs.json.JsObject]].map(r => (r \ "id").as[Int])
    val client = new TmdbClient(tools.RoutingHttpFetch.getOnly(Seq("/search/movie" -> body)), apiKey = Some(settings.TmdbApiKey("replay")))
    val hits = new TmdbIdentityLookups(client, new services.enrichment.ImdbClient(tools.RoutingHttpFetch.dead("imdb")), Nil).candidates(CandidateQuery.Title("anything")).toOption.get
    hits.map(_.tmdbId) shouldBe recorded
    hits.head.tmdbId shouldBe 946306
  }

  "the films IMDb lists under a title" should "be the TMDB records of IMDb's own entries of that very title, by their IMDb ids" in {
    // Recording 36224654409 (US): IMDb's suggestions for "Caligula: The Ultimate Cut" list the
    // re-cut (tt29703523) and the 1979 film; only the re-cut is titled so, and TMDB's find by its
    // id is the record its search never returns.
    def fixture(path: String) = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path)), java.nio.charset.StandardCharsets.UTF_8)
    val fetch = tools.RoutingHttpFetch.getOnly(Seq(
      "suggestion/c/Caligula" -> fixture("test/resources/fixtures/imdb/suggestion_caligula_the_ultimate_cut.json"),
      "/find/tt29703523"      -> fixture("test/resources/fixtures/tmdb/find_tt29703523.json")))
    val client = new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("replay")), retrySleep = (_: Long) => ())
    val hits = new TmdbIdentityLookups(client, new services.enrichment.ImdbClient(fetch), Nil)
      .candidates(CandidateQuery.Imdb("Caligula: The Ultimate Cut")).toOption.get
    hits.map(h => (h.tmdbId, h.title, h.year)) shouldBe Seq((1774981, "Caligula: The Ultimate Cut", Some(2024)))
    fetch.calls.map(_._2).filter(_.contains("/find/")) should have size 1
  }

  "lookups over a live source" should "read side by side, so a prefetch's threads overlap their round-trips" in {
    val body = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(
      "test/resources/fixtures/08-06-2026/api.themoviedb.org/3/search/movie.f25d5a92")), java.nio.charset.StandardCharsets.UTF_8)
    // Each read lingers until another is in flight (or a second passes): lookups that took turns never overlap.
    val inFlight = new java.util.concurrent.atomic.AtomicInteger
    val overlap  = new java.util.concurrent.atomic.AtomicInteger
    val fetch    = new tools.HttpFetch {
      def get(url: String): String = {
        overlap.accumulateAndGet(inFlight.incrementAndGet(), math.max)
        val until = System.nanoTime() + 1_000_000_000L
        while (inFlight.get < 2 && System.nanoTime() < until) Thread.onSpinWait()
        overlap.accumulateAndGet(inFlight.get, math.max)
        inFlight.decrementAndGet()
        body
      }
      def post(url: String, body: String, contentType: String): String = throw new java.io.IOException("no posts")
    }
    val lookups = new TmdbIdentityLookups(new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("replay")), retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(tools.RoutingHttpFetch.dead("imdb")), Nil)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(2)
    try Seq("one", "two").map(title => pool.submit(() => lookups.candidates(CandidateQuery.Title(title)))).foreach(_.get(20, java.util.concurrent.TimeUnit.SECONDS))
    finally pool.shutdownNow()
    overlap.get shouldBe 2
  }
}
