package services.observations

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.ListingKey
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeAttempt, ScrapeOutcome}
import tools.{HttpFetch, HttpStatusException, MutableClock}

import java.time.Instant

/**
 * The shadow capture is one decorator per seam — every external lookup (`ObservingHttpFetch`),
 * every venue detail (`ObservingDetailEnricher`), every scraped listing (`ObservingScrapeArchive`)
 * — and each must be INVISIBLE: the same answer or the same exception reaches the caller whether
 * capture is on or off, and a store that fails never fails the call it observes.
 */
class ObservationCaptureSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-09-26T10:00:00Z")

  private object Upstream extends HttpFetch {
    override def get(url: String): String =
      if (url.contains("missing")) throw new HttpStatusException(404, "GET", url, None)
      else if (url.contains("down")) throw new java.net.SocketTimeoutException("slow")
      else s"body of $url"
    override def getBytes(url: String): Array[Byte] = Array[Byte](1, 2, -3)
    override def post(url: String, body: String, contentType: String): String = s"answer to $body"
  }

  /** A store whose storage is gone: every write throws. */
  private def brokenStore = new ObservationStore(new InMemoryObservationBackend, new InMemoryObservationBackend {
    override def insert(observation: StoredObservation): Unit = throw new IllegalStateException("mongo down")
  }, new MutableClock(t0))

  "ObservingHttpFetch" should "hand back exactly what the service answered, and keep it as an observation" in {
    val store = ObservationStore.inMemory(new MutableClock(t0))
    val fetch = new ObservingHttpFetch(Upstream, store)
    fetch.get("https://api.themoviedb.org/3/search/movie?query=X&api_key=K") shouldBe
      "body of https://api.themoviedb.org/3/search/movie?query=X&api_key=K"
    fetch.get("https://www.imdb.com/title/tt1/", Map("User-Agent" -> "x")) shouldBe "body of https://www.imdb.com/title/tt1/"
    fetch.getBytes("https://img/p.jpg").toSeq shouldBe Seq[Byte](1, 2, -3)
    fetch.post("https://api.graphql.imdb.com/", "{q}", "application/json") shouldBe "answer to {q}"

    store.lookup(LookupQuery.of("GET", "https://api.themoviedb.org/3/search/movie?query=X&api_key=K")).map(_.answer) shouldBe
      Some(LookupAnswer.Body("body of https://api.themoviedb.org/3/search/movie?query=X&api_key=K"))
    store.lookup(LookupQuery.of("GET", "https://www.imdb.com/title/tt1/")).isDefined shouldBe true
    store.lookup(LookupQuery.of("BYTES", "https://img/p.jpg")).map(_.answer) shouldBe Some(LookupAnswer.ofBytes(Array[Byte](1, 2, -3)))
    store.lookup(LookupQuery.of("POST", "https://api.graphql.imdb.com/", Some("{q}"))).map(_.answer) shouldBe
      Some(LookupAnswer.Body("answer to {q}"))
  }

  it should "rethrow the very failure the service raised, and keep the failure as an observation" in {
    val store = ObservationStore.inMemory(new MutableClock(t0))
    val fetch = new ObservingHttpFetch(Upstream, store)
    val miss  = intercept[HttpStatusException](fetch.get("https://x/missing"))
    miss.code shouldBe 404
    intercept[java.net.SocketTimeoutException](fetch.get("https://x/down"))
    store.lookup(LookupQuery.of("GET", "https://x/missing")).map(_.answer) should matchPattern {
      case Some(LookupAnswer.Failed(Some(404), "GET", _)) => }
    store.lookup(LookupQuery.of("GET", "https://x/down")).map(_.answer.definitive) shouldBe Some(false)
  }

  it should "never fail the call it observes when the store cannot write" in {
    val fetch = new ObservingHttpFetch(Upstream, brokenStore)
    fetch.get("https://x/ok") shouldBe "body of https://x/ok"
    intercept[HttpStatusException](fetch.get("https://x/missing")).code shouldBe 404
  }

  private final class Venue extends DetailEnricher {
    val cinema: Cinema = KinoMuza
    val detailGroup    = "muza"
    override def detailTarget: Source = Multikino
    override def enrichmentServiceOverride: Option[String] = Some("chain")
    override def defersTmdbResolution: Boolean = false
    def fetchFilmDetail(ref: String): Option[FilmDetail] =
      if (ref.endsWith("/gone")) throw new HttpStatusException(410, "GET", ref, None)
      else if (ref.endsWith("/empty")) None
      else Some(FilmDetail(director = Seq("Agnès Varda"), releaseYear = Some(1962), runtimeMinutes = Some(90)))
  }

  "ObservingDetailEnricher" should "be the enricher it wraps in every respect, and keep each detail it fetched" in {
    val store   = ObservationStore.inMemory(new MutableClock(t0))
    val venue   = new Venue
    val wrapped = new ObservingDetailEnricher(venue, store)
    (wrapped.cinema, wrapped.detailGroup, wrapped.detailTarget, wrapped.enrichmentServiceOverride,
      wrapped.defersTmdbResolution) shouldBe
      ((venue.cinema, venue.detailGroup, venue.detailTarget, venue.enrichmentServiceOverride, venue.defersTmdbResolution))

    wrapped.fetchFilmDetail("https://muza/cleo") shouldBe venue.fetchFilmDetail("https://muza/cleo")
    wrapped.fetchFilmDetail("https://muza/empty") shouldBe None
    intercept[HttpStatusException](wrapped.fetchFilmDetail("https://muza/gone")).code shouldBe 410

    ObservingDetailEnricher.detail(store, KinoMuza, "https://muza/cleo") shouldBe Some(Right(venue.fetchFilmDetail("https://muza/cleo")))
    ObservingDetailEnricher.detail(store, KinoMuza, "https://muza/empty") shouldBe Some(Right(None))
    ObservingDetailEnricher.detail(store, KinoMuza, "https://muza/gone") should matchPattern {
      case Some(Left(LookupAnswer.Failed(Some(410), _, _))) => }
    ObservingDetailEnricher.detail(store, KinoMuza, "https://muza/never-asked") shouldBe None
  }

  "ObservingScrapeArchive" should "archive exactly what the archive it wraps would, and keep every listing of a scrape with content" in {
    val clock    = new MutableClock(t0)
    val store    = ObservationStore.inMemory(clock)
    val plain    = new InMemoryScrapeArchiveRepository
    val inner    = new InMemoryScrapeArchiveRepository
    val observed = new ObservingScrapeArchive(inner, store)
    val films    = Seq(
      CinemaMovie(Movie("Cléo de 5 à 7", releaseYear = Some(1962)), KinoMuza, None, Some("https://muza/cleo"), None, Nil, Nil, Nil),
      CinemaMovie(Movie("Sinn und Sinnlichkeit", releaseYear = Some(1995)), KinoMuza, None, None, None, Nil, Seq("Ang Lee"), Nil))
    val attempts = Seq(
      ScrapeAttempt(KinoMuza, Some("Poznań"), t0, listingComplete = true, films),
      ScrapeAttempt(KinoMuza, Some("Poznań"), t0.plusSeconds(60), listingComplete = true, Nil),
      ScrapeAttempt(KinoMuza, Some("Poznań"), t0.plusSeconds(120), listingComplete = false, Nil, Some("boom")))
    attempts.foreach { a => plain.record(a); observed.record(a) }

    observed.findAll() shouldBe plain.findAll()
    observed.find(KinoMuza) shouldBe plain.find(KinoMuza)
    observed.lastContentAt() shouldBe plain.lastContentAt()
    observed.enabled shouldBe inner.enabled
    observed.find(KinoMuza).map(_.outcome) shouldBe Some(ScrapeOutcome.Failed)

    store.currentListings().map(_.key).toSet shouldBe films.map(ListingKey.of(KinoMuza, _)).toSet
  }

  it should "never fail the scrape it observes when the store cannot write" in {
    val inner    = new InMemoryScrapeArchiveRepository
    val observed = new ObservingScrapeArchive(inner, new ObservationStore(new InMemoryObservationBackend {
      override def insert(observation: StoredObservation): Unit = throw new IllegalStateException("mongo down")
    }, new InMemoryObservationBackend, new MutableClock(t0)))
    val film = CinemaMovie(Movie("Cléo de 5 à 7"), KinoMuza, None, None, None, Nil, Nil, Nil)
    observed.record(ScrapeAttempt(KinoMuza, None, t0, listingComplete = true, Seq(film)))
    inner.find(KinoMuza).map(_.films) shouldBe Some(Seq(film))
  }
}
