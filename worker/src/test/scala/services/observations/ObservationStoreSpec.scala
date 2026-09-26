package services.observations

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import tools.{HttpStatusException, MutableClock}

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration._

/** The in-memory backend under the store's rules. */
class ObservationStoreSpec extends ObservationStoreBehaviour {
  protected def newStore(clock: MutableClock): ObservationStore = ObservationStore.inMemory(clock)
}

/**
 * The observation store's rules — what counts as a NEW observation, what a transient failure may
 * not overwrite, when anything expires — live in [[ObservationStore]], above the storage seam.
 * Every backend runs the same cases: `ObservationStoreSpec` (in memory) and
 * `MongoObservationStoreIntegrationSpec` (Mongo).
 */
trait ObservationStoreBehaviour extends AnyFlatSpec with Matchers {

  /** A store over EMPTY storage. */
  protected def newStore(clock: MutableClock): ObservationStore

  private val t0 = Instant.parse("2026-09-26T10:00:00Z")

  private def advance(clock: MutableClock, d: FiniteDuration): Unit = clock.advance(java.time.Duration.ofMillis(d.toMillis))

  private def store(clock: MutableClock) = newStore(clock)

  private val search = LookupQuery.of("GET", "https://api.themoviedb.org/3/search/movie?query=Belle&api_key=SECRET")

  private def listing(title: String, year: Option[Int], page: Option[String], showtimes: Int = 1): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year), KinoMuza, None, page, None, Nil, Seq("Mamoru Hosoda"),
      (1 to showtimes).map(d => Showtime(LocalDateTime.of(2026, 9, 26 + d % 3, 18, 0), None)))

  "a lookup query" should "be the credential-masked request, so a key never reaches the store" in {
    search.key should not include "SECRET"
    search.host shouldBe "api.themoviedb.org"
    LookupQuery.of("POST", "https://api.graphql.imdb.com/", Some("{a}")) should not be
      LookupQuery.of("POST", "https://api.graphql.imdb.com/", Some("{b}"))
  }

  it should "be identity evidence exactly when it is a venue's detail or a TMDB request" in {
    search.isIdentityEvidence shouldBe true
    LookupQuery.venueDetail("Kino Muza", "/film/belle").isIdentityEvidence shouldBe true
    LookupQuery.of("GET", "https://www.metacritic.com/movie/belle/").isIdentityEvidence shouldBe false
    LookupQuery.of("GET", "https://www.rottentomatoes.com/m/belle_2021").isIdentityEvidence shouldBe false
    LookupQuery.of("POST", "https://caching.graphql.imdb.com/", Some("{a}")).isIdentityEvidence shouldBe false
    // A poster is TMDB's too, but not its API: no identity question is answered by an image.
    LookupQuery.of("BYTES", "https://image.tmdb.org/t/p/w500/belle.jpg").isIdentityEvidence shouldBe false
  }

  "a lookup observation" should "be kept once per distinct answer, stamped with when it was fetched" in {
    val clock = new MutableClock(t0)
    val s     = store(clock)
    s.observeLookup(search, LookupAnswer.Body("""{"results":[]}"""))
    advance(clock, 1.hour)
    s.observeLookup(search, LookupAnswer.Body("""{"results":[]}"""))
    s.lookupHistory(search).map(_.fetchedAt) shouldBe Seq(t0)
    s.lookup(search).map(_.lastFetchedAt) shouldBe Some(t0.plusSeconds(3600))

    advance(clock, 1.hour)
    s.observeLookup(search, LookupAnswer.Body("""{"results":[{"id":1}]}"""))
    s.lookupHistory(search).map(_.answer) shouldBe Seq(
      LookupAnswer.Body("""{"results":[]}"""), LookupAnswer.Body("""{"results":[{"id":1}]}"""))
    s.lookup(search).map(_.answer) shouldBe Some(LookupAnswer.Body("""{"results":[{"id":1}]}"""))
  }

  it should "never let a transient failure supersede a definitive answer — a failed read is not data" in {
    val clock = new MutableClock(t0)
    val s     = store(clock)
    s.observeLookup(search, LookupAnswer.Body("ok"))
    s.observeLookup(search, LookupAnswer.failureOf(new HttpStatusException(503, "GET", "u", None), "GET"))
    s.observeLookup(search, LookupAnswer.failureOf(new java.net.SocketTimeoutException("slow"), "GET"))
    s.lookup(search).map(_.answer) shouldBe Some(LookupAnswer.Body("ok"))

    // …but a definitive miss does: a 404 is the service's answer.
    s.observeLookup(search, LookupAnswer.failureOf(new HttpStatusException(404, "GET", "u", None), "GET"))
    s.lookup(search).map(_.answer.definitive) shouldBe Some(true)
    s.lookup(search).map(_.answer) should matchPattern { case Some(LookupAnswer.Failed(Some(404), _, _)) => }
  }

  it should "record a transient failure when nothing better is known, and let the first real answer replace it" in {
    val s = store(new MutableClock(t0))
    s.observeLookup(search, LookupAnswer.failureOf(new HttpStatusException(429, "GET", "u", None), "GET"))
    s.lookup(search).map(_.answer.definitive) shouldBe Some(false)
    s.observeLookup(search, LookupAnswer.Body("ok"))
    s.lookup(search).map(_.answer) shouldBe Some(LookupAnswer.Body("ok"))
  }

  it should "round-trip bytes exactly" in {
    val s     = store(new MutableClock(t0))
    val bytes = Array[Byte](0, -1, 7, 42)
    val q     = LookupQuery.of("BYTES", "https://example.org/poster.jpg")
    s.observeLookup(q, LookupAnswer.ofBytes(bytes))
    s.lookup(q).map(_.answer) match {
      case Some(b: LookupAnswer.Bytes) => b.bytes.toSeq shouldBe bytes.toSeq
      case other                       => fail(s"expected bytes, got $other")
    }
  }

  "a listing observation" should "be keyed by ListingKey and be the listing's evidence, not its showtimes" in {
    val clock = new MutableClock(t0)
    val s     = store(clock)
    val belle = listing("Belle", Some(2021), Some("https://kinomuza.pl/belle"), showtimes = 3)
    s.observeListing(KinoMuza, belle)
    advance(clock, 1.day)
    // The next day's scrape: same film, different showtimes — not a new observation.
    s.observeListing(KinoMuza, belle.copy(showtimes = belle.showtimes.drop(1)))
    val key = ListingKey.of(KinoMuza, belle)
    s.listingHistory(key).map(_.observedAt) shouldBe Seq(t0)
    s.listing(key).map(_.listing.movie.title) shouldBe Some("Belle")
    s.listing(key).map(_.listing.showtimes) shouldBe Some(Nil)

    // The venue corrects its evidence under the same page: a new observation of the same key.
    advance(clock, 1.hour)
    s.observeListing(KinoMuza, belle.copy(movie = belle.movie.copy(runtimeMinutes = Some(121))))
    s.listingHistory(key).map(_.listing.movie.runtimeMinutes) shouldBe Seq(None, Some(121))
  }

  it should "keep two page-less listings of one title apart by what the venue published" in {
    val s = store(new MutableClock(t0))
    s.observeListing(KinoMuza, listing("Sinn und Sinnlichkeit", Some(1995), None))
    s.observeListing(KinoMuza, listing("Sinn und Sinnlichkeit", Some(2026), None))
    s.currentListings().map(_.listing.movie.releaseYear).sorted shouldBe Seq(Some(1995), Some(2026))
  }

  "retention" should "expire a key once it has been neither observed nor used for the window, and superseded versions a window after they were replaced" in {
    val clock  = new MutableClock(t0)
    val s      = store(clock)
    val window = ObservationRetention.Window
    val other  = LookupQuery.of("GET", "https://www.imdb.com/title/tt1/")
    s.observeLookup(search, LookupAnswer.Body("v1"))
    s.observeLookup(other, LookupAnswer.Body("x"))
    advance(clock, window / 2)
    s.observeLookup(search, LookupAnswer.Body("v2"))
    advance(clock, window / 2 + 1.minute)
    // `other` was last seen a window ago; `search`'s v1 was superseded half a window ago.
    s.lookup(other) shouldBe None
    s.lookupHistory(search).map(_.answer) shouldBe Seq(LookupAnswer.Body("v1"), LookupAnswer.Body("v2"))

    // A read RENEWS: a lookup the resolver still reads is live whether or not anyone re-fetched it.
    s.lookup(search).isDefined shouldBe true
    advance(clock, window - 1.minute)
    s.lookupHistory(search).map(_.answer) shouldBe Seq(LookupAnswer.Body("v2"))
    s.lookup(search).map(_.answer) shouldBe Some(LookupAnswer.Body("v2"))
  }

  it should "be derived from the pipeline's own longest re-ask period, not chosen" in {
    ObservationRetention.Window shouldBe (ObservationRetention.LongestReaskPeriod * 2)
    ObservationRetention.LongestReaskPeriod should be >= services.cadence.RatingCadence.MaxInterval
  }
}
