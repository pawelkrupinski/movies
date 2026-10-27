package services.identity
package adapters

import models.{CinemaMovie, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import tools.{HttpStatusException, MutableClock, TestWiring}

import scala.util.{Failure, Success}

/**
 * A take that flipped in production, resolved again at each side of the flip from the history alone
 * ([[IdentityHistoryReplay]]): the worker's model over its store, TMDB answering, a 429 leaving one record unfiled
 * while a second "Lalka" joins the search — then filed. At every moment the replay over the documents and listings
 * as they stood then takes what the live model took, the flip included; and the take record explains the flip by
 * the inputs that moved ([[TakeExplanation]]).
 */
class IdentityHistoryReplaySpec extends AnyFlatSpec with Matchers {
  private val language = "pl-PL"
  private val titles   = services.movies.SingleCountryNormalizer.titleNormalizer

  private final class World {
    val clock   = new MutableClock(TestWiring.FixedInstant)
    val history = new InMemoryIdentityHistory
    val docs    = new HistoryTmdbDocuments(new InMemoryTmdbDocuments, history, clock, "b1")
    val store   = new TmdbStore(docs, clock)
    val normalizer = new TmdbNormalizer(store, failures = new KeptFailures(history, clock, "b1"))
    val reads   = new ObservationReads
    var model   = Option.empty[IncrementalResolver]
    private val row = CinemaMovie(Movie("Lalka", releaseYear = Some(2025)), Multikino, None, Some("Multikino/Lalka"), None, Nil, Nil,
      Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 1, 18, 0), None)))
    val first: Seq[Listing] = Listing.distinct(Listing.all(Seq(Multikino -> Seq(row)), titles))
    val service = new IdentityModelService(
      () => { val m = new IncrementalResolver(new TrackedLookups(new StoredTmdbLookups(store, language, UnansweredTmdbLookups, reads), reads,
        Some(java.util.concurrent.Executors.newFixedThreadPool(2))), titles, IdentityCalibration.resolver); model = Some(m); m },
      reads, () => first, titles, scala.concurrent.duration.Duration(1, "second"), java.util.concurrent.Executors.newSingleThreadScheduledExecutor(),
      clock = clock, history = history, build = "b1")
    store.onChanged(service.observed)

    def search(results: (Int, Int, Double)*): Unit =
      normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=Lalka",
        Success(results.map { case (id, year, popularity) =>
          s"""{"id":$id,"title":"Lalka","original_title":"Lalka","release_date":"$year-01-01","popularity":$popularity}""" }.mkString("""{"results":[""", ",", "]}")))
    def record(id: Int, year: Int, director: String, outcome: Option[Throwable] = None): Unit = Seq(
      s"https://api.themoviedb.org/3/movie/$id?language=$language&append_to_response=credits,release_dates" ->
        s"""{"id":$id,"title":"Lalka","original_title":"Lalka","release_date":"$year-01-01","runtime":120,"popularity":5.0,"credits":{"crew":[{"job":"Director","name":"$director"}]}}""",
      s"https://api.themoviedb.org/3/movie/$id?language=en-US&append_to_response=alternative_titles" ->
        s"""{"id":$id,"title":"The Doll","popularity":5.0,"alternative_titles":{"titles":[]}}""")
      .foreach { case (url, body) => normalizer.filed("GET", url, outcome.fold[scala.util.Try[String]](Success(body))(Failure(_))) }

    def take(key: ListingKey): Option[Int] = model.flatMap(_.decisions.find(_.members.contains(key))).flatMap(_.film)
    /** The take at `at`, resolved again from the documents and listings as the history says they stood then. */
    def replayed(key: ListingKey, at: Long): Option[Int] = {
      val replay   = IdentityHistoryReplay.from(history, at)
      val lookups  = new StoredTmdbLookups(new TmdbStore(replay.documents(docs), clock), language, UnansweredTmdbLookups, ObservationReads.Untracked)
      IdentityResolver.resolve(replay.listings(model.get.listings), lookups, titles, ResolveInputs.of(Nil)).decisions.find(_.members.contains(key)).flatMap(_.film)
    }
    /** A drain a minute on, and the moment its takes stood at: what moves next moves after it. */
    def tick(): Long = {
      clock.advance(java.time.Duration.ofMinutes(1)); service.drain()
      val at = clock.millis(); clock.advance(java.time.Duration.ofSeconds(1)); at
    }
  }

  "a take that flipped while a record could not be read" should "be resolved again at each side of the flip as the worker took it" in {
    val w   = new World
    val key = w.first.head.key
    // 1: the search names Kawalski's 2025 "Lalka" and a 2024 one, both records filed: Kawalski's, by its year
    w.search((2, 2025, 12.4), (4, 2024, 3.5))
    w.record(2, 2025, "Maciej Kawalski"); w.record(4, 2024, "Someone Else")
    w.service.takeUp()
    val t1 = w.tick()
    val before = w.take(key)
    before shouldBe Some(2)
    // 2: Kawalski's record let go by the retention sweep, and read again — answered 429
    val stamp = TmdbStore.fetchedAt(w.docs.get(TmdbKind.Film, Seq("2"))("2")).get
    w.docs.deleteIfStill(TmdbKind.Film, Seq("2" -> stamp))
    w.record(2, 2025, "Maciej Kawalski", Some(new HttpStatusException(429, "GET", "https://api.themoviedb.org/3/movie/2", None)))
    w.service.observed(TmdbStore.keyOf(TmdbKind.Film, "2"))
    val t2 = w.tick()
    val interim = w.take(key)
    // 3: read again, filed: Kawalski's again
    w.record(2, 2025, "Maciej Kawalski")
    val t3 = w.tick()
    w.take(key) shouldBe before
    withClue("the 429 moved the take: the case has teeth") { interim should not be before }

    Seq(t1 -> before, t2 -> interim, t3 -> before).foreach { case (at, took) => w.replayed(key, at) shouldBe took }
    // the failure is kept, by its URL and what failed
    w.history.all.filter(_.kind == HistoryEntry.Failure).map(e => (e.id.contains("/movie/2"), e.failure)) should contain ((true, Some("http 429")))
  }

  "a listing that joined after the moment replayed" should "be left out of it, and one let go since be back in" in {
    val w = new World
    w.search((2, 2025, 12.4), (1, 1968, 3.5)); w.record(2, 2025, "Maciej Kawalski"); w.record(1, 1968, "Wojciech Has")
    w.service.takeUp()
    val t1 = w.tick()
    val muza = CinemaMovie(Movie("Lalka", releaseYear = Some(1968)), KinoMuza, None, Some("Muza/Lalka"), None, Nil, Nil,
      Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 2, 18, 0), None)))
    w.service.venueScraped(KinoMuza, Seq(muza))
    w.tick()
    w.service.venueScraped(Multikino, Nil)
    w.tick()
    val replay = IdentityHistoryReplay.from(w.history, t1)
    replay.listings(w.model.get.listings).map(_.key) shouldBe w.first.map(_.key)
  }
}
