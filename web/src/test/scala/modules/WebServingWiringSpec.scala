package modules

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}
import services.readmodel.{InMemoryReadModelRepository, ReadModelProjection, ReadModelReader}
import play.api.test.Helpers.stubControllerComponents

/**
 * Guards the read/write split invariant: **the serving app cannot and does not
 * scrape**. The scrape + enrichment engine lives in the separate `worker`
 * module and is deliberately absent from `web`'s classpath; `web` keeps its
 * cache warm purely from Mongo (boot hydrate + change stream). Two tripwires
 * encode that:
 *
 *   1. The enrichment classes aren't even loadable here — if a refactor ever
 *      drags the worker engine onto `web`'s classpath, `Class.forName` stops
 *      throwing and these tests fail.
 *   2. Booting the wiring with an empty data layer leaves the cache empty:
 *      nothing in the serving process fills it by scraping. It only ever holds
 *      what the repository (Mongo, here an in-memory stand-in) already has.
 */
class WebServingWiringSpec extends AnyFlatSpec with Matchers {

  // ── 1. Classpath boundary ────────────────────────────────────────────────
  // These three classes ARE the scrape/enrich engine (`ScrapeReaper` drives the
  // queue-based cinema scrape scheduling; `MovieService` owns the TMDB/IMDb
  // worker pool; `FilmwebRatings` is one of the rating enrichers). They live in
  // `worker`, not on `web`'s classpath — so resolving them by name must fail. A
  // passing `Class.forName` would mean the engine had leaked back into the
  // serving app.

  "The serving app's classpath" should "not contain the ScrapeReaper scrape scheduler" in {
    an [ClassNotFoundException] should be thrownBy Class.forName("services.tasks.ScrapeReaper")
  }

  it should "not contain the MovieService enrichment worker pool" in {
    an [ClassNotFoundException] should be thrownBy Class.forName("services.movies.MovieService")
  }

  it should "not contain the FilmwebRatings enricher" in {
    an [ClassNotFoundException] should be thrownBy Class.forName("services.enrichment.FilmwebRatings")
  }

  // ── 2. Warm-cache-without-scrape ─────────────────────────────────────────
  // A minimal `Wiring` wired against a DISABLED Mongo + an in-memory repository, so
  // `boot()` exercises the real start path (`mongoConnection.database`,
  // `movieCache.start()`) without touching a cluster. Whatever the cache holds
  // after boot came from the repository alone — there is no scrape path to add more.

  private class TestWiring(seed: Seq[(String, Option[Int], MovieRecord)]) extends Wiring {
    // A connection with no URI never dials Mongo; `required = false` keeps the
    // disabled state a silent no-op rather than a boot failure.
    override lazy val mongoConnection: MongoConnection =
      new MongoConnection(uri = None, dbName = "kinowo", required = false)
    override lazy val movieRepository = new InMemoryMovieRepository(seed)
    // The serving read path is `webReadModel` over `readModelRepository` (the
    // worker-populated web_movies / web_screenings). Seed that — projected
    // through the real `ReadModelProjection`, exactly as the worker writes it —
    // so boot's hydrate has the same single fill path production does.
    override lazy val readModelRepository: ReadModelReader = {
      val store = new InMemoryReadModelRepository()
      seed.foreach { case (title, year, record) =>
        val stored = StoredMovieRecord(title, year, record)
        store.upsertMovie(ReadModelProjection.resolve(stored))
        ReadModelProjection.screenings(stored).foreach(store.upsertScreening)
      }
      store
    }

    val controllerComponents = stubControllerComponents()
    def environmentMode       = play.api.Mode.Test
    implicit def materializer: org.apache.pekko.stream.Materializer = null

    /** Expose the protected data-layer start so the test can drive it. */
    def boot(): Unit = start()
  }

  private val seededRecord = MovieRecord(tmdbId = Some(42))

  "The serving cache" should "hold exactly the rows already in the repository after boot" in {
    // Seed one row, boot, and assert the cache surfaces it — proving boot
    // hydrates from the repository (the only fill path the serving app has).
    val wiring = new TestWiring(Seq(("Drzewo Magii", Some(2024), seededRecord)))
    wiring.boot()

    val movies = wiring.webReadModel.allMovies()
    movies.map(_.title) should contain ("Drzewo Magii")
  }

  it should "stay empty when the repository is empty — nothing scrapes to fill it" in {
    // Empty repository → boot → empty cache. If the serving process had any scrape
    // path wired in, this is where it would surface a row out of nowhere. It
    // doesn't, so the cache stays empty.
    val wiring = new TestWiring(Seq.empty)
    wiring.boot()

    wiring.webReadModel.allMovies() shouldBe empty
  }

  // ── 3. /debug movie-mirror connection selection ──────────────────────────────
  // `movieRepository` reads the `movies` corpus over a local mirror when
  // MONGODB_MOVIES_MIRROR_URI is set, else the shared prod connection. The mirror
  // is used UNCONDITIONALLY when configured — there is no fall-back to the prod
  // tunnel, even when the mirror is unreachable (a previous version fell back to
  // prod whenever the mirror's `database` was absent).
  //
  // A `uri = None` MongoConnection never dials Mongo, so it stands in for both an
  // "unreachable mirror" (its `database` is None, as a failed probe leaves it) and
  // the prod connection — distinct instances we can identity-check.
  private def stubConnection(label: String): MongoConnection =
    new MongoConnection(uri = None, dbName = label, required = false)

  "Wiring.movieConnection" should
    "use the configured local mirror unconditionally — no prod fall-back even when unreachable" in {
    val mirror = stubConnection("mirror") // database = None, i.e. an unreachable mirror
    val prod   = stubConnection("prod")
    val chosen = Wiring.movieConnection(Some("mongodb://127.0.0.1:9/x"), _ => mirror, prod)
    (chosen eq mirror) shouldBe true
  }

  it should "use the shared prod connection when no mirror URI is set" in {
    val prod = stubConnection("prod")
    val chosen = Wiring.movieConnection(None, _ => fail("must not open a mirror when the URI is unset"), prod)
    (chosen eq prod) shouldBe true
  }

  it should "not force the prod connection when a mirror URI is configured" in {
    var prodForced = false
    Wiring.movieConnection(Some("mongodb://127.0.0.1:9/x"), _ => stubConnection("mirror"),
      { prodForced = true; stubConnection("prod") })
    prodForced shouldBe false
  }
}
