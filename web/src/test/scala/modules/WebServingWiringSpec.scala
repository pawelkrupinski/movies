package modules

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.movies.{InMemoryMovieRepo, StoredMovieRecord}
import services.readmodel.{InMemoryReadModelRepo, ReadModelProjection, ReadModelReader}
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
 *      what the repo (Mongo, here an in-memory stand-in) already has.
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
  // A minimal `Wiring` wired against a DISABLED Mongo + an in-memory repo, so
  // `boot()` exercises the real start path (`mongoConnection.database`,
  // `movieCache.start()`) without touching a cluster. Whatever the cache holds
  // after boot came from the repo alone — there is no scrape path to add more.

  private class TestWiring(seed: Seq[(String, Option[Int], MovieRecord)]) extends Wiring {
    // A connection with no URI never dials Mongo; `required = false` keeps the
    // disabled state a silent no-op rather than a boot failure.
    override lazy val mongoConnection: MongoConnection =
      new MongoConnection(uri = None, dbName = "kinowo", required = false)
    override lazy val movieRepo = new InMemoryMovieRepo(seed)
    // The serving read path is `webReadModel` over `readModelRepo` (the
    // worker-populated web_movies / web_screenings). Seed that — projected
    // through the real `ReadModelProjection`, exactly as the worker writes it —
    // so boot's hydrate has the same single fill path production does.
    override lazy val readModelRepo: ReadModelReader = {
      val store = new InMemoryReadModelRepo()
      seed.foreach { case (title, year, rec) =>
        val stored = StoredMovieRecord(title, year, rec)
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

  "The serving cache" should "hold exactly the rows already in the repo after boot" in {
    // Seed one row, boot, and assert the cache surfaces it — proving boot
    // hydrates from the repo (the only fill path the serving app has).
    val wiring = new TestWiring(Seq(("Drzewo Magii", Some(2024), seededRecord)))
    wiring.boot()

    val movies = wiring.webReadModel.allMovies()
    movies.map(_.title) should contain ("Drzewo Magii")
  }

  it should "stay empty when the repo is empty — nothing scrapes to fill it" in {
    // Empty repo → boot → empty cache. If the serving process had any scrape
    // path wired in, this is where it would surface a row out of nowhere. It
    // doesn't, so the cache stays empty.
    val wiring = new TestWiring(Seq.empty)
    wiring.boot()

    wiring.webReadModel.allMovies() shouldBe empty
  }
}
