package modules


import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
  // `TestWebWiring`: the real `Wiring` over a DISABLED Mongo + an in-memory repository, so
  // `boot()` exercises the real start path (`mongoConnection.database`,
  // `movieCache.start()`) without touching a cluster. Whatever the cache holds
  // after boot came from the repository alone — there is no scrape path to add more.

  private val seededRecord = MovieRecord(tmdbId = Some(42))

  "The serving cache" should "hold exactly the rows already in the repository after boot" in {
    // Seed one row, boot, and assert the cache surfaces it — proving boot
    // hydrates from the repository (the only fill path the serving app has).
    val wiring = new TestWebWiring(Seq(("Drzewo Magii", Some(2024), seededRecord)))
    wiring.boot()

    val movies = wiring.webReadModel.allMovies()
    movies.map(_.title) should contain ("Drzewo Magii")
  }

  it should "stay empty when the repository is empty — nothing scrapes to fill it" in {
    // Empty repository → boot → empty cache. If the serving process had any scrape
    // path wired in, this is where it would surface a row out of nowhere. It
    // doesn't, so the cache stays empty.
    val wiring = new TestWebWiring(Seq.empty)
    wiring.boot()

    wiring.webReadModel.allMovies() shouldBe empty
  }
}
