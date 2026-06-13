package integration

import models._
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaScraper, KinoPalacoweClient}
import tools.TestWiring

/**
 * Real-network smoke for every cinema scraper. Sources the scrapers from
 * production `Wiring` (via [[TestWiring]] — same wiring, just with
 * controllerComponents / environmentMode stubs) so this spec exercises the
 * exact construction graph the running app uses. A wiring-level regression
 * (wrong fetch passed to a cinema, Zyte routing dropped, …) lands
 * here as a real 401/403/timeout against the live cinema API.
 *
 * One explicit `it should "fetch films"` per cinema so each is individually
 * click-runnable from the IDE.
 *
 * Per-cinema runtime strictness:
 *   - `requireRuntime`  — every entry must have a runtime (default).
 *   - `runtimeOptional` — Cinema City + Kino Bułgarska legitimately list a
 *     few entries without a runtime (CC: `length: null` / "Czas
 *     niepotwierdzony" on upcoming films; KB: "pokazy przedpremierowe").
 *     Require ≥ 2/3.
 *   - `runtimeNotParsed` — Kino Apollo's scraped layout doesn't expose
 *     runtime at all, so only the base-shape (non-empty + at least one
 *     showtime) is checked.
 */
object ClientIntegrationSpec {
  // Singleton: `ParallelTestExecution` mixes in `OneInstancePerTest`, so a
  // class-level `val wiring = new TestWiring {}` would build one Wiring per
  // test — 10 simultaneous MongoMovieRepo connections, hydrations, etc.
  // We just want one production-shaped graph shared across all parallel
  // network probes.
  private val wiring = new TestWiring {}
}

class ClientIntegrationSpec
  extends AnyFlatSpec
    with Matchers
    with ParallelTestExecution {

  import ClientIntegrationSpec.wiring

  "Multikino"                should "fetch films" in requireRuntime(Multikino)
  "Kino Malta Charlie Monroe" should "fetch films" in requireRuntime(CharlieMonroe)
  // Pałacowe scrapes BARE: the listing exposes runtime for only a handful of
  // films (JSON `duration`, or the `lead` prose for accessibility screenings);
  // ~90% carry it ONLY on the detail page. Production fills it via the deferred
  // EnrichDetails task, so assert the FINAL enriched result the way prod serves
  // it — `fetch()` then `fetchFilmDetail` per film — and require EVERY film to end
  // up with a runtime. (Uses the production client directly: the wired scraper is
  // wrapped by FilmwebFallbackScraper, which doesn't expose the detail fetch.)
  "Kino Pałacowe" should "expose a runtime for every film after detail enrichment" in {
    val client = new KinoPalacoweClient(wiring.httoFetch)
    RetryWithBackoff() {
      val films = client.fetch()
      withClue("Kino Pałacowe (after detail): ") {
        assertBaseShape(films)
        films.foreach { cm =>
          val runtime = cm.movie.runtimeMinutes
            .orElse(cm.filmUrl.flatMap(u => client.fetchFilmDetail(u).flatMap(_.runtimeMinutes)))
          withClue(s"${cm.movie.title} (${cm.filmUrl.getOrElse("?")}): ") { runtime should not be empty }
        }
      }
    }
  }
  "Helios Posnania"          should "fetch films" in requireRuntime(Helios)
  "Cinema City Plaza"        should "fetch films" in runtimeOptional(CinemaCityPoznanPlaza)
  "Cinema City Kinepolis"    should "fetch films" in runtimeOptional(CinemaCityKinepolis)
  "Kino Muza"                should "fetch films" in requireRuntime(KinoMuza)
  "Kino Bułgarska 19"        should "fetch films" in runtimeOptional(KinoBulgarska)
  "Kino Apollo"              should "fetch films" in requireNoRuntime(KinoApollo)
  // Rialto is mostly films but occasionally lists a concert/opera
  // retransmission (e.g. "André Rieu … retransmisja") that genuinely has no
  // runtime on the page. It's a real listing with showtimes, so the scraper
  // rightly keeps it — tolerate a single runtime-less entry while still
  // failing if the parser drops runtime from a second listing.
  "Kino Rialto"              should "fetch films" in runtimeAllButOne(Rialto)

  private def requireRuntime(cinema: Cinema)    = runScraperFor(cinema, assertAllHaveRuntime)
  private def runtimeOptional(cinema: Cinema)   = runScraperFor(cinema, assertMostHaveRuntime)
  private def runtimeAllButOne(cinema: Cinema)  = runScraperFor(cinema, assertAllButOneHaveRuntime)
  private def requireNoRuntime(cinema: Cinema)  = runScraperFor(cinema, _ => ()) // parser doesn't expose runtime

  private def runScraperFor(cinema: Cinema, runtimeAssertion: Seq[CinemaMovie] => Unit): Unit = {
    val scraper = scraperFor(cinema)
    RetryWithBackoff() {
      val result = scraper.fetch()
      withClue(s"${cinema.displayName}: ") {
        assertBaseShape(result)
        runtimeAssertion(result)
      }
    }
  }

  private def scraperFor(cinema: Cinema): CinemaScraper =
    wiring.cinemaScrapers.find(_.cinema == cinema)
      .getOrElse(fail(s"No scraper wired for ${cinema.displayName}"))

  private def assertAllHaveRuntime(result: Seq[CinemaMovie]): Unit =
    result.find(_.movie.runtimeMinutes.isEmpty) shouldBe empty

  private def assertMostHaveRuntime(result: Seq[CinemaMovie]): Unit = {
    val withRuntime = result.count(_.movie.runtimeMinutes.nonEmpty)
    withClue(s"only $withRuntime of ${result.size} movies have a runtime: ") {
      (withRuntime * 3) should be >= (result.size * 2)
    }
  }

  private def assertAllButOneHaveRuntime(result: Seq[CinemaMovie]): Unit = {
    val withoutRuntime = result.filter(_.movie.runtimeMinutes.isEmpty).map(_.movie.title)
    withClue(s"${withoutRuntime.size} movies lack a runtime (at most one event listing tolerated): $withoutRuntime ") {
      withoutRuntime.size should be <= 1
    }
  }

  private def assertBaseShape(result: Seq[CinemaMovie]): Unit = {
    result                      should not be empty
    result.flatMap(_.showtimes) should not be empty
  }
}
