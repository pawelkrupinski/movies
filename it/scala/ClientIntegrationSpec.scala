package integration

import models._
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraper
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
  "Kino Pałacowe"            should "fetch films" in requireRuntime(KinoPalacowe)
  "Helios Posnania"          should "fetch films" in requireRuntime(Helios)
  "Cinema City Plaza"        should "fetch films" in runtimeOptional(CinemaCityPoznanPlaza)
  "Cinema City Kinepolis"    should "fetch films" in runtimeOptional(CinemaCityKinepolis)
  "Kino Muza"                should "fetch films" in requireRuntime(KinoMuza)
  "Kino Bułgarska 19"        should "fetch films" in runtimeOptional(KinoBulgarska)
  "Kino Apollo"              should "fetch films" in requireNoRuntime(KinoApollo)
  // Rialto is an event cinema: alongside films it lists concert/opera
  // retransmissions (e.g. "André Rieu … retransmisja") that genuinely have
  // no runtime on the page. They're real listings with showtimes, so the
  // scraper rightly keeps them — runtimeOptional tolerates them while still
  // catching a parser regression that drops runtime from the actual films.
  "Kino Rialto"              should "fetch films" in runtimeOptional(Rialto)

  private def requireRuntime(cinema: Cinema)    = runScraperFor(cinema, assertAllHaveRuntime)
  private def runtimeOptional(cinema: Cinema)   = runScraperFor(cinema, assertMostHaveRuntime)
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

  private def assertBaseShape(result: Seq[CinemaMovie]): Unit = {
    result                      should not be empty
    result.flatMap(_.showtimes) should not be empty
  }
}
