package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

/** Regression for the Kino Muza detail-page synopsis race.
 *
 *  Muza's synopsis/poster live on each film's detail page and are fetched ONLY
 *  by the async `KinoMuzaSynopsisRefresher` (subscribed to `CinemaMovieAdded`,
 *  dispatching the fetch onto its own scheduler). `drainServices` settled the
 *  TMDB/IMDb/ratings cascade but NOT this refresher — so whether the synopsis
 *  had landed by the time a spec snapshotted the cache was a pure timing race.
 *  `ScrapeOrderDeterminismSpec`'s whole-corpus path masked it (its long serial
 *  `converge` always gave the async fetch time to win); the fast single-film
 *  replay surfaced it as a flaky `synopsis`/`posterUrl` Some-vs-None on Kino
 *  Muza films (e.g. "Erupcja").
 *
 *  `drainServices` now drives `refreshAllStaleSync()` — a deterministic, bounded
 *  inline pass. Here the async path is stopped first, so the ONLY thing that can
 *  fill a Muza synopsis is that sync pass: the assertion fails before the fix
 *  (every Muza slot stays `None`) and passes after. */
class KinoMuzaSynopsisDrainSpec extends AnyFlatSpec with Matchers {

  "drainServices" should "settle Kino Muza detail synopsis inline, not leave it to the async race" in {
    val w = new FixtureTestWiring("08-06-2026")
    // Disable the async event-driven refresher so the sync drain pass is the
    // only thing that can populate a Muza synopsis — isolating the fix from the
    // timing race it removes.
    w.kinoMuzaSynopsisRefresher.stop()

    val muzaFilms = w.kinoMuzaClient.fetch()
    muzaFilms should not be empty
    w.movieCache.recordCinemaScrape(KinoMuza, muzaFilms)

    w.drainServices()

    // At least one Muza slot now carries a real (non-empty) synopsis from its
    // detail page. Without the inline drain pass — and with the async path
    // stopped — every Muza slot would still be `None` here.
    val muzaSynopses = w.movieCache.snapshot()
      .flatMap(_.record.data.get(KinoMuza))
      .flatMap(_.synopsis)
      .filter(_.nonEmpty)
    muzaSynopses should not be empty
  }
}
