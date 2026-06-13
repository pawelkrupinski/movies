package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

/** Regression for Kino Muza's deferred detail-page synopsis.
 *
 *  Muza's synopsis/poster/trailer live on each film's detail page, not its
 *  listing. They ride the standard deferred-detail pipeline now (a deduped
 *  `EnrichDetails` task calling `KinoMuzaClient.fetchFilmDetail`), which
 *  `enrichDetailsSync` drains to completion alongside every other
 *  `DetailEnricher`. Unlike Pałacowe et al., Muza's detail is display-only, so
 *  `defersTmdbResolution = false` — the row resolves from the listing and the
 *  synopsis merges in when its task runs.
 *
 *  This pins the contract that the detail task actually fills a Muza synopsis:
 *  it fails if KinoMuza stops being wired into the deferred-detail pipeline
 *  (every Muza slot would stay `None`). */
class KinoMuzaSynopsisDrainSpec extends AnyFlatSpec with Matchers {

  "the deferred-detail pipeline" should "fill Kino Muza detail synopsis via its EnrichDetails task" in {
    val w = new FixtureTestWiring("08-06-2026")

    val muzaFilms = w.cinemaScraperCatalog.kinoMuzaClient.fetch()
    muzaFilms should not be empty
    w.movieCache.recordCinemaScrape(KinoMuza, muzaFilms)

    // Drain the EnrichDetails tasks (detail reaper enqueues each Muza row with a
    // filmUrl; the real handler fetches the detail page and merges synopsis /
    // poster / trailer into the Muza slot).
    w.enrichDetailsSync()

    val muzaSynopses = w.movieCache.snapshot()
      .flatMap(_.record.data.get(KinoMuza))
      .flatMap(_.synopsis)
      .filter(_.nonEmpty)
    muzaSynopses should not be empty
  }
}
