package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

/** Regression for Kino Muza's deferred detail-page synopsis.
 *
 *  Muza's synopsis/poster/trailer live on each film's detail page, not its
 *  listing. With staging ingest always-on, a newcomer Muza film no longer fills
 *  its detail via the cache's `EnrichDetails` task — `recordCinemaScrape`
 *  diverts it to `pending_movies`, and the staging promoter detail-enriches it
 *  (calling `KinoMuzaClient.fetchFilmDetail`) before folding it into `movies`.
 *  Unlike Pałacowe et al., Muza's detail is display-only, so
 *  `defersTmdbResolution = false` — the row resolves from the listing and the
 *  synopsis merges in during promotion.
 *
 *  This pins the contract that the staging pipeline actually fills a Muza
 *  synopsis: it fails if KinoMuza stops being detail-enriched on its way out of
 *  staging (every Muza slot would stay `None`). */
class KinoMuzaSynopsisDrainSpec extends AnyFlatSpec with Matchers {

  "the staging pipeline" should "fill Kino Muza detail synopsis while promoting it out of staging" in {
    val w = new FixtureTestWiring("08-06-2026")

    val muzaFilms = w.cinemaScraperCatalog.kinoMuzaClient.fetch()
    muzaFilms should not be empty
    // Newcomers divert to `pending_movies` (staging always-on); the cache stays
    // empty until the fold.
    w.movieCache.recordCinemaScrape(KinoMuza, muzaFilms)

    // Promote: the staging promoter fetches each Muza film's detail page and
    // merges synopsis / poster / trailer into the Muza slot, then folds the
    // resolved row into `movies` (rehydrated into the cache).
    w.drainStaging()

    val muzaSynopses = w.movieCache.snapshot()
      .flatMap(_.record.cinemaShowings.collect { case (KinoMuza, slot) => slot })
      .flatMap(_.synopsis)
      .filter(_.nonEmpty)
    muzaSynopses should not be empty
  }
}
