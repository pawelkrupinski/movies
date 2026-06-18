package services.cinemas

import models.{Cinema, CinemaMovie}

/** Decorator that drops live stage/music events (concerts, stand-up, kabaret,
 *  recitals, theatre plays) from a scraper's output, keeping the listings
 *  film-only. Small municipal & arthouse venues mix those into the same feed
 *  their films come from; [[NonMovieEventClassifier]] decides which rows go.
 *
 *  Composed OUTSIDE the retry/uptime/fallback layers (see `WorkerWiring`):
 *   - the inner retry never re-runs just because filtering left zero films;
 *   - `UptimeRecordingScraper` records the upstream's RAW outcome, so a venue
 *     that returned data (films + events) still stamps a healthy bar;
 *   - it filters whatever was finally SERVED — including rows substituted by
 *     `FilmwebFallbackScraper`, whose Filmweb feed carries events too.
 *
 *  Filtering here (inside `fetch()`) rather than at the cache means every
 *  consumer of `fetch()` is covered identically — the production
 *  `CinemaScrapeRunner` and the fixture/e2e harnesses that call
 *  `recordCinemaScrape(scraper.fetch())` directly. An all-events tick collapses
 *  to an empty `fetch()`, which `MovieCache.recordCinemaScrape` already treats
 *  like a failed/empty tick: it keeps the cinema's existing slots rather than
 *  pruning them. */
class NonMovieEventFilteringScraper(delegate: CinemaScraper) extends CinemaScraper {

  def cinema: Cinema = delegate.cinema

  def scrapeHosts: Set[String] = delegate.scrapeHosts

  override def maxFetchAttempts: Int = delegate.maxFetchAttempts

  override def chain: Boolean = delegate.chain

  override def sourceUrl: Option[String] = delegate.sourceUrl

  def fetch(): Seq[CinemaMovie] =
    delegate.fetch().filterNot(cm => NonMovieEventClassifier.isLiveEvent(cm.movie.title))
}
