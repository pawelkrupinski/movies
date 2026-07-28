package services.cinemas.common

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.events.{EventBus, MovieDetailsComplete}
import services.movies.{CacheKey, MovieCache}
import services.cinemas.pl.FilmwebShowtimesClient
import services.scrapes.{ScrapeArchiveRepository, ScrapeAttempt}

import java.time.Instant

/**
 * The per-cinema scrape core: fetch a cinema's current listings, write them
 * through `MovieCache`, and decide — per genuinely-new (cinema, title, year) —
 * whether to enrich it NOW or after deferred detail.
 *
 * A film a DEFERRED cinema (one implementing `DetailEnricher`) scrapes with a
 * detail `filmUrl` will get an `EnrichDetails` task that supplies its director /
 * original title / production year. For those we DON'T publish
 * `MovieDetailsComplete` yet — we mark the row `detailPending` so it's held out
 * of the read model and out of the TMDB stage until the detail lands; the
 * `EnrichDetailsHandler` publishes the event once it does. Everything else (no
 * deferred detail, or a row already TMDB-concluded) is published immediately, so
 * TMDB resolves it right away. `recordCinemaScrape` also publishes
 * `CinemaMovieAdded` per new film, which is what a `DetailTaskEnqueuer` keys the
 * detail task off (with `DetailReaper` as the periodic backstop).
 *
 * Shared so the "what happens for one cinema" rule lives in one place: the
 * queue-driven `ScrapeCinemaHandler` calls this for each scrape task, and the
 * fixture harness calls it directly. It deliberately does NOT catch scrape
 * failures — each caller decides what a failure means (the handler logs and
 * lets the reaper retry).
 */
class CinemaScrapeRunner(
  movieCache:      MovieCache,
  bus:             EventBus,
  deferredCinemas: Set[Cinema],
  // Keeps each cinema's last consolidated listing so it can be replayed later
  // (into a test, into an empty database) without re-scraping. Defaults to the
  // no-op store so specs and scripts that don't care needn't wire one.
  scrapeArchive:   ScrapeArchiveRepository = ScrapeArchiveRepository.empty
) extends Logging {

  def run(scraper: CinemaScraper): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    val cinema: Cinema = scraper.cinema
    val t0      = System.currentTimeMillis()
    // A throw is archived as a barren attempt and then rethrown untouched, so
    // callers keep deciding what a failure means while the archive still records
    // that the cinema was tried and failed.
    val movies  =
      try scraper.fetch()
      catch {
        case failure: Throwable =>
          archive(scraper, Seq.empty, Some(messageOf(failure)))
          throw failure
      }
    // Archive BEFORE the cache fold, so what lands is the client's own output
    // rather than anything the corpus merge did to it — that is what a replay
    // needs. Both scrape paths reach here: a non-chunked `fetch()` is the live
    // scrape, and a chunked one arrives as a `PreScrapedCinemaScraper` wrapping
    // the already-reduced chunks.
    archive(scraper, movies, error = None)
    val touched = movieCache.recordCinemaScrape(cinema, movies, scraper.listingIsComplete)
    val events   = classify(cinema, touched)
    val elapsed  = System.currentTimeMillis() - t0
    val awaiting = touched.count(_._3) - events.size
    logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms (${events.size} ready, $awaiting awaiting detail)")
    events.foreach(bus.publish)
    touched
  }

  private def archive(scraper: CinemaScraper, movies: Seq[CinemaMovie], error: Option[String]): Unit =
    scrapeArchive.record(ScrapeAttempt(
      cinema          = scraper.cinema,
      city            = Cinema.cityOf(scraper.cinema),
      at              = Instant.now(),
      listingComplete = scraper.listingIsComplete,
      films           = movies,
      error           = error
    ))

  /** Exception messages are often null (NPE, some driver errors); fall back to
   *  the class name so a red row always says something. */
  private def messageOf(failure: Throwable): String =
    Option(failure.getMessage).filter(_.nonEmpty).getOrElse(failure.getClass.getName)

  /** For each genuinely-new `(cinema, title, year)` in `touched`, decide its
   *  enrichment trigger. As a side effect, marks `detailPending = true` on the
   *  rows that must wait for deferred detail; returns the `MovieDetailsComplete`
   *  events to publish now for the rows that are ready. Shared by `run` (prod,
   *  publishes inline) and the fixture harness (collects + publishes once the
   *  whole tick has settled). */
  def classify(cinema: Cinema, touched: Seq[(CinemaMovie, CacheKey, Boolean)]): Seq[MovieDetailsComplete] =
    touched.collect { case (cm, key, true) => (cm, key) }.flatMap { case (cm, key) =>
      if (movieCache.get(key).exists(_.tmdbConcluded))
        None // already resolved / concluded no-match — a re-scrape needn't re-trigger
      else if (deferredCinemas(cinema) && cm.filmUrl.exists(u => !FilmwebShowtimesClient.isFilmwebFilmUrl(u))) {
        // Wait for the EnrichDetails task to supply director/originalTitle/year;
        // `EnrichDetailsHandler` publishes MovieDetailsComplete once it lands. A
        // Filmweb-FALLBACK row's filmweb.pl URL is excluded — the cinema's own
        // enricher can't fetch it, so the row would hang `detailPending` forever;
        // it enriches now from its listing/Filmweb data instead.
        movieCache.putIfPresent(key, _.copy(detailPending = true))
        None
      } else
        Some(CinemaScrapeRunner.detailsCompleteEvent(cm, key))
    }
}

object CinemaScrapeRunner {
  /** The `MovieDetailsComplete` a ready-to-enrich scraped row implies. Pure, so
   *  the scrape-side producers can't drift in how they build it. */
  def detailsCompleteEvent(cm: CinemaMovie, key: CacheKey): MovieDetailsComplete =
    MovieDetailsComplete(
      key.cleanTitle, key.year, cm.movie.originalTitle,
      if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None)
}
