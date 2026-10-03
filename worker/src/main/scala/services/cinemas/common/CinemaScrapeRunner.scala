package services.cinemas.common

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.events.{EventBus, MovieDetailsComplete}
import services.movies.{CacheKey, MovieCache, ScrapeSink}
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
  scrapeArchive:   ScrapeArchiveRepository = ScrapeArchiveRepository.empty,
  // Where the scrape goes after it is archived: `movieCache`'s landing, unless the country is cut
  // over to the identity projection (docs/design/identity-resolver.md §8, phase 5), whose intake
  // takes the listing as the venue's published one and places nothing itself.
  landing:         Option[ScrapeSink] = None
) extends Logging {

  private val sink: ScrapeSink = landing.getOrElse(movieCache)

  def run(scraper: CinemaScraper): Seq[(CinemaMovie, CacheKey, Boolean)] = scrape(scraper)._2

  /** [[run]], with the listing as fetched beside what the sink placed: a venue's remaining runway is
   *  read off the listing (`VenueScrapeCadence`) — the identity intake places nothing itself. */
  def scrape(scraper: CinemaScraper): (Seq[CinemaMovie], Seq[(CinemaMovie, CacheKey, Boolean)]) = {
    val cinema: Cinema = scraper.cinema
    val t0      = tools.Stopwatch.start()
    // A throw is archived as a barren attempt and then rethrown untouched, so
    // callers keep deciding what a failure means while the archive still records
    // that the cinema was tried and failed.
    val CinemaScraper.Scraped(movies, viaFallback) =
      try scraper.fetchWithSource()
      catch {
        case failure: Throwable =>
          archive(scraper, Seq.empty, Some(messageOf(failure)))
          throw failure
      }
    // The sink decides FIRST, then the scrape is archived — in a `finally`, so a sink that throws
    // still leaves the attempt archived. A cut-over country's intake reads the archive for a venue
    // with no accepted listing of its own (its last scrape is what the old path last landed from):
    // archived first, that "last scrape" was the very scrape being judged, so the depth/breadth
    // guards compared a shrunken listing with itself and never held it back, and the first scrape
    // was never recorded as accepted. What is archived is still the client's own output (`movies`
    // as fetched, never the corpus merge's) — what a replay needs. Both scrape paths reach here: a
    // non-chunked `fetch()` is the live scrape, and a chunked one arrives as a
    // `PreScrapedCinemaScraper` wrapping the already-reduced chunks.
    val touched =
      try sink.recordCinemaScrape(cinema, movies, scraper.listingIsComplete, scraper.sourceKey, viaFallback)
      finally archive(scraper, movies, error = None)
    val events   = classify(cinema, touched)
    val elapsed  = t0.millis
    val awaiting = touched.count(_._3) - events.size
    logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms (${events.size} ready, $awaiting awaiting detail)")
    events.foreach(bus.publish)
    (movies, touched)
  }

  /** File one scrape attempt in the archive — the runner's own step, public so a harness that
   *  drives the scrape itself (`TestWiring.runOneScrapeTick`) archives exactly as `run` does. */
  def archive(scraper: CinemaScraper, movies: Seq[CinemaMovie], error: Option[String]): Unit =
    scrapeArchive.record(ScrapeAttempt(
      cinema           = scraper.cinema,
      city             = Cinema.cityOf(scraper.cinema),
      at               = Instant.now(),
      listingComplete  = scraper.listingIsComplete,
      films            = movies,
      error            = error,
      noScheduleListed = scraper.noScheduleListed
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
