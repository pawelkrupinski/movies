package services.cinemas.common

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.movies.ScrapeSink
import services.scrapes.{ScrapeArchiveRepository, ScrapeAttempt}

import java.time.Clock

/**
 * The per-cinema scrape core: fetch a cinema's current listings and hand them to the identity intake,
 * archiving the attempt either way. Shared so the "what happens for one cinema" rule lives in one place:
 * the queue-driven `ScrapeCinemaHandler` calls this for each scrape task, and the fixture harness calls
 * it directly. It deliberately does NOT catch scrape failures — each caller decides what a failure means
 * (the handler logs and lets the reaper retry).
 */
class CinemaScrapeRunner(
  // Where the scrape goes after it is archived: the identity intake, which takes the listing as the
  // venue's published one (docs/design/identity-resolver.md §8).
  sink:          ScrapeSink,
  // Stamps each archived attempt.
  clock:         Clock,
  // Keeps each cinema's last consolidated listing so it can be replayed later
  // (into a test, into an empty database) without re-scraping. Defaults to the
  // no-op store so specs and scripts that don't care needn't wire one.
  scrapeArchive: ScrapeArchiveRepository = ScrapeArchiveRepository.empty,
  // Told each landing's completeness, so a venue that is never complete (never pruned) shows.
  completeness:  ListingCompletenessRecorder = ListingCompletenessRecorder.none
) extends Logging {

  /** Scrape `scraper`'s cinema, hand the listing to the sink and archive it; the listing as fetched,
   *  which is what the venue's remaining horizon is read off (`VenueScrapeCadence`). */
  def run(scraper: CinemaScraper): Seq[CinemaMovie] = {
    val cinema: Cinema = scraper.cinema
    val t0      = tools.Stopwatch.start()
    // A throw is archived as a barren attempt and then rethrown untouched, so
    // callers keep deciding what a failure means while the archive still records
    // that the cinema was tried and failed.
    val CinemaScraper.Scraped(movies, viaFallback, readsComplete) =
      try scraper.fetchWithSource()
      catch {
        case failure: Throwable =>
          archive(scraper, Seq.empty, listingComplete = true, Some(messageOf(failure)))
          throw failure
      }
    // The sink decides FIRST, then the scrape is archived — in a `finally`, so a sink that throws
    // still leaves the attempt archived. The intake reads the archive for a venue with no accepted
    // listing of its own: archived first, that "last scrape" was the very scrape being judged, so the
    // depth/breadth guards compared a shrunken listing with itself and never held it back, and the
    // first scrape was never recorded as accepted. What is archived is still the client's own output
    // (`movies` as fetched) — what a replay needs. Both scrape paths reach here: a non-chunked
    // `fetch()` is the live scrape, and a chunked one arrives as a `PreScrapedCinemaScraper` wrapping
    // the already-reduced chunks.
    // Complete only when the scraper's structure says so AND every page it read answered.
    val verdict  = ListingCompleteness.of(scraper.listingIsComplete, readsComplete)
    val complete = verdict == ListingCompleteness.Complete
    completeness.landed(cinema, verdict)
    try sink.recordCinemaScrape(cinema, movies, complete, scraper.sourceKey, viaFallback)
    finally archive(scraper, movies, complete, error = None)
    logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${t0.millis}ms" +
      (if (complete) "" else " — listing INCOMPLETE (a page failed or a chunk is missing), so nothing is pruned"))
    movies
  }

  /** File one scrape attempt in the archive. */
  private def archive(scraper: CinemaScraper, movies: Seq[CinemaMovie], listingComplete: Boolean, error: Option[String]): Unit =
    scrapeArchive.record(ScrapeAttempt(
      cinema           = scraper.cinema,
      city             = Cinema.cityOf(scraper.cinema),
      at               = clock.instant(),
      listingComplete  = listingComplete,
      films            = movies,
      error            = error,
      noScheduleListed = scraper.noScheduleListed
    ))

  /** Exception messages are often null (NPE, some driver errors); fall back to
   *  the class name so a red row always says something. */
  private def messageOf(failure: Throwable): String =
    Option(failure.getMessage).filter(_.nonEmpty).getOrElse(failure.getClass.getName)
}
