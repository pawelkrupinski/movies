package services

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.cinemas.CinemaScraper
import services.movies.MovieCache
import services.events.{EventBus, MovieRecordCreated}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit

/** Scrape scheduler. Hits every cinema every 5 minutes, hands the result
 *  to `MovieCache.recordCinemaScrape`, and publishes a `MovieRecordCreated` event
 *  per movie so the enrichment pipeline can pick them up. Holds no
 *  in-memory state of its own — `MovieCache` is the read path.
 *
 *  Per CLAUDE.md OCP guidance: adding a new cinema is a new `CinemaScraper`
 *  wired in `AppLoader`; this class never names a specific cinema. */
class ShowtimeCache(
  scrapers:   Seq[CinemaScraper],
  bus:        EventBus,
  movieCache: MovieCache
) extends Logging {

  logger.info(s"Starting — commit ${Option(System.getenv("COMMIT_SHA")).getOrElse("unknown")}")

  private val fetchExecutor = DaemonExecutors.fixedPool("showtime-fetch", scrapers.size.max(1))
  private val scheduler     = DaemonExecutors.scheduler("showtime-cache-refresh")

  /** Schedule the periodic refresh. Fires immediately on the first tick so
   *  the cache starts warming as soon as the app is up; then every 5
   *  minutes. Each cinema fetch publishes its own `MovieRecordCreated` events as
   *  soon as it completes, so enrichment starts work without waiting for
   *  the N-cinema barrier. */
  def start(): Unit =
    scheduler.scheduleAtFixedRate(
      () => scrapers.foreach { s =>
        fetchExecutor.submit(new Runnable { def run(): Unit = refreshOne(s) })
      },
      0L, 5L, TimeUnit.MINUTES
    )

  def stop(): Unit = {
    scheduler.shutdown()
    fetchExecutor.shutdown()
  }

  private def refreshOne(scraper: CinemaScraper): Unit = {
    val cinema = scraper.cinema
    val t0     = System.currentTimeMillis()
    try {
      val movies  = scraper.fetch()
      val elapsed = System.currentTimeMillis() - t0
      // Publish against the *canonical* CacheKey that recordCinemaScrape
      // actually wrote each slot to — when the redirect absorbed this
      // scrape's (title, year) into an existing sibling row, the bus event
      // names the sibling's key so the TMDB stage doesn't run a second
      // time for a phantom row.
      //
      // Suppress the event when this (cinema, raw title, raw year) tuple
      // has already been scraped onto this row: every downstream listener
      // is idempotent (re-checking state before doing work), so a steady-
      // state tick where nothing changed would dispatch hundreds of no-op
      // events per 5-minute interval. Listeners still get fresh events
      // when a cinema reports a new title spelling, a year correction, or
      // when a brand new film shows up.
      val touched     = movieCache.recordCinemaScrape(cinema, movies)
      val publishable = touched.count(_._3)
      logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms ($publishable new)")
      touched.foreach { case (cm, key, isNew) =>
        if (isNew)
          bus.publish(MovieRecordCreated(key.cleanTitle, key.year, cm.movie.originalTitle, cm.director))
      }
    } catch {
      case e: Exception =>
        val elapsed = System.currentTimeMillis() - t0
        logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
    }
  }
}
