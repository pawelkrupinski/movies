package services.movies

import play.api.Logging
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.util.Try

/**
 * Daily tick: drop rows whose `cinemaShowings` map is empty. Those are films
 * that no cinema is currently showing — every cinema's scrape tick has
 * pruned its slot via `recordCinemaScrape`. Without this cleanup the cache
 * + Mongo grow forever, holding enrichment for thousands of films that
 * dropped out of all schedules months ago.
 *
 * Trade-off: a film that returns after a gap pays the full re-enrichment
 * cost on its next scrape (TMDB search → IMDb suggestion → MC/RT/Filmweb
 * URL discovery + rating scrapes). Acceptable given how rarely a real
 * re-screening happens versus the steady-state churn of one-off events,
 * festival items, anniversary screenings, and similar.
 *
 * Lifecycle owned by `AppLoader` (`start()` schedules the daily tick;
 * `stop()` is registered as a shutdown hook). Per CLAUDE.md, the class
 * never self-subscribes or self-schedules.
 */
class UnscreenedCleanup(cache: MovieCache) extends Logging {

  private val scheduler = DaemonExecutors.scheduler("unscreened-cleanup")

  // First run shortly after boot so newly hydrated rows get a pass; then
  // every 24h. The hour-of-day this lands on shifts with each restart, which
  // is fine — there's no consumer timing dependency.
  private val StartupDelaySeconds = 20L
  private val RunEveryHours       = 24L

  /** Walk every cached row; drop the ones with no cinema slot left. Returns
   *  the count of rows removed. Public so the backfill script (and tests)
   *  can invoke a one-shot pass; the daily scheduler calls the same method. */
  def removeUnscreened(): Int = {
    val orphans = cache.entries.collect { case (k, e) if e.cinemaShowings.isEmpty => k }
    if (orphans.nonEmpty)
      logger.info(s"Unscreened-row cleanup: dropping ${orphans.size} row(s) with no current screenings.")
    orphans.foreach(cache.invalidate)
    orphans.size
  }

  def start(): Unit = {
    logger.info(s"Unscreened-row cleanup scheduled every ${RunEveryHours}h (first run in ${StartupDelaySeconds}s).")
    scheduler.scheduleAtFixedRate(
      () => Try(removeUnscreened()).recover {
        case ex => logger.warn(s"Unscreened-row cleanup tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, RunEveryHours * 3600, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = scheduler.shutdown()
}
