package services.cinemas

import models.{KinoMuza, Source, SourceData}
import play.api.Logging
import services.Stoppable
import services.movies.MovieCache
import tools.{DaemonExecutors, HttpFetch}

import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}

/**
 * Periodic background job that fills in missing Kino Muza synopses one row
 * at a time. The 5-minute scrape (`KinoMuzaClient.fetch`) only loads the
 * listing page — per-film synopses live on detail pages
 * (`https://www.kinomuza.pl/movie/<slug>/`) and fetching 80+ of them every
 * tick trips Muza's burst limiter, leaving the whole batch empty. This
 * refresher takes a slower, steadier pass: one detail page per tick, far
 * below the rate that triggers the limiter.
 *
 * Empty-string sentinel for "tried, nothing found": when Muza's detail
 * page parses cleanly but holds no synopsis paragraph, the slot's
 * `synopsis` field is set to `Some("")` — so the candidate filter
 * (`synopsis.isEmpty`, which matches `None` only) doesn't pick it up
 * again. A failed HTTP request leaves `synopsis = None` so the row stays
 * in the queue and the next tick retries.
 *
 * Lifecycle owned by `Wiring` (`start()` schedules the periodic tick;
 * `stop()` shuts the scheduler down). Per CLAUDE.md the class never
 * self-schedules in its constructor.
 */
class KinoMuzaSynopsisRefresher(
  cache:  MovieCache,
  client: KinoMuzaClient,
  http:   HttpFetch
) extends Stoppable with Logging {

  private val scheduler = DaemonExecutors.scheduler("kino-muza-synopsis")
  // First tick 60 s after boot so the cache has hydrated and the first
  // scrape has populated current Muza slots. Then once a minute — Muza
  // tolerates that easily and 50 unresolved rows clear in under an hour.
  private val StartupDelaySeconds = 60L
  private val IntervalSeconds     = 60L

  /** Find one cached row whose Muza slot has a filmUrl but no synopsis yet,
   *  fetch + parse the detail page, write the result back. Public so tests
   *  can invoke a single pass deterministically.
   *
   *  Returns true when a row was processed (whether or not the fetch
   *  produced a synopsis), false when there was nothing to do. */
  def refreshOne(): Boolean = {
    val candidate = cache.entries.iterator
      .collectFirst {
        case (key, e) if e.data.get(KinoMuza).exists(needsSynopsis) => (key, e)
      }
    candidate match {
      case None => false
      case Some((key, _)) =>
        val slot    = cache.get(key).flatMap(_.data.get(KinoMuza)).getOrElse(SourceData())
        val url     = slot.filmUrl.getOrElse(return false)
        Try(http.get(url)) match {
          case Success(html) =>
            // `Some("")` is the "tried, nothing found" sentinel — keeps the
            // row out of the candidate filter on subsequent ticks even when
            // Muza's page has no synopsis paragraph.
            val synopsis = client.parseSynopsis(html).getOrElse("")
            writeSynopsis(key, synopsis)
            logger.debug(s"KinoMuza synopsis: ${key.cleanTitle} (${synopsis.length} chars)")
          case Failure(ex) =>
            // Transient fetch failure (timeout, rate limit) — leave the
            // slot's synopsis None so the next tick retries.
            logger.debug(s"KinoMuza synopsis fetch failed for ${key.cleanTitle} ($url): ${ex.getMessage}")
        }
        true
    }
  }

  // Only refresh rows whose Muza slot has a filmUrl (to fetch) and an
  // unset synopsis (None — the empty-string sentinel means "already
  // tried" and explicitly stays out of this set).
  private def needsSynopsis(slot: SourceData): Boolean =
    slot.synopsis.isEmpty && slot.filmUrl.isDefined

  private def writeSynopsis(key: services.movies.CacheKey, synopsis: String): Unit =
    cache.putIfPresent(key, current =>
      current.data.get(KinoMuza) match {
        case Some(s) => current.copy(data = current.data + ((KinoMuza: Source) -> s.copy(synopsis = Some(synopsis))))
        case None    => current  // Muza slot dropped between read and write — leave alone.
      }
    )

  def start(): Unit = {
    logger.info(s"KinoMuza synopsis refresher scheduled every ${IntervalSeconds}s " +
                s"(first run in ${StartupDelaySeconds}s).")
    scheduler.scheduleAtFixedRate(
      () => Try(refreshOne()).recover {
        case ex => logger.warn(s"KinoMuza synopsis refresher tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, IntervalSeconds, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = scheduler.shutdown()
}
