package services.cinemas

import models.{KinoMuza, Source, SourceData}
import play.api.Logging
import services.Stoppable
import services.movies.MovieCache
import tools.{DaemonExecutors, HttpFetch}

import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}

/**
 * Periodic background job that fills in missing Kino Muza synopses (and
 * trailers) one row at a time. The 5-minute scrape (`KinoMuzaClient.fetch`)
 * only loads the listing page — per-film synopses and YouTube embed iframes
 * live on detail pages (`https://www.kinomuza.pl/movie/<slug>/`) and
 * fetching 80+ of them every tick trips Muza's burst limiter, leaving the
 * whole batch empty. This refresher takes a slower, steadier pass: one
 * detail page per tick, far below the rate that triggers the limiter.
 *
 * Synopsis + trailer share the same per-row fetch — Muza ships both on the
 * same detail page, so doing them together costs one HTTP request per
 * candidate instead of two.
 *
 * Empty-string sentinel for "tried, nothing found": when Muza's detail
 * page parses cleanly but holds no synopsis paragraph, the slot's
 * `synopsis` field is set to `Some("")` — so the candidate filter
 * (`synopsis.isEmpty`, which matches `None` only) doesn't pick it up
 * again. Trailer URLs have no analogous sentinel — a film without a
 * trailer iframe just keeps `trailerUrl = None` forever, which is what we
 * want (no display, no retry needed since the page rarely sprouts a
 * trailer post-release).
 *
 * A failed HTTP request leaves both fields untouched so the row stays in
 * the queue and the next tick retries.
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
            val trailer  = client.parseTrailer(html)
            val poster   = client.parsePoster(html)
            writeSlot(key, synopsis, trailer, poster)
            logger.debug(s"KinoMuza synopsis+trailer: ${key.cleanTitle} (${synopsis.length} chars" +
                         trailer.map(_ => ", trailer").getOrElse("") +
                         poster.map(_ => ", poster").getOrElse("") + ")")
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

  private def writeSlot(
    key:      services.movies.CacheKey,
    synopsis: String,
    trailer:  Option[String],
    poster:   Option[String]
  ): Unit =
    cache.putIfPresent(key, current =>
      current.data.get(KinoMuza) match {
        case Some(s) =>
          val updated = s.copy(
            synopsis   = Some(synopsis),
            // Don't overwrite a trailer URL the listing-scrape's `parseHtml`
            // path might already have populated (none today, but keep the
            // refresher idempotent against future schema changes).
            trailerUrl = trailer.orElse(s.trailerUrl),
            // Detail-page poster is higher-fidelity (portrait, ~556×800) than
            // the listing thumbnail (~1200×675 landscape still). Upgrade
            // when present; keep the listing thumbnail when the detail page
            // has no poster slot so we don't lose the fallback.
            posterUrl  = poster.orElse(s.posterUrl)
          )
          current.copy(data = current.data + ((KinoMuza: Source) -> updated))
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
