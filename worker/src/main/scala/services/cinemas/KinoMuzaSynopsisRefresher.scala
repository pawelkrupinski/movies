package services.cinemas

import models.{KinoMuza, Source, SourceData}
import org.jsoup.Jsoup
import play.api.Logging
import services.Stoppable
import services.events.{CinemaMovieAdded, DomainEvent}
import services.movies.{CacheKey, MovieCache}
import tools.{DaemonExecutors, HttpFetch}

import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}

/**
 * Per-row detail-page enricher for Kino Muza. The 5-min listing scrape
 * (`KinoMuzaClient.fetch`) deliberately carries no synopsis, trailer, or
 * poster — those live on each film's detail page
 * (`https://www.kinomuza.pl/movie/<slug>/`) and fetching all 80+ every
 * tick trips Muza's burst limiter, leaving the whole batch empty.
 *
 * Two triggers:
 *
 *   - **Event-driven** (fast path): subscribe `onCinemaMovieAdded` to the
 *     `CinemaMovieAdded` event that `MovieCache.recordCinemaScrape`
 *     publishes for newly-persisted (Muza, title, year) tuples. The slot
 *     is already in the cache when the event fires (publish-after-put),
 *     so the handler can read the slot's `filmUrl` immediately and
 *     dispatch the fetch onto its own scheduler — no race, no blocking
 *     of the publisher's thread.
 *
 *   - **Periodic safety net** (slow path): `refreshOne()` ticks once a
 *     minute and walks any row whose Muza slot still has a `filmUrl` but
 *     no synopsis. Catches rows whose first event was dropped (process
 *     restart between persist and consumption, transient handler crash,
 *     etc.).
 *
 * Synopsis, trailer, and poster share the same detail-page fetch — Muza
 * ships all three on the same page, so one HTTP request per candidate
 * covers them.
 *
 * Empty-string sentinel for "tried, nothing found": when Muza's detail
 * page parses cleanly but holds no synopsis paragraph, the slot's
 * `synopsis` field is set to `Some("")` — so the candidate filter
 * (`synopsis.isEmpty`, which matches `None` only) doesn't pick it up
 * again. Trailer/poster have no analogous sentinel — they just stay
 * `None` if the detail page lacks them, which is what we want.
 *
 * A failed HTTP request leaves the slot untouched so the next periodic
 * tick (or a later event) retries.
 *
 * Lifecycle owned by `Wiring` (`start()` schedules the periodic tick,
 * `Wiring` also subscribes `onCinemaMovieAdded`; `stop()` shuts the
 * scheduler down). Per CLAUDE.md the class never self-schedules or
 * self-subscribes in its constructor.
 */
class KinoMuzaSynopsisRefresher(
  cache:  MovieCache,
  client: KinoMuzaClient,
  http:   HttpFetch
) extends Stoppable with Logging {

  private val scheduler = DaemonExecutors.scheduler("kino-muza-synopsis")
  // First tick 60 s after boot so the cache has hydrated and the first
  // scrape has populated current Muza slots. Then every 5 minutes — the
  // event path covers the common case (new Muza film → immediate
  // detail-page fetch); this slower scan is only the safety net for
  // events lost across a restart, so it doesn't need to be hot.
  private val StartupDelaySeconds = 60L
  private val IntervalSeconds     = 300L

  /** Event-driven entry point. Subscribed by `Wiring` to the bus —
   *  fires when `recordCinemaScrape` persists a *new* Kino Muza
   *  (title, year) tuple. Dispatches the actual HTTP fetch onto the
   *  refresher's own scheduler so the publisher's thread doesn't block
   *  on the network round-trip. */
  def onCinemaMovieAdded: PartialFunction[DomainEvent, Unit] = {
    case CinemaMovieAdded(KinoMuza, title, year, Some(filmUrl)) =>
      val key = cache.keyOf(title, year)
      scheduler.schedule(
        new Runnable {
          def run(): Unit = Try(refreshIfStale(key, filmUrl)).recover {
            case ex => logger.warn(s"KinoMuza event-driven refresh failed for $title: ${ex.getMessage}")
          }
        },
        0L, TimeUnit.SECONDS
      ): Unit
  }

  /** Find one cached row whose Muza slot has a filmUrl but no synopsis yet,
   *  fetch + parse the detail page, write the result back. Public so tests
   *  can invoke a single pass deterministically.
   *
   *  Returns true when a row was processed (whether or not the fetch
   *  produced a synopsis), false when there was nothing to do. */
  def refreshOne(): Boolean = {
    val candidate: Option[CacheKey] = cache.entries.iterator
      .collectFirst { case (key, e) if e.data.get(KinoMuza).exists(needsRefresh) => key }
    candidate match {
      case None      => false
      case Some(key) =>
        cache.get(key).flatMap(_.data.get(KinoMuza)).flatMap(_.filmUrl) match {
          case Some(url) => fetchAndWrite(key, url); true
          case None      => false
        }
    }
  }

  /** Synchronously refresh EVERY currently-stale Muza slot, exactly once each,
   *  in deterministic key order. Unlike the async `onCinemaMovieAdded` path —
   *  which dispatches each fetch onto its own scheduler and so races a snapshot
   *  taken right after the scrape — this settles the synopsis/poster inline and
   *  makes the outcome a pure function of the fixtures. Used by the test
   *  harness drain (`TestWiring.drainServices`) so the determinism specs don't
   *  see a `synopsis`/`posterUrl` that sometimes landed and sometimes didn't. A
   *  slot whose detail fetch fails stays `None` (same as prod's "retry next
   *  tick") but deterministically — so it's identical every replay. One bounded
   *  pass: a failed fetch is NOT re-picked, unlike a `refreshOne` loop. */
  def refreshAllStaleSync(): Unit =
    cache.entries.iterator
      .collect { case (key, e) if e.data.get(KinoMuza).exists(needsRefresh) => key }
      .toSeq
      .sortBy(k => (k.cleanTitle, k.year.getOrElse(Int.MinValue)))
      .foreach { key =>
        cache.get(key).flatMap(_.data.get(KinoMuza)).flatMap(_.filmUrl)
          .foreach(url => fetchAndWrite(key, url))
      }

  /** Idempotent per-row refresh — only fetches when the slot still looks
   *  unrefreshed. Used by the event handler to avoid re-fetching a row
   *  whose first-time event was already processed and persisted. */
  private def refreshIfStale(key: CacheKey, filmUrl: String): Unit =
    if (cache.get(key).flatMap(_.data.get(KinoMuza)).exists(needsRefresh))
      fetchAndWrite(key, filmUrl)

  private def fetchAndWrite(key: CacheKey, url: String): Unit =
    Try(http.get(url)) match {
      case Success(stringHtml) =>
        // Parse the detail page once; pass the same Document to all three
        // parsers (synopsis / trailer / poster) so Jsoup's moderate-cost
        // parse runs once per row instead of three times. `Some("")` is
        // the "tried, nothing found" sentinel for synopsis — keeps the
        // row out of the candidate filter on subsequent ticks even when
        // Muza's page has no synopsis paragraph.
        val doc      = Jsoup.parse(stringHtml)
        val synopsis = client.parseSynopsis(doc).getOrElse("")
        val trailer  = client.parseTrailer(doc)
        val poster   = client.parsePoster(doc)
        writeSlot(key, synopsis, trailer, poster)
        logger.debug(s"KinoMuza synopsis+trailer: ${key.cleanTitle} (${synopsis.length} chars" +
                     trailer.map(_ => ", trailer").getOrElse("") +
                     poster.map(_ => ", poster").getOrElse("") + ")")
      case Failure(ex) =>
        // Transient fetch failure (timeout, rate limit) — leave the slot's
        // synopsis None so the next tick / next event retries.
        logger.debug(s"KinoMuza synopsis fetch failed for ${key.cleanTitle} ($url): ${ex.getMessage}")
    }

  // Only refresh rows whose Muza slot has a filmUrl (to fetch) and an
  // unset synopsis (None — the empty-string sentinel means "already
  // tried" and explicitly stays out of this set).
  private def needsRefresh(slot: SourceData): Boolean =
    slot.synopsis.isEmpty && slot.filmUrl.isDefined

  private def writeSlot(
    key:      CacheKey,
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
            // Detail-page poster is higher-fidelity (portrait) than any
            // legacy listing thumbnail still sitting in the slot. Upgrade
            // when present; keep whatever was there when the detail page
            // has no `img.img-fuild` slot.
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
