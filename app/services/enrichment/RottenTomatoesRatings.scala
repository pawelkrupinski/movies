package services.enrichment

import play.api.Logging
import services.events.{DomainEvent, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

/**
 * Rotten Tomatoes score maintenance — the RT counterpart of `ImdbRatings`.
 *
 * Two responsibilities:
 *   1. **Per-row refresh**: when the TMDB stage publishes `TmdbResolved`,
 *      fetch the current Tomatometer score off the row's stored
 *      `rottenTomatoesUrl` and write it back. Subscribe `onTmdbResolved`
 *      on the bus from `AppLoader`.
 *   2. **Periodic walk**: refresh every cached row with an RT URL hourly,
 *      so the score stays close to the live site. Driven by `start()`.
 *
 * Lifecycle is owned by the caller (`AppLoader` calls `start()` and registers
 * `stop()` as a shutdown hook). The class never self-subscribes or
 * self-schedules — see CLAUDE.md.
 *
 * Note: `TmdbResolved` only fires when the TMDB stage produced an `imdbId`.
 * Rows where TMDB resolved without an IMDb cross-reference are picked up by
 * the next hourly walk instead.
 */
class RottenTomatoesRatings(cache: EnrichmentCache, rt: RottenTomatoesClient) extends Logging {

  // 5 workers comfortably under CLAUDE.md's "5–10" band for undocumented
  // services. Each refresh is a single GET to a /m/ page; the bottleneck is
  // RT's response time, not us. Smaller than the TMDB stage's pool so an RT
  // slowdown can't starve more important fetches.
  private val Workers       = 5
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"rt-stage-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  private val refreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "rt-refresh"); t.setDaemon(true); t
  }
  // First run fires shortly after startup so Mongo hydration has time to
  // populate the cache before the walk reads from it.
  private val StartupDelaySeconds = 10L
  private val RefreshHours        = 1L

  // ── Event listener ─────────────────────────────────────────────────────────

  /** Bus listener: fetch the RT score as soon as the TMDB stage publishes a
   *  `TmdbResolved` for this `(title, year)`. Async — the publisher (the
   *  TMDB stage worker) is not blocked on RT. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. */
  private[enrichment] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous version of `schedule` — handy for scripts and tests that
   *  want to drive a refresh on the calling thread. */
  private[enrichment] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  // Look up the row, fetch the score, write back if it changed. Skips rows
  // without a `rottenTomatoesUrl` (RT didn't know the film, or hasn't been
  // resolved yet). Per-row failures are swallowed (network blip, Cloudflare
  // challenge); the next periodic tick tries again.
  private def refreshOne(key: CacheKey): Unit =
    cache.get(key).flatMap(e => e.rottenTomatoesUrl.map(url => (e, url))).foreach { case (e, url) =>
      Try(rt.scoreFor(url)).toOption.flatten match {
        case Some(score) if !e.rottenTomatoes.contains(score) =>
          logger.debug(s"RT: ${key.cleanTitle} $url ${e.rottenTomatoes.getOrElse("—")} → $score")
          cache.put(key, e.copy(rottenTomatoes = Some(score)))
        case _ => ()
      }
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row with a `rottenTomatoesUrl`, refreshing its score.
   *  Skips rows without an RT URL (the next TMDB-retry tick may produce one).
   *  Runs on the dedicated `rt-refresh` thread one entry at a time. */
  private[enrichment] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val withUrl   = snapshot.collect { case (k, e) if e.rottenTomatoesUrl.isDefined => (k, e, e.rottenTomatoesUrl.get) }
    val skipped   = snapshot.size - withUrl.size
    logger.info(s"RT refresh: starting tick over ${withUrl.size} cached row(s) with RT URL" +
                (if (skipped > 0) s" (skipping $skipped without RT URL)." else "."))
    var changed = 0
    var failed  = 0
    withUrl.foreach { case (key, enrichment, url) =>
      Try(rt.scoreFor(url)) match {
        case Success(fresh) if fresh != enrichment.rottenTomatoes =>
          logger.debug(s"RT refresh: ${key.cleanTitle} $url ${enrichment.rottenTomatoes.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.put(key, enrichment.copy(rottenTomatoes = fresh))
          changed += 1
        case Success(_) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"RT refresh: $url lookup failed: ${ex.getMessage}")
      }
    }
    val took = System.currentTimeMillis() - startedAt
    logger.info(s"RT refresh: tick done in ${took}ms — $changed changed, $failed failed, ${withUrl.size - changed - failed} unchanged.")
  }

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the periodic hourly refresh. Called from `AppLoader` — the
   *  class is otherwise lifecycle-agnostic so tests can construct it freely
   *  without a background thread firing on `new`. */
  def start(): Unit = {
    logger.info(s"RT refresh scheduled every ${RefreshHours}h (first run in ${StartupDelaySeconds}s).")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(refreshAll()).recover {
        case ex => logger.warn(s"RT refresh tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, RefreshHours * 3600, TimeUnit.SECONDS
    )
  }

  /** Drain the worker pool so in-flight upserts hit Mongo before
   *  `EnrichmentRepo` closes its client. `AppLoader` must register this so
   *  the repo's close hook runs strictly after this returns. */
  def stop(): Unit = {
    worker.shutdown()
    refreshScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }
}
