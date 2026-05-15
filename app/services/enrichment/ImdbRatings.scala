package services.enrichment

import services.movies.{CacheKey, MovieCache}

import play.api.Logging
import services.events.{DomainEvent, ImdbIdResolved, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

/**
 * IMDb rating maintenance — extracted out of `MovieService` so the IMDb
 * GraphQL CDN's tempo doesn't block (or get blocked by) the TMDB stage.
 *
 * Two responsibilities:
 *   1. **Per-row refresh**: when a row's TMDB stage publishes `TmdbResolved`,
 *      fetch the current rating and write it back. Subscribe `onTmdbResolved`
 *      on the bus from `AppLoader`.
 *   2. **Periodic walk**: refresh every cached row hourly so live ratings
 *      stay close to imdb.com. Driven by `start()`.
 *
 * Lifecycle is owned by the caller: `AppLoader` calls `start()` to schedule
 * the periodic tick and registers `stop()` as a shutdown hook. The class
 * does not self-subscribe or self-schedule (CLAUDE.md).
 */
class ImdbRatings(cache: MovieCache, imdb: ImdbClient) extends Logging {

  // IMDb's GraphQL CDN is fast and we only have a few hundred rows; 3 workers
  // is plenty for both event-driven per-row refresh and the hourly walk's
  // burst. Smaller than the TMDB stage's pool so a sudden IMDb slowdown can't
  // starve more important fetches.
  private val Workers       = 3
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"imdb-stage-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  private val refreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "imdb-refresh"); t.setDaemon(true); t
  }
  // First run fires shortly after startup so Mongo hydration has time to
  // populate the cache before the walk reads from it.
  private val StartupDelaySeconds = 10L
  private val RefreshHours        = 1L

  // ── Event listeners ────────────────────────────────────────────────────────

  /** Bus listener: fetch the IMDb rating as soon as the TMDB stage produces an
   *  `imdbId`. Async — the publisher (the TMDB stage worker) is not blocked. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  /** Bus listener: `ImdbIdResolver` recovered the id for a TMDB-only row;
   *  fetch the rating now that we have it. */
  val onImdbIdResolved: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. No-op when the row no
   *  longer exists (was invalidated between event publish and execution). */
  private[services] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous per-row refresh by `CacheKey` — internal callers that
   *  already have the key in hand. */
  private[services] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  /** Synchronous per-row refresh by `(title, year)` — public so scripts and
   *  tests can drive a single row's IMDb refresh on the calling thread,
   *  mirroring the same overload on `MetascoreRatings` / `RottenTomatoesRatings`
   *  / `FilmwebRatings`. */
  def refreshOneSync(title: String, year: Option[Int]): Unit =
    refreshOne(cache.keyOf(title, year))

  // Look up the row, fetch the rating, write back if it changed. Skips rows
  // without an `imdbId` (TMDB-only — IMDb hasn't cross-referenced the film
  // yet). Per-row failures are swallowed (network blip, IMDb HTML challenge);
  // the next periodic tick tries again.
  private def refreshOne(key: CacheKey): Unit =
    cache.get(key).flatMap(e => e.imdbId.map(id => (e, id))).foreach { case (e, id) =>
      Try(imdb.lookup(id)).toOption.flatten match {
        case Some(rating) if !e.imdbRating.contains(rating) =>
          logger.debug(s"IMDb: ${key.cleanTitle} $id ${e.imdbRating.getOrElse("—")} → $rating")
          cache.putIfPresent(key, _.copy(imdbRating = Some(rating)))
        case _ => ()
      }
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row with an `imdbId`, refreshing its rating. Skips
   *  rows without an `imdbId` (TMDB resolved them but IMDb hasn't cross-
   *  referenced yet — the daily TMDB-retry tick re-checks those). Runs on
   *  the dedicated `imdb-refresh` thread one entry at a time. */
  private[services] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val withImdb  = snapshot.collect { case (k, e) if e.imdbId.isDefined => (k, e, e.imdbId.get) }
    val skipped   = snapshot.size - withImdb.size
    logger.info(s"IMDb refresh: starting tick over ${withImdb.size} cached row(s) with imdbId" +
                (if (skipped > 0) s" (skipping $skipped without imdbId)." else "."))
    var changed = 0
    var failed  = 0
    withImdb.foreach { case (key, enrichment, id) =>
      Try(imdb.lookup(id)) match {
        case Success(fresh) if fresh != enrichment.imdbRating =>
          logger.debug(s"IMDb refresh: ${key.cleanTitle} $id ${enrichment.imdbRating.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(imdbRating = fresh))
          changed += 1
        case Success(_) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"IMDb refresh: $id lookup failed: ${ex.getMessage}")
      }
    }
    val took = System.currentTimeMillis() - startedAt
    logger.info(s"IMDb refresh: tick done in ${took}ms — $changed changed, $failed failed, ${withImdb.size - changed - failed} unchanged.")
  }

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the periodic hourly refresh. Called from `AppLoader` — the
   *  class is otherwise lifecycle-agnostic so tests can construct it freely
   *  without a background thread firing on `new`. */
  def start(): Unit = {
    logger.info(s"IMDb refresh scheduled every ${RefreshHours}h (first run in ${StartupDelaySeconds}s).")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(refreshAll()).recover {
        case ex => logger.warn(s"IMDb refresh tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, RefreshHours * 3600, TimeUnit.SECONDS
    )
  }

  /** Drain the worker pool so in-flight upserts hit Mongo before
   *  `MovieRepo` closes its client. `AppLoader` must register this so
   *  the repo's close hook runs strictly after this returns. */
  def stop(): Unit = {
    worker.shutdown()
    refreshScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }
}
