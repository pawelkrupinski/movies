package services.enrichment

import play.api.Logging
import services.events.{DomainEvent, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

/**
 * Metascore (Metacritic critic aggregate, 0–100) maintenance — same shape as
 * `ImdbRatings`. Reads the row's stored `metacriticUrl` and asks
 * `MetacriticClient.metascoreFor` for the current score; writes back when it
 * changes.
 *
 * Two responsibilities:
 *   1. **Per-row refresh**: when the TMDB stage publishes `TmdbResolved`,
 *      refresh the score for that row. Subscribe `onTmdbResolved` on the bus
 *      from `AppLoader`.
 *   2. **Periodic walk**: refresh every cached row hourly so the scores stay
 *      close to metacritic.com. Driven by `start()`.
 *
 * Skips rows without a `metacriticUrl` — there's nothing to scrape. Skips
 * (silently) when MC's JSON-LD has no `aggregateRating` (typical for films
 * MC hasn't aggregated yet). Lifecycle owned by the caller; the class doesn't
 * self-subscribe or self-schedule (CLAUDE.md).
 */
class MetascoreRatings(cache: EnrichmentCache, metacritic: MetacriticClient) extends Logging {

  // MC's pages are heavier than IMDb's GraphQL endpoint and the hourly walk
  // makes hundreds of requests. 3 workers keeps us comfortably under MC's
  // tolerated rate while still parallelising the I/O wait.
  private val Workers       = 3
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"metascore-stage-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  private val refreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "metascore-refresh"); t.setDaemon(true); t
  }
  // Stagger the startup tick so we don't race the IMDb refresh — both walk
  // the same cache, and serialising the bursts avoids fan-in on Mongo
  // upserts.
  private val StartupDelaySeconds = 30L
  private val RefreshHours        = 1L

  // ── Event listener ─────────────────────────────────────────────────────────

  /** Bus listener: fetch the Metascore as soon as the TMDB stage produces an
   *  imdbId (and, by extension, a `metacriticUrl` if MC has the film). */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. No-op when the row no
   *  longer exists or has no `metacriticUrl`. */
  private[enrichment] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous version of `schedule` — used by callers that want a
   *  single-shot answer (e.g. backfill scripts). */
  private[enrichment] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  // Look up the row, scrape the score from MC, write back if it changed.
  // Skips rows without a `metacriticUrl` (MC doesn't index every film).
  // Per-row failures are swallowed (network blip, Cloudflare challenge) —
  // the periodic tick tries again.
  private def refreshOne(key: CacheKey): Unit =
    cache.get(key).flatMap(e => e.metacriticUrl.map(url => (e, url))).foreach { case (e, url) =>
      Try(metacritic.metascoreFor(url)).toOption.flatten match {
        case Some(score) if !e.metascore.contains(score) =>
          logger.debug(s"Metascore: ${key.cleanTitle} $url ${e.metascore.getOrElse("—")} → $score")
          cache.put(key, e.copy(metascore = Some(score)))
        case _ => ()
      }
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row with a `metacriticUrl`, refreshing its Metascore.
   *  Runs on the dedicated `metascore-refresh` thread one entry at a time —
   *  MC pages are heavy enough that parallelism risks rate-limit pushback. */
  private[enrichment] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val withUrl   = snapshot.collect { case (k, e) if e.metacriticUrl.isDefined => (k, e, e.metacriticUrl.get) }
    val skipped   = snapshot.size - withUrl.size
    logger.info(s"Metascore refresh: starting tick over ${withUrl.size} cached row(s) with MC URL" +
                (if (skipped > 0) s" (skipping $skipped without MC URL)." else "."))
    var changed = 0
    var failed  = 0
    withUrl.foreach { case (key, enrichment, url) =>
      Try(metacritic.metascoreFor(url)) match {
        case Success(fresh) if fresh != enrichment.metascore =>
          logger.debug(s"Metascore refresh: ${key.cleanTitle} $url ${enrichment.metascore.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.put(key, enrichment.copy(metascore = fresh))
          changed += 1
        case Success(_) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"Metascore refresh: $url lookup failed: ${ex.getMessage}")
      }
    }
    val took = System.currentTimeMillis() - startedAt
    logger.info(s"Metascore refresh: tick done in ${took}ms — $changed changed, $failed failed, ${withUrl.size - changed - failed} unchanged.")
  }

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the periodic hourly refresh. Called from `AppLoader`. */
  def start(): Unit = {
    logger.info(s"Metascore refresh scheduled every ${RefreshHours}h (first run in ${StartupDelaySeconds}s).")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(refreshAll()).recover {
        case ex => logger.warn(s"Metascore refresh tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, RefreshHours * 3600, TimeUnit.SECONDS
    )
  }

  /** Drain the worker pool so in-flight upserts hit Mongo before
   *  `EnrichmentRepo` closes its client. */
  def stop(): Unit = {
    worker.shutdown()
    refreshScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }
}
