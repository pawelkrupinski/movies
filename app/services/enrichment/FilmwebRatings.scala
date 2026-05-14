package services.enrichment

import play.api.Logging
import services.events.{DomainEvent, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

/**
 * Filmweb data maintenance — the Filmweb counterpart of `ImdbRatings`,
 * `RottenTomatoesRatings`, and `MetascoreRatings`.
 *
 * Two responsibilities:
 *   1. **Per-row refresh**: when the TMDB stage publishes `TmdbResolved`,
 *      fetch Filmweb data for that row. Subscribe `onTmdbResolved` on the
 *      bus from `AppLoader`.
 *   2. **Periodic walk**: refresh every cached row hourly. For rows that
 *      already have a `filmwebUrl`, the walk does the cheap rating-only
 *      lookup (one HTTP); for rows without a URL it does the full
 *      `filmweb.lookup` (search + info + rating).
 *
 * Lifecycle is owned by the caller (`AppLoader` calls `start()` and
 * registers `stop()` as a shutdown hook). The class never self-subscribes
 * or self-schedules — see CLAUDE.md.
 *
 * Filmweb has tighter rate limits than the other services (CLAUDE.md notes
 * "5 workers comfortable, more risks soft-blocks"); we stay at 3 here since
 * the per-row work is heavier (potentially search + info + rating).
 */
class FilmwebRatings(cache: EnrichmentCache, filmweb: FilmwebClient) extends Logging {

  private val Workers       = 3
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"filmweb-stage-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  private val refreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "filmweb-refresh"); t.setDaemon(true); t
  }
  // Stagger startup against the other rating services (IMDb @10s, RT @10s,
  // Metascore @30s) so the first-tick bursts don't pile up.
  private val StartupDelaySeconds = 45L
  private val RefreshHours        = 1L

  // ── Event listener ─────────────────────────────────────────────────────────

  /** Bus listener: fetch Filmweb data as soon as the TMDB stage publishes a
   *  `TmdbResolved` for this `(title, year)`. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. */
  private[enrichment] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous version of `schedule` — handy for scripts and tests. */
  private[enrichment] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  /** Synchronous refresh by `(title, year)` — public entry point for scripts.
   *  Looks up the row's cache key the same way the rest of the pipeline does. */
  def refreshOneSync(title: String, year: Option[Int]): Unit =
    refreshOne(cache.keyOf(title, year))

  // Look up Filmweb data for the row, write back any changes. Two paths:
  //   - Row already has filmwebUrl → cheap: rating-only refresh via the
  //     URL's id. Skips the search/info round-trips that the original
  //     resolution paid for.
  //   - Row doesn't have filmwebUrl → expensive: full lookup (search →
  //     pick best year-match → rating). Populates the URL too.
  // Per-row failures are swallowed (network blip, Filmweb soft-block); the
  // next periodic tick tries again.
  private def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      e.filmwebUrl match {
        case Some(url) =>
          Try(filmweb.ratingFor(url)).toOption.flatten match {
            case Some(rating) if !e.filmwebRating.contains(rating) =>
              logger.debug(s"Filmweb: ${key.cleanTitle} $url ${e.filmwebRating.getOrElse("—")} → $rating")
              cache.put(key, e.copy(filmwebRating = Some(rating)))
            case _ => ()
          }
        case None =>
          Try(filmweb.lookup(key.cleanTitle, key.year)).toOption.flatten.foreach { fw =>
            logger.debug(s"Filmweb: ${key.cleanTitle} discovered ${fw.url} rating=${fw.rating.getOrElse("—")}")
            cache.put(key, e.copy(filmwebUrl = Some(fw.url), filmwebRating = fw.rating))
          }
      }
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row and refresh its Filmweb data. Rows with a URL
   *  get the cheap rating-only refresh; rows without one get the expensive
   *  full-lookup. The latter group is small in practice — only films
   *  Filmweb didn't surface at first enrichment. */
  private[enrichment] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.filmwebUrl.isDefined }
    logger.info(s"Filmweb refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → rating-only, ${missingUrl.size} without → full lookup).")
    var changed       = 0
    var failed        = 0
    var urlDiscovered = 0

    withUrl.foreach { case (key, enrichment) =>
      val url = enrichment.filmwebUrl.get
      Try(filmweb.ratingFor(url)) match {
        case Success(fresh) if fresh != enrichment.filmwebRating =>
          logger.debug(s"Filmweb refresh: ${key.cleanTitle} $url ${enrichment.filmwebRating.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.put(key, enrichment.copy(filmwebRating = fresh))
          changed += 1
        case Success(_) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"Filmweb refresh: $url lookup failed: ${ex.getMessage}")
      }
    }

    missingUrl.foreach { case (key, enrichment) =>
      Try(filmweb.lookup(key.cleanTitle, key.year)) match {
        case Success(Some(fw)) =>
          urlDiscovered += 1
          cache.put(key, enrichment.copy(filmwebUrl = Some(fw.url), filmwebRating = fw.rating))
        case Success(None) => ()
        case Failure(ex) =>
          failed += 1
          logger.debug(s"Filmweb refresh: ${key.cleanTitle} full-lookup failed: ${ex.getMessage}")
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"Filmweb refresh: tick done in ${took}ms — $changed rating(s) changed, " +
                s"$urlDiscovered URL(s) newly discovered, $failed failed.")
  }

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the periodic hourly refresh. Called from `AppLoader`. */
  def start(): Unit = {
    logger.info(s"Filmweb refresh scheduled every ${RefreshHours}h (first run in ${StartupDelaySeconds}s).")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(refreshAll()).recover {
        case ex => logger.warn(s"Filmweb refresh tick failed: ${ex.getMessage}")
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
