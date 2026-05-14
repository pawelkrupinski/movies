package services.enrichment

import clients.TmdbClient
import play.api.Logging
import services.events.{DomainEvent, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

/**
 * Rotten Tomatoes side of enrichment — owns BOTH:
 *   - `rottenTomatoesUrl` discovery (slug probe with year-suffix preference,
 *     cleanTitle fallback, lazy `englishTitle` fallback for non-English films).
 *   - `rottenTomatoes` (Tomatometer percentage) scrape from the resolved URL.
 *
 * Mirrors `FilmwebRatings` / `MetascoreRatings`: per-row refresh on
 * `TmdbResolved`, plus a periodic hourly walk that discovers missing URLs and
 * refreshes existing scores. Lifecycle owned by `AppLoader`; the class never
 * self-subscribes or self-schedules (CLAUDE.md).
 *
 * URL resolution needs TMDB data (release year, English title) that the
 * Enrichment row alone doesn't carry — we hit `tmdb.details(tmdbId)` lazily
 * only when a row needs URL discovery.
 */
class RottenTomatoesRatings(
  cache: EnrichmentCache,
  tmdb:  TmdbClient,
  rt:    RottenTomatoesClient
) extends Logging {

  // 5 workers comfortably under CLAUDE.md's "5–10" band for undocumented
  // services. Each refresh is a single GET to a /m/ page; smaller than the
  // TMDB stage's pool so an RT slowdown can't starve more important fetches.
  private val Workers       = 5
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"rt-stage-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  private val refreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "rt-refresh"); t.setDaemon(true); t
  }
  // Stagger startup against IMDb (10s) so first-tick bursts don't pile up.
  private val StartupDelaySeconds = 15L
  private val RefreshHours        = 1L

  // ── Event listener ─────────────────────────────────────────────────────────

  /** Bus listener: discover the RT URL (if missing) and refresh the Tomatometer
   *  as soon as the TMDB stage produces a row. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. */
  private[enrichment] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous version of `schedule` — handy for scripts and tests. */
  private[enrichment] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  /** Synchronous refresh by `(title, year)` — public entry point for scripts. */
  def refreshOneSync(title: String, year: Option[Int]): Unit =
    refreshOne(cache.keyOf(title, year))

  // Two paths, mirroring FilmwebRatings / MetascoreRatings:
  //   - URL already known → cheap: scrape Tomatometer, write back if changed.
  //   - URL missing       → expensive: probe RT slug variants (with year-
  //     suffix preference + English-title fallback for non-English films),
  //     write the URL, then scrape the score.
  // Per-row failures are swallowed; the next periodic tick tries again.
  private def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      val urlOpt = e.rottenTomatoesUrl.orElse(resolveAndPersistUrl(key, e))
      urlOpt.foreach(url => refreshScoreFromUrl(key, e, url))
    }

  private def resolveAndPersistUrl(key: CacheKey, e: models.Enrichment): Option[String] =
    e.tmdbId.flatMap { tmdbId =>
      val linkTitle  = e.originalTitle.getOrElse(key.cleanTitle)
      val rtFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None
      val details    = tmdb.details(tmdbId)
      val year       = details.flatMap(_.releaseYear)

      val primary = Try(rt.urlFor(linkTitle, rtFallback, year)).toOption.flatten
      val resolved = primary.orElse {
        val englishTitle = details.flatMap(_.englishTitle)
          .filterNot(_.equalsIgnoreCase(linkTitle))
          .filterNot(t => rtFallback.exists(_.equalsIgnoreCase(t)))
        englishTitle.flatMap(t => Try(rt.urlFor(t, None, year)).toOption.flatten)
      }

      resolved.foreach { url =>
        logger.debug(s"RT: ${key.cleanTitle} discovered $url")
        cache.put(key, e.copy(rottenTomatoesUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.Enrichment, url: String): Unit =
    Try(rt.scoreFor(url)).toOption.flatten match {
      case Some(score) if !e.rottenTomatoes.contains(score) =>
        val current = cache.get(key).getOrElse(e)
        logger.debug(s"RT: ${key.cleanTitle} $url ${current.rottenTomatoes.getOrElse("—")} → $score")
        cache.put(key, current.copy(rottenTomatoes = Some(score)))
      case _ => ()
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row. Rows with a `rottenTomatoesUrl` get a cheap
   *  Tomatometer refresh; rows without one get the full URL-discovery probe
   *  (and a score refresh if discovery succeeds). */
  private[enrichment] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.rottenTomatoesUrl.isDefined }
    logger.info(s"RT refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → score-only, ${missingUrl.size} without → URL discovery).")
    var changed       = 0
    var failed        = 0
    var urlDiscovered = 0

    withUrl.foreach { case (key, enrichment) =>
      val url = enrichment.rottenTomatoesUrl.get
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

    missingUrl.foreach { case (key, enrichment) =>
      resolveAndPersistUrl(key, enrichment).foreach { url =>
        urlDiscovered += 1
        cache.get(key).foreach(refreshScoreFromUrl(key, _, url))
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"RT refresh: tick done in ${took}ms — $changed score(s) changed, " +
                s"$urlDiscovered URL(s) newly discovered, $failed failed.")
  }

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the periodic hourly refresh. Called from `AppLoader`. */
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
   *  `EnrichmentRepo` closes its client. */
  def stop(): Unit = {
    worker.shutdown()
    refreshScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }
}
