package services.enrichment

import clients.TmdbClient
import play.api.Logging
import services.events.{DomainEvent, TmdbResolved}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.{Failure, Success, Try}

/**
 * Metacritic side of enrichment — owns BOTH:
 *   - `metacriticUrl` discovery (slug probe + cleanTitle fallback + lazy
 *     `englishTitle` fallback for non-English films).
 *   - `metascore` scrape from the resolved URL.
 *
 * Mirrors `FilmwebRatings`'s shape: per-row refresh on `TmdbResolved`, plus a
 * periodic hourly walk that discovers missing URLs and refreshes existing
 * scores. Lifecycle owned by `AppLoader`; the class never self-subscribes or
 * self-schedules (CLAUDE.md).
 *
 * URL resolution needs TMDB data (release year, English title) that the
 * Enrichment row alone doesn't carry — we hit `tmdb.details(tmdbId)` lazily
 * for rows that need URL discovery, and never for rows that already have a
 * canonical URL.
 */
class MetascoreRatings(
  cache:      EnrichmentCache,
  tmdb:       TmdbClient,
  metacritic: MetacriticClient
) extends Logging {

  // 3 workers — MC pages are heavier than IMDb's GraphQL endpoint, and the
  // hourly walk makes hundreds of requests. 3 keeps us under MC's tolerated
  // rate while still parallelising the I/O wait.
  private val Workers       = 3
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"metascore-stage-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

  private val refreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "metascore-refresh"); t.setDaemon(true); t
  }
  // Stagger the startup tick so we don't race the IMDb / RT refreshes — they
  // all walk the same cache and serialising the bursts avoids Mongo fan-in.
  private val StartupDelaySeconds = 30L
  private val RefreshHours        = 1L

  // ── Event listener ─────────────────────────────────────────────────────────

  /** Bus listener: discover the MC URL (if missing) and refresh the metascore
   *  as soon as the TMDB stage produces a row. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. No-op when the row no
   *  longer exists or can't be resolved. */
  private[enrichment] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous version of `schedule` — used by backfill scripts and tests. */
  private[enrichment] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  /** Synchronous refresh by `(title, year)` — public entry point for scripts.
   *  Looks up the row's cache key the same way the rest of the pipeline does. */
  def refreshOneSync(title: String, year: Option[Int]): Unit =
    refreshOne(cache.keyOf(title, year))

  // Two paths, mirroring FilmwebRatings:
  //   - URL already known → cheap: scrape metascore, write back if changed.
  //   - URL missing       → expensive: probe MC slug variants (with TMDB
  //     details for English title + year disambiguation), write the URL,
  //     then scrape the metascore.
  // Per-row failures are swallowed; the next periodic tick tries again.
  private def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      val urlOpt = e.metacriticUrl.orElse(resolveAndPersistUrl(key, e))
      urlOpt.foreach(url => refreshScoreFromUrl(key, e, url))
    }

  // Probe MC for a canonical /movie/ URL using TMDB's title data; write it
  // back to the cache when found. Returns the new URL (or None if MC didn't
  // index the film).
  private def resolveAndPersistUrl(key: CacheKey, e: models.Enrichment): Option[String] =
    e.tmdbId.flatMap { tmdbId =>
      val linkTitle  = e.originalTitle.getOrElse(key.cleanTitle)
      val mcFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None
      val details    = tmdb.details(tmdbId)
      val year       = details.flatMap(_.releaseYear)

      val primary = Try(metacritic.urlFor(linkTitle, mcFallback, year)).toOption.flatten
      val resolved = primary.orElse {
        // English-title fallback for non-English films — only consult when
        // primary + cleanTitle both miss.
        val englishTitle = details.flatMap(_.englishTitle)
          .filterNot(_.equalsIgnoreCase(linkTitle))
          .filterNot(t => mcFallback.exists(_.equalsIgnoreCase(t)))
        englishTitle.flatMap(t => Try(metacritic.urlFor(t, None, year)).toOption.flatten)
      }

      resolved.foreach { url =>
        logger.debug(s"Metascore: ${key.cleanTitle} discovered $url")
        cache.put(key, e.copy(metacriticUrl = Some(url)))
      }
      resolved
    }

  private def refreshScoreFromUrl(key: CacheKey, e: models.Enrichment, url: String): Unit =
    Try(metacritic.metascoreFor(url)).toOption.flatten match {
      case Some(score) if !e.metascore.contains(score) =>
        // Re-read the cached row in case `resolveAndPersistUrl` just wrote
        // the URL — we want to merge our score onto the freshest version.
        val current = cache.get(key).getOrElse(e)
        logger.debug(s"Metascore: ${key.cleanTitle} $url ${current.metascore.getOrElse("—")} → $score")
        cache.put(key, current.copy(metascore = Some(score)))
      case _ => ()
    }

  // ── Periodic walk ──────────────────────────────────────────────────────────

  /** Walk every cached row. Rows with a `metacriticUrl` get a cheap score
   *  refresh; rows without one get the full URL-discovery probe (and then a
   *  score refresh if discovery succeeds). Per-row failures are logged at
   *  debug — one bad row can't poison the whole tick. */
  private[enrichment] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.metacriticUrl.isDefined }
    logger.info(s"Metascore refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → score-only, ${missingUrl.size} without → URL discovery).")
    var changed       = 0
    var failed        = 0
    var urlDiscovered = 0

    withUrl.foreach { case (key, enrichment) =>
      val url = enrichment.metacriticUrl.get
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

    missingUrl.foreach { case (key, enrichment) =>
      resolveAndPersistUrl(key, enrichment).foreach { url =>
        urlDiscovered += 1
        // Use the post-write row so the score copy doesn't clobber the URL.
        cache.get(key).foreach(refreshScoreFromUrl(key, _, url))
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"Metascore refresh: tick done in ${took}ms — $changed score(s) changed, " +
                s"$urlDiscovered URL(s) newly discovered, $failed failed.")
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
