package services.enrichment

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Try

import models.CacheKey
import services.movies.MovieCache
import services.tasks.BulkRefreshResult

/**
 * IMDb rating maintenance — the IMDb side of the `*Ratings` pattern.
 *
 * Two refresh paths (both driven by the queue):
 *   1. **Per-row refresh** (`refreshOne`): the `RatingHandler` runs this for
 *      one row once its TMDB stage has produced an `imdbId`.
 *   2. **Full-corpus refresh** (`refreshAll`): the operator-triggered bulk
 *      refresh, and the safety-net the `EnrichmentReaper` enqueues.
 *
 * What a read writes on the row is [[ImdbRowUpdates]]'s; shared entry points live in [[CacheRefresher]].
 *
 * Not a rating site on [[SiteRatings]]' port: IMDb's link (the id) is another stage's to find, and its walk differs
 * from the sites' in what it walks (rows with an id), keeps (a rating its read lacks), counts (a slot-only change) and
 * forgives (a failed details read).
 */
class ImdbRatings(
  movieCache: MovieCache,
  imdb:  ImdbClient,
  // What a read writes, its country names in the deployment's language (`ImdbRowUpdates`).
  updates: ImdbRowUpdates,
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(movieCache, cadenceRecorder) {

  override protected def sourceName: String = "IMDb"

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Look up the row, refresh the rating via the cheap `lookup` GraphQL call,
  // then fetch the full `details` to populate the SourceData(Imdb) slot
  // (synopsis, director, cast, …). Skips rows without an `imdbId` (TMDB-only —
  // IMDb hasn't cross-referenced the film yet).
  //
  // Neither call is wrapped: a FAILED read propagates to `RatingHandler`, which
  // records the attempt for /debug, leaves freshness and the cadence alone, and
  // lets the queue retry with backoff. Swallowing it here is what made IMDb's
  // 2026-07-30 CDN block look like ~47h of films that simply have no rating.
  // A film IMDb genuinely doesn't rate still returns None — see tools.ReadOutcome.
  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
      e.imdbId match {
        case None =>
          logger.info(s"IMDb: $label → no imdbId yet, skipping rating")
          None
        case Some(id) =>
          // Write the rating whenever the displayed badge value changed; the
          // details slot writes as soon as its content changes.
          val freshRating = imdb.lookup(id)
          val update      = updates.of(e, freshRating, imdb.details(id))
          logger.info(s"IMDb: $label $id → rating ${update.fresh.getOrElse("none")}" +
            update.rating.fold("")(_ => s" (changed from ${e.imdbRating.getOrElse("—")})") +
            (if (update.slotMoves) " + details slot" else ""))
          if (update.moves) cache.putIfPresent(key, update.applyTo)
          update.badge
      }
    }

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  /** Walk every cached row with an `imdbId`, refreshing its rating + the
   *  SourceData(Imdb) slot. Skips rows without an `imdbId` (TMDB resolved
   *  them but IMDb hasn't cross-referenced yet — the daily TMDB-retry tick
   *  re-checks those). */
  private[services] def refreshAll(): BulkRefreshResult = {
    val snapshot  = cache.entries
    val startedAt = tools.Stopwatch.start()
    val withImdb  = snapshot.collect { case (k, e) if e.imdbId.isDefined => (k, e, e.imdbId.get) }
    val skipped   = snapshot.size - withImdb.size
    logger.info(s"IMDb refresh: starting tick over ${withImdb.size} cached row(s) with imdbId" +
                (if (skipped > 0) s" (skipping $skipped without imdbId)." else "."))
    val changed = new AtomicInteger(0)
    val failed  = new AtomicInteger(0)
    walkEach("IMDb-refresh", withImdb) { case (key, enrichment, id) =>
      val ratingResult  = Try(imdb.lookup(id))
      val detailsResult = Try(imdb.details(id))
      ratingResult.failed.foreach { exception =>
        failed.incrementAndGet()
        logger.debug(s"IMDb refresh: $id lookup failed: ${exception.getMessage}")
      }
      val update = updates.of(enrichment, ratingResult.toOption.flatten, detailsResult.toOption.flatten)
      if (update.moves) {
        update.rating.foreach(r => logger.debug(s"IMDb refresh: ${key.cleanTitle} $id ${enrichment.imdbRating.getOrElse("—")} → $r"))
        cache.putIfPresent(key, update.applyTo)
        update.badge.foreach(badge => recordCadenceChange(key, enrichment.tmdbId, Some(badge)))
        // Each row counts once: one whose rating read failed is a failure, whatever its slot did.
        ratingResult.foreach(_ => changed.incrementAndGet())
      }
    }
    val took = startedAt.millis
    val message = s"tick done in ${took}ms — ${changed.get} changed, ${failed.get} failed, ${withImdb.size - changed.get - failed.get} unchanged."
    logger.info(s"IMDb refresh: $message")
    BulkRefreshResult.counts(walked = withImdb.size, changed = changed.get, discovered = 0, failed = failed.get, message = message)
  }
}
