package services.enrichment

import services.movies.{CacheKey, MovieCache}
import services.events.{DomainEvent, ImdbIdResolved, TmdbResolved}

import scala.util.{Failure, Success, Try}

/**
 * IMDb rating maintenance — the IMDb side of the `*Ratings` pattern.
 *
 * Two responsibilities:
 *   1. **Per-row refresh**: when a row's TMDB stage publishes `TmdbResolved`
 *      (or `ImdbIdResolver` recovers an id via `ImdbIdResolved`), fetch the
 *      current rating and write it back. Wire on the bus from `AppLoader`.
 *   2. **Periodic walk**: refresh every cached row hourly so live ratings
 *      stay close to imdb.com.
 *
 * Shared lifecycle + worker plumbing lives in [[PeriodicCacheRefresher]].
 */
class ImdbRatings(cache: MovieCache, imdb: ImdbClient)
    extends PeriodicCacheRefresher(
      name                = "IMDb",
      // IMDb's GraphQL CDN is fast and we only have a few hundred rows; 3
      // workers is plenty. Smaller than the TMDB stage's pool so a sudden
      // IMDb slowdown can't starve more important fetches.
      workers             = 3,
      // First run fires shortly after startup so Mongo hydration has time
      // to populate the cache before the walk reads from it.
      startupDelaySeconds = 10L,
      refreshHours        = 1L,
      cache               = cache
    ) {

  // ── Event listeners ────────────────────────────────────────────────────────

  /** Bus listener: fetch the IMDb rating as soon as the TMDB stage produces an
   *  `imdbId`. */
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  /** Bus listener: `ImdbIdResolver` recovered the id for a TMDB-only row;
   *  fetch the rating now that we have it. */
  val onImdbIdResolved: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdResolved(title, year, _) => schedule(cache.keyOf(title, year))
  }

  // ── Per-row work ───────────────────────────────────────────────────────────

  // Look up the row, fetch the rating, write back if it changed. Skips rows
  // without an `imdbId` (TMDB-only — IMDb hasn't cross-referenced the film
  // yet). Per-row failures are swallowed (network blip, IMDb HTML challenge);
  // the next periodic tick tries again.
  protected def refreshOne(key: CacheKey): Unit =
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
   *  referenced yet — the daily TMDB-retry tick re-checks those). */
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
}
