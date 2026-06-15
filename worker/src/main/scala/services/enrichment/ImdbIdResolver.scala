package services.enrichment

import play.api.Logging
import services.Stoppable
import services.events.{DomainEvent, EventBus, ImdbIdMissing, ImdbIdResolved}
import services.movies.MovieCache
import services.resolution.{ResolutionCache, ResolutionKeys}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.Try

/**
 * Recovers a missing IMDb id by querying IMDb's suggestion endpoint, writes
 * the id back to the cached row, then publishes `ImdbIdResolved` so other
 * services (rating fetchers, score scrapers) can chain off the new id.
 *
 * Split out of `ImdbRatings` so rating maintenance doesn't entangle with
 * id discovery — `ImdbRatings` now only deals with the rating lifecycle and
 * can rely on `imdbId` being already set on the rows it touches.
 *
 * Async — runs on its own worker pool so the publisher (the TMDB stage
 * worker) isn't blocked on IMDb. Lifecycle owned by `AppLoader`.
 */
class ImdbIdResolver(
  cache: MovieCache,
  imdb:  ImdbClient,
  bus:   EventBus,
  // IMDb's suggestion endpoint is fast and the event-driven path is sparse
  // (TmdbResolved fans out hundreds of events at startup but very few carry an
  // `ImdbIdMissing`). Virtual threads keep per-task overhead trivial; the rate
  // cap, if any, sits at the HTTP layer. Defaults to a dedicated unbounded
  // pool (tests/scripts unchanged); `Wiring` injects a shared-budget EC. See
  // `SharedExecutionBudget`.
  executionContext:    ExecutionContextExecutorService = DaemonExecutors.virtualThreadEC("imdb-id-resolver"),
  // Caches the IMDb suggestion lookup keyed by (search title, year), so the same
  // search resolves once for 24h across the staging and event-driven paths.
  // Defaults to passthrough so unit specs resolve live unless they wire one.
  imdbIdCache: ResolutionCache = ResolutionCache.passthrough
) extends Stoppable with Logging {

  /** Cached IMDb-id lookup shared by both call sites. Hits-only — a no-match
   *  re-queries next time. */
  private def cachedFindId(searchTitle: String, year: Option[Int]): Option[String] =
    imdbIdCache.getOrResolve(ResolutionKeys.imdb(searchTitle, year))(Try(imdb.findId(searchTitle, year)).toOption.flatten)

  /** Bus listener: when the TMDB stage resolved a film but TMDB has no IMDb
   *  cross-reference for it, recover the id via IMDb's suggestion endpoint
   *  (`ImdbClient.findId`), write it back to the cached row, and publish
   *  `ImdbIdResolved` so downstream rating fetchers refresh.
   *
   *  No-op when the row already carries an imdbId (a stale event raced with
   *  another resolver) or when the search returns nothing — we'd rather leave
   *  the row imdbId-less than guess a wrong id. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, searchTitle) =>
      Future(resolve(title, year, searchTitle, publishEvent = true))(using executionContext)
      ()
  }

  /** Synchronous resolution — public for tests/scripts. Doesn't publish
   *  `ImdbIdResolved` on the bus: sync callers (e.g.
   *  `Wiring.fullySyncOne`) drive the downstream `*Ratings.refreshOneSync`
   *  themselves on the calling thread, so publishing would just create
   *  noisy `RejectedExecutionException` warnings on listener pools that
   *  the caller has already shut down. Production goes through
   *  `onImdbIdMissing` which does publish. */
  def resolveSync(title: String, year: Option[Int], searchTitle: String): Unit =
    resolve(title, year, searchTitle, publishEvent = false)

  /** Cache-free id lookup: query IMDb's suggestion endpoint for `searchTitle`
   *  and return the id, or None on no match / a transient failure. Used by the
   *  staging promoter to recover a missing imdbId INLINE — a staging row isn't in
   *  the cache, so the event-driven `onImdbIdMissing` (which reads + `putIfPresent`s
   *  the cache) can't reach it; recovering here folds the row already carrying the
   *  id, the same end state the direct path's `ImdbIdMissing` chain produces. */
  def findIdFor(searchTitle: String, year: Option[Int]): Option[String] = {
    logger.info(s"IMDb-id (staging): looking up [search='$searchTitle'] (${year.getOrElse("?")})")
    val id = cachedFindId(searchTitle, year)
    logger.info(s"IMDb-id (staging): [search='$searchTitle'] (${year.getOrElse("?")}) → ${id.getOrElse("no match")}")
    id
  }

  private def resolve(
    title:        String,
    year:         Option[Int],
    searchTitle:  String,
    publishEvent: Boolean
  ): Unit = {
    val key = cache.keyOf(title, year)
    cache.get(key).filter(_.imdbId.isEmpty).foreach { _ =>
      logger.info(s"IMDb-id: looking up '${key.cleanTitle}' (${key.year.getOrElse("?")}) [search='$searchTitle']")
      cachedFindId(searchTitle, year) match {
        case Some(id) =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → resolved $id")
          // putIfPresent so a concurrent `cache.invalidate` happening between
          // event publish and id resolution can't resurrect the row.
          val wrote = cache.putIfPresent(key, _.copy(imdbId = Some(id)))
          if (wrote && publishEvent) bus.publish(ImdbIdResolved(title, year, id))
        case None =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match [search='$searchTitle']")
      }
    }
  }

  /** Drain the worker pool so in-flight ImdbIdResolved publishes (and
   *  their downstream listener dispatches into `ImdbRatings.worker`)
   *  have fired before the caller moves on. Waits for the queue to
   *  drain, not a fixed 15-s window — the bounded cap was returning
   *  before real-network suggestion lookups finished and
   *  `cascadeDrainOrder`'s next entry was shutting down a pool that
   *  still had inbound work coming. */
  def stop(): Unit = {
    executionContext.shutdown()
    while (!executionContext.isTerminated) executionContext.awaitTermination(1, TimeUnit.HOURS)
  }
}
