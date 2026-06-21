package services.enrichment

import play.api.Logging
import services.Stoppable
import services.events.{DomainEvent, ImdbIdMissing}
import services.movies.MovieCache
import services.resolution.{ResolutionCache, ResolutionKeys}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.Try

/**
 * Recovers a missing IMDb id by querying IMDb's suggestion endpoint and writes
 * the id back to the cached row — from where the `EnrichmentReaper` picks up the
 * now-eligible IMDb rating on its next pass (no rating event is fired).
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
  // IMDb's suggestion endpoint is fast and the event-driven path is sparse (the
  // TMDB stage emits many resolutions at startup but very few carry an
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
   *  (`ImdbClient.findId`) and write it back to the cached row — the
   *  `EnrichmentReaper` then enqueues its IMDb rating on the next pass.
   *
   *  No-op when the row already carries an imdbId (a stale event raced with
   *  another resolver) or when the search returns nothing — we'd rather leave
   *  the row imdbId-less than guess a wrong id. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, searchTitle) =>
      Future(resolve(title, year, searchTitle))(using executionContext)
      ()
  }

  /** Synchronous resolution — public for tests/scripts (e.g. `Wiring.fullySyncOne`),
   *  which drive the downstream `*Ratings.refreshOneSync` themselves on the calling
   *  thread. Same work as the event-driven path; the id write is the only effect. */
  def resolveSync(title: String, year: Option[Int], searchTitle: String): Unit =
    resolve(title, year, searchTitle)

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

  private def resolve(title: String, year: Option[Int], searchTitle: String): Unit = {
    val key = cache.keyOf(title, year)
    cache.get(key).filter(_.imdbId.isEmpty).foreach { record =>
      logger.info(s"IMDb-id: looking up '${key.cleanTitle}' (${key.year.getOrElse("?")}) [search='$searchTitle']")
      // Try every year the film's cinemas report (plus the key year), sorted — the
      // mirror of the staging recovery. IMDb's release year can sit at any cinema's
      // reported (production) year, not the canonical TMDB one ("Chłopiec na krańcach
      // świata": TMDB 2026, IMDb + the cinemas 2025), so a single-key-year lookup left
      // the id flickering present/absent with arrival order (StagingOrderDeterminismSpec).
      // The sorted year set is order-independent; the per-year EXACT match still refuses
      // a same-series sibling ("Kicia Kocia w przedszkolu" 2024) at no reported year.
      val years = (record.cinemaData.values.flatMap(_.releaseYear).toSet ++ year).toSeq.sorted
      val found = (if (years.isEmpty) Seq(year) else years.map(Option(_)))
        .iterator.flatMap(y => cachedFindId(searchTitle, y)).nextOption()
      found match {
        case Some(id) =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → resolved $id")
          // putIfPresent so a concurrent `cache.invalidate` between the lookup and
          // the write-back can't resurrect the row.
          cache.putIfPresent(key, _.copy(imdbId = Some(id)))
        case None =>
          logger.info(s"IMDb-id: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match [search='$searchTitle']")
      }
    }
  }

  /** Drain the worker pool so in-flight id write-backs have completed before the
   *  caller moves on. Waits for the queue to drain, not a fixed window — the
   *  bounded cap was returning before real-network suggestion lookups finished
   *  and `cascadeDrainOrder`'s next entry was shutting down a pool that still had
   *  inbound work coming. */
  def stop(): Unit = {
    executionContext.shutdown()
    while (!executionContext.isTerminated) executionContext.awaitTermination(1, TimeUnit.HOURS)
  }
}
