package services.enrichment

import play.api.Logging
import services.Stoppable
import services.events.{DomainEvent, EventBus, ImdbIdMissing, ImdbIdResolved}
import services.movies.MovieCache
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
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
class ImdbIdResolver(cache: MovieCache, imdb: ImdbClient, bus: EventBus) extends Stoppable with Logging {

  // IMDb's suggestion endpoint is fast; one or two workers is plenty for the
  // event-driven path (TmdbResolved fans out hundreds of events at startup but
  // very few of them carry an `ImdbIdMissing`).
  private val worker = DaemonExecutors.fixedPool("imdb-id-resolver", 2)

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
      worker.execute(() => resolve(title, year, searchTitle, publishEvent = true))
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

  private def resolve(
    title:        String,
    year:         Option[Int],
    searchTitle:  String,
    publishEvent: Boolean
  ): Unit = {
    val key = cache.keyOf(title, year)
    cache.get(key).filter(_.imdbId.isEmpty).foreach { _ =>
      Try(imdb.findId(searchTitle, year)).toOption.flatten match {
        case Some(id) =>
          logger.info(s"IMDb id resolved via search: ${key.cleanTitle} (${key.year.getOrElse("?")}) → $id")
          // putIfPresent so a concurrent `cache.invalidate` happening between
          // event publish and id resolution can't resurrect the row.
          val wrote = cache.putIfPresent(key, _.copy(imdbId = Some(id)))
          if (wrote && publishEvent) bus.publish(ImdbIdResolved(title, year, id))
        case None =>
          logger.debug(s"IMDb search returned no match for ${key.cleanTitle} (${key.year.getOrElse("?")}) [search='$searchTitle']")
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
    worker.shutdown()
    while (!worker.isTerminated) worker.awaitTermination(1, TimeUnit.HOURS)
  }
}
