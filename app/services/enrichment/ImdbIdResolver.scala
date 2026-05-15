package services.enrichment

import play.api.Logging
import services.events.{DomainEvent, EventBus, ImdbIdMissing, ImdbIdResolved}
import services.movies.MovieCache

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
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
class ImdbIdResolver(cache: MovieCache, imdb: ImdbClient, bus: EventBus) extends Logging {

  // IMDb's suggestion endpoint is fast; one or two workers is plenty for the
  // event-driven path (TmdbResolved fans out hundreds of events at startup but
  // very few of them carry an `ImdbIdMissing`).
  private val Workers       = 2
  private val workerCounter = new AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(Workers, { r: Runnable =>
    val t = new Thread(r, s"imdb-id-resolver-${workerCounter.incrementAndGet()}")
    t.setDaemon(true); t
  })

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
      worker.execute(() => resolve(title, year, searchTitle))
  }

  /** Synchronous resolution — public for tests/scripts. Production goes
   *  through the `onImdbIdMissing` listener. */
  private[services] def resolveSync(title: String, year: Option[Int], searchTitle: String): Unit =
    resolve(title, year, searchTitle)

  private def resolve(title: String, year: Option[Int], searchTitle: String): Unit = {
    val key = cache.keyOf(title, year)
    cache.get(key).foreach { row =>
      if (row.imdbId.isDefined) {
        // Stale event — another resolver beat us to it.
        return
      }
      Try(imdb.findId(searchTitle, year)).toOption.flatten match {
        case Some(id) =>
          logger.info(s"IMDb id resolved via search: ${key.cleanTitle} (${key.year.getOrElse("?")}) → $id")
          // putIfPresent so a concurrent `cache.invalidate` happening between
          // event publish and id resolution can't resurrect the row.
          val wrote = cache.putIfPresent(key, _.copy(imdbId = Some(id)))
          if (wrote) bus.publish(ImdbIdResolved(title, year, id))
        case None =>
          logger.debug(s"IMDb search returned no match for ${key.cleanTitle} (${key.year.getOrElse("?")}) [search='$searchTitle']")
      }
    }
  }

  /** Drain the worker pool — `AppLoader` registers this so `MovieRepo`
   *  closes its client strictly after in-flight writes have landed. */
  def stop(): Unit = {
    worker.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }
}
