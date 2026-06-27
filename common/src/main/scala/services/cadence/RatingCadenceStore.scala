package services.cadence

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Updates}
import com.mongodb.client.model.UpdateOptions
import play.api.Logging

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Per-(rating source, film) change history, keyed by the SAME dedup string the
 * freshness store and queue use (`<site>|tmdb:<id>`). Drives the adaptive refresh
 * interval: [[services.tasks.DueWindow]] reads `statsFor` → [[RatingCadence.intervalFor]]
 * to decide how long until a film's next refresh of that source, and the
 * [[services.tasks.RatingHandler]] calls `record` after each refresh with whether
 * the displayed value changed.
 *
 * The fold itself ([[RatingCadence.record]]) is pure and shared, so the real and
 * fake stores can't disagree on the backoff rule — they differ only in WHERE the
 * stats live.
 */
trait RatingCadenceStore {
  /** Current stats for `key`, or None if this source has never refreshed this film. */
  def statsFor(key: String): Option[RatingChangeStats]

  /** Persist the updated stats for `key`. */
  protected def persist(key: String, stats: RatingChangeStats): Unit

  /** Fold one refresh outcome into `key`'s stats and store it. `changed` is
   *  whether the displayed value differs from the previous scrape. Single-writer
   *  (one worker, and the queue never runs two refreshes of the same key at once),
   *  so the read-then-write needs no lock. */
  final def record(key: String, changed: Boolean, at: Instant = Instant.now()): RatingChangeStats = {
    val next = RatingCadence.record(statsFor(key), changed, at)
    persist(key, next)
    next
  }

  def close(): Unit = ()
}

/** In-memory store for tests and Mongo-less local dev. */
class InMemoryRatingCadenceStore extends RatingCadenceStore {
  private val stats = new ConcurrentHashMap[String, RatingChangeStats]()
  override def statsFor(key: String): Option[RatingChangeStats] = Option(stats.get(key))
  override protected def persist(key: String, s: RatingChangeStats): Unit = { stats.put(key, s); () }
}

/**
 * Mongo-backed store, collection `rating_cadence` with documents
 * `{ _id: <key>, unchangedStreak, windowChecks, windowChanges,
 *    windowStartedAt: ISODate, lastCheckedAt: ISODate }`.
 *
 * Mirrors [[services.freshness.MongoFreshnessStore]]: reads come from an in-memory
 * mirror (the reaper consults it for every cached row × source each tick), hydrated
 * once at boot in a daemon thread and kept current by `record` writing through to
 * both the map and Mongo. A cold mirror just yields the base interval (films refresh
 * a bit MORE often until it warms — safe, never less), so no readiness gate is
 * needed. A `null` db disables persistence; the mirror still works in-process.
 */
class MongoRatingCadenceStore(db: Option[MongoDatabase] = None) extends RatingCadenceStore with Logging {
  private val mirror = new ConcurrentHashMap[String, RatingChangeStats]()
  // Relaxed `{w:1, j:false}` like freshness: cadence stats are a cache — a lost
  // write just means the film briefly reverts toward the base interval.
  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection("rating_cadence").withWriteConcern(services.tasks.MongoTaskQueue.QueueWriteConcern))

  coll.foreach { c =>
    val thread = new Thread(() => hydrate(c), "rating-cadence-init")
    thread.setDaemon(true)
    thread.start()
  }

  override def statsFor(key: String): Option[RatingChangeStats] = Option(mirror.get(key))

  override protected def persist(key: String, s: RatingChangeStats): Unit = {
    mirror.put(key, s)
    coll.foreach { c =>
      // Fire-and-forget: a write failure must never break the refresh loop. The
      // mirror already holds the value; the next restart just won't see it.
      Try {
        c.updateOne(
          Filters.eq("_id", key),
          Updates.combine(
            Updates.set("unchangedStreak", s.unchangedStreak),
            Updates.set("windowChecks", s.windowChecks),
            Updates.set("windowChanges", s.windowChanges),
            Updates.set("windowStartedAt", new java.util.Date(s.windowStartedAt.toEpochMilli)),
            Updates.set("lastCheckedAt", new java.util.Date(s.lastCheckedAt.toEpochMilli))
          ),
          new UpdateOptions().upsert(true)
        ).subscribe(
          (_: org.mongodb.scala.result.UpdateResult) => (),
          (exception: Throwable) => logger.debug(s"Rating-cadence write failed for $key: ${exception.getMessage}")
        )
      }.recover { case exception => logger.debug(s"Rating-cadence write failed for $key: ${exception.getMessage}") }
    }
  }

  private def hydrate(c: MongoCollection[Document]): Unit =
    Try {
      val documents = Await.result(c.find().toFuture(), 60.seconds)
      var count = 0
      documents.foreach { document =>
        for {
          key     <- Option(document.getString("_id"))
          started <- Option(document.getDate("windowStartedAt"))
          checked <- Option(document.getDate("lastCheckedAt"))
        } {
          mirror.put(key, RatingChangeStats(
            unchangedStreak = Option(document.getInteger("unchangedStreak")).map(_.intValue).getOrElse(0),
            windowChecks    = Option(document.getInteger("windowChecks")).map(_.intValue).getOrElse(0),
            windowChanges   = Option(document.getInteger("windowChanges")).map(_.intValue).getOrElse(0),
            windowStartedAt = Instant.ofEpochMilli(started.getTime),
            lastCheckedAt   = Instant.ofEpochMilli(checked.getTime)
          ))
          count += 1
        }
      }
      if (count > 0) logger.info(s"Hydrated $count rating-cadence record(s) from Mongo.")
    }.recover { case exception => logger.warn(s"Rating-cadence hydrate failed: ${exception.getMessage}") }
}
