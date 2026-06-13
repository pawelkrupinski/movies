package services.schedule

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.{MongoCollection, SingleObservableFuture}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonDateTime, BsonString}
import org.mongodb.scala.model.Indexes
import play.api.Logging
import services.MongoErrors

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Cluster-wide, one-shot claim on a single scheduled occurrence. Each worker
 * machine keeps its own recurring timer; on every fire it computes the
 * occurrence id (see [[OccurrenceKey]]) and calls `claim`. `claim` returns true
 * for exactly ONE caller per id across all machines — the winner runs the work,
 * everyone else gets false and skips. This is how the per-machine reaper ticks
 * become "run on one machine per occurrence, rotating" without a separate lock
 * or wall-clock-aligned timers: the bucketed id makes the claim idempotent.
 *
 * The claim is fire-and-forget — there is no release. A 48h TTL on the backing
 * store expires old occurrence records so the (high-churn) collection stays
 * bounded; occurrence ids never recur, so an expired claim is never re-contested.
 */
trait ScheduledRunStore {
  /** Try to claim `occurrenceId`. True for exactly one caller per id. */
  def claim(occurrenceId: String): Boolean
}

/** No-op store that lets every occurrence run — the single-node default (and the
 *  fallback when Mongo is opted out for local dev). With one machine there's no
 *  one to contend with, so claiming everything is correct. */
object AlwaysClaimScheduledRunStore extends ScheduledRunStore {
  def claim(occurrenceId: String): Boolean = true
}

/**
 * Mongo-backed [[ScheduledRunStore]]. Stores one document per claimed occurrence
 * in the collection passed in (typically `kinowo.scheduled_runs`):
 * {{{ { _id: "enrich-imdb@2026-06-13T09:00:00Z", claimedAt: ISODate(...) } }}}
 *
 * `claim` is an `insertOne` keyed by the occurrence id: the first machine's
 * insert wins; a second machine inserting the same `_id` gets a duplicate-key
 * (11000) error, caught and returned as `false` — the same dup-key-insert idiom
 * as `MongoLock` and `MongoTaskQueue` (see [[services.MongoErrors]]).
 *
 * A TTL index on `claimedAt` (`expireAfter` 48h) lets Mongo delete stale
 * occurrence docs, so the collection doesn't grow without bound (a doc per
 * occurrence per window — ~1/min for the scrape reaper). Created fire-and-forget
 * at construction, matching `MongoTaskQueue`'s index init.
 */
class MongoScheduledRunStore(coll: MongoCollection[Document]) extends ScheduledRunStore with Logging {

  locally {
    val t = new Thread(() => {
      Try {
        Await.result(coll.createIndex(
          Indexes.ascending("claimedAt"),
          new JIndexOptions().expireAfter(MongoScheduledRunStore.TtlHours, TimeUnit.HOURS)
        ).toFuture(), 10.seconds)
      }.recover { case ex => logger.warn(s"scheduled_runs TTL index init failed: ${ex.getMessage}") }
      ()
    }, "scheduled-runs-init")
    t.setDaemon(true)
    t.start()
  }

  override def claim(occurrenceId: String): Boolean =
    Try {
      Await.result(coll.insertOne(Document(
        "_id"       -> BsonString(occurrenceId),
        "claimedAt" -> BsonDateTime(Instant.now().toEpochMilli)
      )).toFuture(), 10.seconds)
      true
    }.recover {
      case ex: MongoWriteException if MongoErrors.isDuplicateKey(ex) =>
        // Another machine already claimed this occurrence — skip.
        false
      case ex: Throwable =>
        // On any other failure, skip rather than risk a double-run; the next
        // occurrence gets a fresh claim. (A missed occurrence is recovered by the
        // reaper's freshness gating, which re-queues the same stale work later.)
        logger.warn(s"ScheduledRunStore.claim($occurrenceId) failed: ${ex.getMessage}")
        false
    }.getOrElse(false)
}

object MongoScheduledRunStore {
  /** How long a claimed-occurrence doc lives before Mongo's TTL monitor deletes
   *  it. Comfortably longer than any job's period so a claim can't expire while
   *  its window is still live, short enough to keep the collection small. */
  val TtlHours: Long = 48L
}
