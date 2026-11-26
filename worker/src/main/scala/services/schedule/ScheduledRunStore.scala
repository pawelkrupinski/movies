package services.schedule

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Try

import org.mongodb.scala.MongoCollection
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.collection.immutable.Document
import play.api.Logging

/**
 * Cluster-wide, one-shot claim on a single scheduled occurrence. Each worker
 * machine keeps its own recurring timer; on every fire it computes the
 * occurrence id (see [[OccurrenceKey]]) and calls `claim`. `claim` returns true
 * for exactly ONE caller per id across all machines — the winner runs the work,
 * everyone else gets false and skips. This is how the per-machine reaper ticks
 * become "run on one machine per occurrence, rotating" without a separate lock
 * or wall-clock-aligned timers: the bucketed id makes the claim idempotent.
 *
 * The claim is fire-and-forget — there is no release. A 48h expiry on the backing
 * store deletes old occurrence records so the (high-churn) collection stays
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
 * [[ScheduledRunStore]] over a keyed store of claims ([[tools.KeyedStore]]), one per claimed occurrence:
 * {{{ { _id: "enrich-imdb@2026-06-13T09:00:00Z", claimedAt: ISODate(...) } }}}
 *
 * `claim` is an insert keyed by the occurrence id ([[tools.StoreOnce.insert]]): the first machine's insert wins, a second
 * inserting the same `_id` is refused -- the same dup-key-insert idiom as `MongoTaskQueue`. Any other failure skips
 * rather than risk a double run: the next occurrence gets a fresh claim, and a missed one is recovered by the reaper's
 * freshness gating, which re-queues the same stale work later.
 *
 * `claimedAt` is the wiring's clock's instant, and a claim lives [[KeyedScheduledRunStore.Expiry]] past it
 * ([[tools.DocumentExpiry]]: Mongo's TTL index, the in-memory store's clock), so the collection doesn't grow without
 * bound (a document per occurrence per window -- ~1/min for the scrape reaper).
 */
abstract class KeyedScheduledRunStore(claims: tools.StoreOnce[String, Instant], clock: java.time.Clock) extends ScheduledRunStore with Logging {
  override def claim(occurrenceId: String): Boolean =
    Try(claims.insert(occurrenceId, clock.instant())).recover { case exception =>
      logger.warn(s"ScheduledRunStore.claim($occurrenceId) failed: ${exception.getMessage}")
      false
    }.get
}

object KeyedScheduledRunStore {
  /** How long a claimed-occurrence document lives before it is deleted. Comfortably longer than any job's period so a
   *  claim can't expire while its window is still live, short enough to keep the collection small. */
  val Expiry: tools.DocumentExpiry = tools.DocumentExpiry("claimedAt", 48.hours)

  /** A claim's document: when it was claimed. */
  val Codec: tools.DocCodec[Instant] = new tools.DocCodec[Instant] {
    def encode(claimedAt: Instant): Document = Document("claimedAt" -> BsonDateTime(claimedAt.toEpochMilli))
    def decode(document: Document): Option[Instant] =
      document.get("claimedAt").filter(_.isDateTime).map(d => Instant.ofEpochMilli(d.asDateTime.getValue))
  }

  /** The claims in `documents`. */
  def claims(documents: tools.StoreDocuments): tools.KeyedStore[String, Instant] = new tools.KeyedStore(documents, tools.KeyCodec.Text, Codec)
}

/** The claims in the collection passed in (typically `kinowo.scheduled_runs`), its TTL index made at construction. */
class MongoScheduledRunStore(coll: MongoCollection[Document], clock: java.time.Clock) extends KeyedScheduledRunStore(
  KeyedScheduledRunStore.claims(tools.MongoStoreDocuments.of(coll, Some(KeyedScheduledRunStore.Expiry))), clock)
