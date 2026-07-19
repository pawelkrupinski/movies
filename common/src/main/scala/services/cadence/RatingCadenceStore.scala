package services.cadence

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.bson.{BsonDateTime, BsonDocument, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, Sorts, Updates}
import com.mongodb.client.model.UpdateOptions
import play.api.Logging
import services.movies.KeysetScan

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

  /** Fold one refresh outcome into `key`'s stats and store it. `reportedValue` is
   *  `Some(displayValue)` when this refresh (re)set the row's badge text, else `None`;
   *  it counts as a change only if it differs from the last recorded value (see
   *  [[RatingCadence.record]] — a re-keyed row re-reporting the same value is not a
   *  visible change). Single-writer (one worker, and the queue never runs two
   *  refreshes of the same key at once), so the read-then-write needs no lock. */
  final def record(key: String, reportedValue: Option[String], at: Instant = Instant.now()): RatingChangeStats = {
    val next = RatingCadence.record(statsFor(key), reportedValue, at)
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
 * `{ _id: <key>, backoffLevel, unchangedStreak, windowChecks, windowChanges,
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
            Updates.set("backoffLevel", s.backoffLevel),
            Updates.set("unchangedStreak", s.unchangedStreak),
            Updates.set("windowChecks", s.windowChecks),
            Updates.set("windowChanges", s.windowChanges),
            Updates.set("windowStartedAt", new java.util.Date(s.windowStartedAt.toEpochMilli)),
            Updates.set("lastCheckedAt", new java.util.Date(s.lastCheckedAt.toEpochMilli)),
            Updates.set("lastChange", MongoRatingCadenceStore.encodeChange(s.lastChange)),
            Updates.set("prevChange", MongoRatingCadenceStore.encodeChange(s.prevChange))
          ),
          new UpdateOptions().upsert(true)
        ).subscribe(
          (_: org.mongodb.scala.result.UpdateResult) => (),
          (exception: Throwable) => logger.debug(s"Rating-cadence write failed for $key: ${exception.getMessage}")
        )
      }.recover { case exception => logger.debug(s"Rating-cadence write failed for $key: ${exception.getMessage}") }
    }
  }

  private def hydrate(c: MongoCollection[Document]): Unit = {
    // Keyset-paged, NOT one unbounded `find().toFuture()`: rating_cadence is one row
    // per (source, film) — thousands and growing with the corpus — and a single
    // cursor over it brushes the 60s timeout / can StackOverflow the async driver as
    // it grows (Sentry KINOWO-19, the class the freshness + read-model hydrates
    // already migrated away from). Each page is a bounded
    // `find(_id > last).sort(_id).limit`.
    var count = 0
    KeysetScan.scan[Document](
      label          = "RatingCadenceStore hydrate",
      batchSize      = 2000,
      maxAttempts    = 3,
      initialBackoff = 500.millis,
      keyOf          = _.getString("_id"),
      fetchPage      = (afterId, limit) => {
        val find = afterId.fold(c.find())(a => c.find(Filters.gt("_id", a)))
        Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds)
      },
      onIncomplete   = exception => logger.warn(s"Rating-cadence hydrate keyset scan failed: ${exception.getMessage}")
    )(batch => batch.foreach { document =>
      MongoRatingCadenceStore.decodeRecord(document).foreach { case (key, stats) => mirror.put(key, stats); count += 1 }
    })
    if (count > 0) logger.info(s"Hydrated $count rating-cadence record(s) from Mongo.")
  }
}

object MongoRatingCadenceStore {
  /** Encode a change as a `{at, from, to}` sub-document, or BSON null when absent. */
  private[cadence] def encodeChange(change: Option[RatingChange]): BsonValue = change match {
    case Some(c) => BsonDocument(
      "at"   -> BsonDateTime(c.at.toEpochMilli),
      "from" -> BsonString(c.from),
      "to"   -> BsonString(c.to)
    )
    case None    => BsonNull()
  }

  /** Read a `{at, from, to}` change sub-document back, or None if absent/null/malformed.
   *  Legacy docs stored the new value under `value` with no `from`; read those too so
   *  history written before this field split still renders (with an empty `from`). */
  private[cadence] def decodeChange(document: Document, field: String): Option[RatingChange] =
    document.get(field).filter(_.isDocument).map(_.asDocument()).flatMap { d =>
      def str(k: String): Option[String] = Option(d.get(k)).filter(_.isString).map(_.asString().getValue)
      for {
        at <- Option(d.get("at")).filter(_.isDateTime).map(_.asDateTime().getValue)
        to <- str("to").orElse(str("value"))
      } yield RatingChange(Instant.ofEpochMilli(at), str("from").getOrElse(""), to)
    }

  /** Parse one `rating_cadence` Mongo document into `(dedupKey, stats)`, or None
   *  when the required fields are missing. Shared by the worker's boot hydrate and
   *  the web cadence reader so they decode the document identically. */
  def decodeRecord(document: Document): Option[(String, RatingChangeStats)] =
    for {
      key     <- Option(document.getString("_id"))
      started <- Option(document.getDate("windowStartedAt"))
      checked <- Option(document.getDate("lastCheckedAt"))
    } yield {
      val streak = Option(document.getInteger("unchangedStreak")).map(_.intValue).getOrElse(0)
      key -> RatingChangeStats(
        unchangedStreak = streak,
        windowChecks    = Option(document.getInteger("windowChecks")).map(_.intValue).getOrElse(0),
        windowChanges   = Option(document.getInteger("windowChanges")).map(_.intValue).getOrElse(0),
        windowStartedAt = Instant.ofEpochMilli(started.getTime),
        lastCheckedAt   = Instant.ofEpochMilli(checked.getTime),
        lastChange      = decodeChange(document, "lastChange"),
        prevChange      = decodeChange(document, "prevChange"),
        // Legacy rows (pre-backoffLevel) keyed the interval straight off the streak,
        // so fall back to it — the film keeps its current interval across the deploy
        // instead of snapping back to the base and re-climbing (a refresh storm).
        backoffLevel    = Option(document.getInteger("backoffLevel")).map(_.intValue).getOrElse(streak)
      )
    }
}

/** Read-only view of the `rating_cadence` collection for the dev cadence page
 *  (the web app). A one-shot full read — the page is dev-only and low-traffic, so
 *  no mirror. Returns `(dedupKey, stats)` pairs; an unreadable/absent Mongo yields
 *  an empty list rather than throwing. */
trait RatingCadenceReader {
  def all(): Seq[(String, RatingChangeStats)]

  /** Stats for a KNOWN set of keys — what one film's /debug expand needs (its four
   *  `<site>|tmdb:<id>` keys), as a bounded `_id in [...]` lookup instead of the
   *  full-collection read `all` does for the cadence overview page. */
  def forKeys(keys: Seq[String]): Map[String, RatingChangeStats]
}

object RatingCadenceReader {
  /** No data — the default where cadence isn't wired (tests, Mongo-less dev). */
  val empty: RatingCadenceReader = new RatingCadenceReader {
    override def all(): Seq[(String, RatingChangeStats)] = Seq.empty
    override def forKeys(keys: Seq[String]): Map[String, RatingChangeStats] = Map.empty
  }
}

class MongoRatingCadenceReader(db: Option[MongoDatabase]) extends RatingCadenceReader with Logging {
  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection("rating_cadence"))

  override def all(): Seq[(String, RatingChangeStats)] = coll match {
    case None => Seq.empty
    case Some(c) =>
      Try(Await.result(c.find().toFuture(), 30.seconds).flatMap(MongoRatingCadenceStore.decodeRecord))
        .recover { case e => logger.warn(s"Rating-cadence read failed: ${e.getMessage}"); Seq.empty }
        .getOrElse(Seq.empty)
  }

  override def forKeys(keys: Seq[String]): Map[String, RatingChangeStats] =
    if (keys.isEmpty) Map.empty
    else coll.fold(Map.empty[String, RatingChangeStats]) { c =>
      Try(Await.result(c.find(Filters.in("_id", keys*)).toFuture(), 10.seconds))
        .map(_.flatMap(MongoRatingCadenceStore.decodeRecord).toMap)
        .recover { case e => logger.warn(s"Rating-cadence lookup failed: ${e.getMessage}"); Map.empty }
        .getOrElse(Map.empty)
    }
}
