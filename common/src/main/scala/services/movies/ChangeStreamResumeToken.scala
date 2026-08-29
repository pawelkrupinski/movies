package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import org.bson.BsonDocument
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Persists ONE change-stream's resume token to `change_stream_tokens` (`_id =
 * streamId`) so the cursor reopens — after a terminal error, and the big win, after
 * a WORKER RESTART — from where it left off, REPLAYING events that landed while the
 * process was down. That closes the downtime gap the consumers' periodic backstops
 * (cache rehydrate / projector reconcile) exist for.
 *
 * One instance per watched collection: `MongoMovieRepository` owns the `"movies"`
 * token, `MongoScreeningsRepository` the `"screenings"` one — a showtime change
 * writes only `screenings`, so without its own resumable stream a restart drops the
 * showtime edits made while down and only the full reproject catches them (the
 * asymmetry that kept the reproject non-redundant).
 *
 * `enabled` is ON only in the WORKER (the durable mirror); web /debug + scripts pass
 * it OFF so an ephemeral viewer's cursor position can't clobber the worker's shared
 * token. Best-effort throughout — a failed load/save only logs; the backstop covers
 * a miss. Persist is time-throttled + fire-and-forget so the driver thread never
 * blocks on a Mongo write per event; a clean shutdown forces one synchronous save so
 * a restart's resume is deterministic.
 */
class ChangeStreamResumeToken(streamId: String, database: Option[MongoDatabase], enabled: Boolean) extends Logging {
  import ChangeStreamResumeToken.TokenSaveThrottleMs

  private val lastToken       = new AtomicReference[BsonDocument](null)
  private val lastTokenSaveMs = new AtomicLong(0L)
  private lazy val coll: Option[MongoCollection[Document]] =
    if (!enabled) None
    else database.map(_.getCollection[Document]("change_stream_tokens").withWriteConcern(WriteConcern.W1.withJournal(false)))

  /** The persisted position to reopen from, if any (a restart / prior terminal error). */
  def load(): Option[BsonDocument] =
    coll.flatMap { c =>
      Try(Option(Await.result(c.find(Filters.eq("_id", streamId)).first().toFuture(), 5.seconds)))
        .toOption.flatten.flatMap(_.get("token")).map(_.asDocument())
    }

  /** Record the latest seen token — call BEFORE fanning an event out, so a consumer
   *  signal can never observe an event before the position moves. */
  def advance(token: BsonDocument): Unit = lastToken.set(token)

  /** Persist the advanced position. `force` (clean shutdown) writes SYNCHRONOUSLY so a
   *  restart resumes deterministically; otherwise fire-and-forget + time-throttled. */
  def save(force: Boolean): Unit = {
    val token = lastToken.get()
    if (token != null) coll.foreach { c =>
      val nowMs = System.currentTimeMillis()
      if (force || nowMs - lastTokenSaveMs.get() >= TokenSaveThrottleMs) {
        lastTokenSaveMs.set(nowMs)
        val write = c.replaceOne(Filters.eq("_id", streamId),
          Document("_id" -> streamId, "token" -> token), new ReplaceOptions().upsert(true)).toFuture()
        if (force) Try(Await.result(write, 5.seconds))
      }
    }
  }

  /** Drop the token so the next open starts fresh at "now" — for a too-old / invalidated
   *  token (oplog window exceeded), where resuming would loop on the same error. */
  def clear(): Unit = {
    lastToken.set(null)
    coll.foreach(c => Try(Await.result(c.deleteOne(Filters.eq("_id", streamId)).toFuture(), 5.seconds)))
  }
}

object ChangeStreamResumeToken {
  private val TokenSaveThrottleMs = 5000L

  /** The errors where KEEPING the token loops for ever — resuming from it can only fail
   *  again, so the next open must start fresh and let the backstop resync the gap.
   *
   *  Two shapes, and the second one cost a full outage:
   *
   *   - `ChangeStreamHistoryLost` (286) — the token fell out of the oplog window.
   *   - `InvalidResumeToken` (260) *"Attempting to resume a change stream using 'resumeAfter'
   *     is not allowed from an invalidate notification"* — a collection drop INVALIDATES the
   *     cursor, and the invalidate event's own token is the last thing `advance` saw, so the
   *     saved position is one the server will never accept. This one carries NO error label,
   *     which is why the integration test that drops the collection is the thing that found
   *     it: with only the label + 280 handled, the reopen loop below just retried it for ever.
   *   - Anything the driver labels `NonResumableChangeStreamError`, in practice
   *     `ChangeStreamFatalError` (280) *"cannot resume stream; the resume token was not
   *     found"*. That is what a token from BEFORE a collection drop/restore becomes: the
   *     2026-08-29 Mongo migration dump-and-restored every collection, so all three
   *     workers booted holding a token that pointed into the pre-restore oplog. This
   *     predicate matched neither the code nor the message, so the token was KEPT, every
   *     open failed the same way, and the movies + screenings change streams were dead
   *     from boot on every country — the read model took zero projections and only
   *     prune deletes, and the site quietly served a shrinking, frozen corpus.
   *
   *  Matched on the driver's error LABEL first because that is the canonical signal (it
   *  covers codes we have not seen yet); the codes and message text are belt-and-braces
   *  for drivers/servers that report one but not the other. */
  def isInvalid(e: Throwable): Boolean = e match {
    case m: com.mongodb.MongoException =>
      m.hasErrorLabel("NonResumableChangeStreamError") ||
        m.getCode == 286 /* ChangeStreamHistoryLost */ ||
        m.getCode == 280 /* ChangeStreamFatalError */ ||
        m.getCode == 260 /* InvalidResumeToken */ ||
        Option(m.getMessage).exists { s =>
          val lower = s.toLowerCase
          s.contains("ChangeStreamHistoryLost") || s.contains("NonResumableChangeStreamError") ||
            lower.contains("resume of change stream was not possible") ||
            lower.contains("resume token was not found") ||
            lower.contains("not allowed from an invalidate notification")
        }
    case _ => false
  }
}
