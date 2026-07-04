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

  /** The one error where KEEPING the token loops: a too-old / invalidated token (oplog
   *  window exceeded → `ChangeStreamHistoryLost`). Clearing it makes the next open start
   *  fresh + the backstop resyncs the gap. */
  def isInvalid(e: Throwable): Boolean = e match {
    case m: com.mongodb.MongoException =>
      m.getCode == 286 /* ChangeStreamHistoryLost */ ||
        Option(m.getMessage).exists(s => s.contains("ChangeStreamHistoryLost") ||
          s.toLowerCase.contains("resume of change stream was not possible"))
    case _ => false
  }
}
