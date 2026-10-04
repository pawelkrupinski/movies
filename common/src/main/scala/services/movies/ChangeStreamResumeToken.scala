package services.movies

import java.util.Locale

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

  /** The persisted position to reopen from (a restart / prior terminal error): absent when none
   *  was saved, FAILED when it could not be read — which is not "none saved". */
  def load(): tools.ReadOutcome[BsonDocument] =
    coll.fold[tools.ReadOutcome[BsonDocument]](tools.ReadOutcome.none(s"$streamId resume token (not persisted)")) { c =>
      tools.MongoRead.one(s"$streamId resume token", 5.seconds)(c.find(Filters.eq("_id", streamId)).headOption())
        .flatMap(_.get("token").fold[tools.ReadOutcome[BsonDocument]](tools.ReadOutcome.none(s"$streamId resume token"))(
          token => tools.ReadOutcome.Answered(token.asDocument())))
    }

  // How many opens in a row found the saved position unreadable. Only the opening thread
  // (the reopen scheduler, or a registration under the stream's lock) touches it.
  private val unreadableOpens = new java.util.concurrent.atomic.AtomicInteger(0)

  /** Where to open the cursor: the saved position, or "now" when none was saved.
   *
   *  A position that could not be READ is not "none saved": opened at now, the cursor skips every
   *  change since the last save, which then reaches the consumers only through their backstops
   *  (cache rehydrate, projector reconcile — hours). So the open is [[ChangeStreamResumeToken.Position.Deferred]]
   *  and the caller retries it on its reopen backoff; only after
   *  [[ChangeStreamResumeToken.MaxDeferredOpens]] deferrals in a row does it open at now — a cursor
   *  that never opens is worse than one that skips — said at WARN, unlike a first-ever open. */
  def openFrom(): ChangeStreamResumeToken.Position = {
    import ChangeStreamResumeToken.Position
    load() match {
      case tools.ReadOutcome.Answered(token) => unreadableOpens.set(0); Position.At(Some(token))
      case tools.ReadOutcome.Absent(_)       => unreadableOpens.set(0); Position.At(None)
      case tools.ReadOutcome.Failed(cause)   =>
        if (unreadableOpens.incrementAndGet() <= ChangeStreamResumeToken.MaxDeferredOpens) {
          logger.warn(s"Change stream '$streamId': the saved resume position could not be read (${cause.explain}) — " +
            "deferring the open to the reopen backoff")
          Position.Deferred
        } else {
          unreadableOpens.set(0)
          logger.warn(s"Change stream '$streamId': the saved resume position stayed unreadable (${cause.explain}) — " +
            "opening at now; changes made since the last save are recovered only by the backstop")
          Position.At(None)
        }
    }
  }

  // Bumped by every `clear()`. A position is advanced only once its event is APPLIED —
  // on the apply thread, possibly well after delivery — so an event delivered before a
  // clear can finish after it; its stale `generation` is what stops it re-arming the token
  // the clear threw away.
  private val generations = new AtomicLong(0L)

  /** The generation to capture when an event is DELIVERED and hand back to [[advance]]
   *  once it has been applied. */
  def generation: Long = generations.get()

  /** Record `token` as the position to resume AFTER — call once its event has been
   *  APPLIED, fan-out included. A token delivered before the last [[clear]] is ignored. */
  def advance(token: BsonDocument, deliveredAt: Long): Unit = synchronized {
    if (deliveredAt == generations.get()) lastToken.set(token)
  }

  /** The position a save would persist now — for specs. */
  private[movies] def current: Option[BsonDocument] = Option(lastToken.get())

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
    synchronized { generations.incrementAndGet(); lastToken.set(null) }
    coll.foreach(c => Try(Await.result(c.deleteOne(Filters.eq("_id", streamId)).toFuture(), 5.seconds)))
  }
}

object ChangeStreamResumeToken {
  private val TokenSaveThrottleMs = 5000L

  /** Where a cursor opens. */
  sealed trait Position
  object Position {
    /** Open now: after `token`, or at "now" when it is `None`. */
    final case class At(token: Option[BsonDocument]) extends Position
    /** Do not open yet — the saved position could not be read; retry on the reopen backoff. */
    case object Deferred extends Position
  }

  /** Opens deferred in a row on an unreadable position before one opens at now anyway: with the
   *  reopen backoff (1 s, 5 s, 15 s, …) that rides out a blip of about twenty seconds. */
  val MaxDeferredOpens = 3

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
          val lower = s.toLowerCase(Locale.ROOT)
          s.contains("ChangeStreamHistoryLost") || s.contains("NonResumableChangeStreamError") ||
            lower.contains("resume of change stream was not possible") ||
            lower.contains("resume token was not found") ||
            lower.contains("not allowed from an invalidate notification")
        }
    case _ => false
  }
}
