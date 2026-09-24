package services.movies

import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.bson.BsonDocument
import org.mongodb.scala.{Document, MongoCollection, Observer, Subscription}
import play.api.Logging
import services.readmodel.DecodeFailureMetrics

import java.util.concurrent.atomic.AtomicReference
import scala.reflect.ClassTag

/**
 * ONE side collection's change-stream subscription — `screenings` or `movie_slots` —
 * ringing `onChange(filmId)` for every row that changed, so the caller can re-read and
 * re-dispatch that film. The two collections share [[SlotKeyed]]'s addressing and this
 * shares their cursor: resumed from a persisted token, bounded by the caller's demand
 * window, metered, and reopened on a backoff after a terminal error.
 *
 * WHY ONE PIECE. `MongoScreeningsRepository.watch` carried all of this inline, and
 * `movie_slots` had no cursor at all — a slot row written after a film's last projection
 * (measured on prod 2026-09-07: 63 (film, venue) pairs in the UK, 33 in PL) was never
 * projected, because the projection needs the slot to emit that venue's row and a
 * repertory film's other rows never change again. A second inline copy would have been the
 * third change-stream observer in the codebase to get the resume-token / reopen / demand
 * dance subtly different from the others; this is the one place it is written.
 *
 * `onChange(filmId, applied)`: the caller calls `applied()` once it has APPLIED the event
 * (not merely queued it) — that, and only that, moves this cursor's resume position.
 *
 * What an event carries: insert/update/replace deliver the document, whose `filmIdOf`
 * names the film; a DELETE carries only the composite `_id`, from which
 * [[SlotKeyed.filmIdOf]] recovers the prefix. `onChange` is called ON THE DRIVER'S I/O
 * THREAD — the caller hands the blocking re-read off (see `MovieChangeStream`), which is
 * why the demand window is the caller's: only the caller knows when an event is APPLIED.
 *
 * A row the codec cannot decode is SKIPPED, not fatal — see [[ChangeEventDecoder]]: it is
 * counted, logged, releases its unit of demand and is never acknowledged, so the resume
 * position does not move for it.
 *
 * Requires a replica set (a single-node RS counts), like the `movies` stream.
 */
final class SideCollectionWatch[Dto: ClassTag](
  name:        String,
  collection:  MongoCollection[Dto],
  filmIdOf:    Dto => String,
  resumeToken: ChangeStreamResumeToken,
  metrics:     SideCollectionChangeMetrics,
  // Where a row the codec refuses is counted, under this collection's `name`.
  decodeFailures: DecodeFailureMetrics,
  // The reopen driver, keyed by `name`. Production schedules on a daemon thread; a spec
  // can hand over one that fires when it says so.
  reopenDriver: (String, () => Unit) => ChangeStreamReopen = ChangeStreamReopen.onDaemonScheduler
) extends Logging {

  /** Open the cursor; the handle stops reopening, persists the final position and
   *  unsubscribes. */
  def watch(onChange: (String, () => Unit) => Unit, demand: ChangeStreamDemand): AutoCloseable = {
    val decoder = ChangeEventDecoder.of[Dto](name, collection.codecRegistry, decodeFailures)
    val subRef = new AtomicReference[Subscription]()
    // A terminal error is the END of a cursor — the driver never brings it back, and unlike
    // the movies stream there is not even a later registration to re-open this one. Without
    // this driver a single blip left every change on the collection unseen until the process
    // restarted.
    def open(): Unit = {
      // Resume from the last persisted token (a restart / prior terminal error) so changes
      // that landed while down are replayed; else open at "now".
      val resumeFrom = resumeToken.load()
      // Post-images arrive UNDECODED and are decoded below: decoded by the driver, one row the
      // codec refuses ended the cursor — see [[ChangeEventDecoder]].
      val base       = collection.watch[BsonDocument]()
      resumeFrom.fold(base)(t => base.resumeAfter(Document(t)))
        .subscribe(new Observer[ChangeStreamDocument[BsonDocument]] {
          override def onSubscribe(s: Subscription): Unit = { subRef.set(s); demand.opened(s) }
          override def onNext(change: ChangeStreamDocument[BsonDocument]): Unit = {
            reopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
            // Count the event BEFORE anything can drop it: an event the caller's `onChange`
            // throws on, or one the resume-token save fails behind, still cost a projection.
            metrics.recordChangeEvent(
              ChangeStreamMetrics.normalizeOp(Option(change.getOperationType).map(_.getValue).getOrElse("")))
            // The resume position moves only when the CALLER says this event is applied —
            // the `applied` it is handed with the film id. `onChange` only queues the re-read;
            // advancing here, at delivery, persisted a position past every queued event, so a
            // restart resumed after changes that were never applied. The generation stops a
            // late acknowledgement re-arming a token `clear()` has since thrown away.
            val token      = change.getResumeToken
            val generation = resumeToken.generation
            val applied    = () => { resumeToken.advance(token, generation); resumeToken.save(force = false) }
            def deletedFilm = Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
              .map(v => if (v.isString) v.asString.getValue else v.toString)
              .map(SlotKeyed.filmIdOf) // a delete carries no post-image — split the _id
            val filmId = decoder.postImage(change) match {
              case ChangeEventDecoder.PostImage.Present(row) => Some(filmIdOf(row))
              case ChangeEventDecoder.PostImage.Absent       => deletedFilm
              case ChangeEventDecoder.PostImage.Undecodable  => None // counted and logged by the decoder
            }
            filmId match {
              case Some(fid) => try onChange(fid, applied)
                catch { case e: Throwable => logger.warn(s"$name watch onChange($fid) failed: ${e.getMessage}") }
              // Nothing to hand the caller, so nothing will release this event's demand but us —
              // left unreleased, every skipped event narrowed the window until the cursor stalled.
              // Not acknowledged: the event was not applied, so the resume position stays put.
              case None => demand.applied()
            }
          }
          override def onError(e: Throwable): Unit = {
            if (ChangeStreamResumeToken.isInvalid(e)) {
              logger.warn(s"$name change stream: resume token invalid (${e.getMessage}) — clearing it; " +
                "the next open starts fresh and the backstop resyncs the gap.")
              resumeToken.clear()
            } else
              logger.warn(s"$name change stream ended (${e.getMessage}) — a reopen resumes from the " +
                "persisted token; the backstop covers the meantime.")
            subRef.set(null)
            demand.closed()
            reopen.failed()
          }
          override def onComplete(): Unit = { subRef.set(null); demand.closed(); reopen.failed() }
        })
      logger.info(s"$name change stream: watching" +
        s"${if (resumeFrom.isDefined) ", resumed from persisted token" else ""}.")
    }
    lazy val reopen: ChangeStreamReopen = reopenDriver(name, () => open())
    open()
    new AutoCloseable { override def close(): Unit = {
      reopen.close()
      resumeToken.save(force = true) // final position synchronously so the next process resumes here
      demand.closed()
      Option(subRef.get()).foreach(_.unsubscribe())
    } }
  }
}
