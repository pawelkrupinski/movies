package services.movies

import tools.ChangeStreamLiveness

import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.bson.BsonDocument
import org.mongodb.scala.{Observer, Subscription}
import play.api.Logging

import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

/** [[MovieChangeStream]]'s `movies` cursor: opened after a resume position, each event's post-image
 *  decoded and handed to the apply side ([[MovieChangeApply]]) as a re-read of its film — or, with no
 *  post-image, a delete — and its resume position moved past each contiguous run of applied events.
 *  An error or its end clears it and asks `reopen` for another on its backoff. `subscribe` opens
 *  the collection's change stream ([[MovieChangeStream.Source]]). */
final class MoviesChangeCursor(
  subscribe:           (Option[BsonDocument], Observer[ChangeStreamDocument[BsonDocument]]) => Unit,
  resumeToken:         ChangeStreamResumeToken,
  reopen:              ChangeStreamReopen,
  changeApply:         MovieChangeApply,
  fanout:              ChangeStreamFanout[MovieChangeApply.Delivery, MovieChangeApply.VenueDelivery],
  liveness:            ChangeStreamLiveness,
  changeStreamMetrics: ChangeStreamMetrics,
  changeDemandWindow:  Int,
  // Post-images arrive undecoded (see `Source`) and are decoded here, so a document the codec
  // refuses is one skipped event rather than the end of the cursor — see [[ChangeEventDecoder]].
  postImages:          ChangeEventDecoder[StoredMovieDto]
) extends Logging {

  private val changeSub    = new AtomicReference[Subscription]()
  // The demand window of this cursor (each side cursor has its own, since each is a separate
  // subscription). All drain into the apply thread, so its queue is capped at the sum of the windows.
  private val moviesDemand = new ChangeStreamDemand(changeDemandWindow)

  // The movies cursor's delivered events: its position moves only past a contiguous run of applied ones,
  // saved time-throttled and fire-and-forget ([[ChangeStreamResumeToken.acknowledgements]]).
  private val acknowledge = resumeToken.acknowledgements()

  /** Open the cursor after `resumeFrom` (at "now" when `None`). Called under the stream's `changeLock`. */
  def open(resumeFrom: Option[BsonDocument]): Unit = {
    subscribe(resumeFrom, new Observer[ChangeStreamDocument[BsonDocument]] {
      override def onSubscribe(s: Subscription): Unit = {
        changeSub.set(s); moviesDemand.opened(s); liveness.watching(ChangeStreamLiveness.Movies)
      }
      override def onNext(change: ChangeStreamDocument[BsonDocument]): Unit = onMoviesChange(change)
      override def onError(e: Throwable): Unit = { resumeToken.onCursorError("MovieRepository", e, logger); moviesCursorEnded() }
      override def onComplete(): Unit = moviesCursorEnded()
    })
    logger.info(s"MongoMovieRepository: watching change stream (shared by all listeners)" +
      s"${if (resumeFrom.isDefined) ", resumed from persisted token" else ""}.")
  }

  /** The `movies` cursor is gone — an error or its end: reopened on the backoff. */
  private def moviesCursorEnded(): Unit = {
    changeSub.set(null)
    moviesDemand.closed()
    reopen.failed()
  }

  /** One event the `movies` cursor delivered, applied OFF the Netty I/O loop: the stitch read +
   *  projection must not run there (they made the loops contend + spin — see `MovieChangeApply.changeApply`). */
  private def onMoviesChange(change: ChangeStreamDocument[BsonDocument]): Unit = {
    reopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
    liveness.delivered(ChangeStreamLiveness.Movies)
    recordChangeMetrics(change)
    // The resume position moves only once this event is APPLIED — in its apply task,
    // AFTER the fan-out (see `MovieChangeApply.applyReread`). Advancing HERE, at delivery, persisted a
    // position past every delivered-but-queued event (up to a demand window of them), so a
    // restart resumed after events that were never applied. The generation guards against
    // a `clear()` (invalid token) landing while this event is still queued.
    val ack = acknowledge(change.getResumeToken)
    postImages.postImage(change) match {
      case ChangeEventDecoder.PostImage.Present(dto) => applyMoviesUpsert(dto._id, ack)
      case ChangeEventDecoder.PostImage.Absent       => applyMoviesDelete(ChangeEventDecoder.documentIdOf(change), ack)
      // A document the codec refuses (counted and logged by the decoder). Nothing to apply,
      // and it must not END the cursor: decoded inside the driver, it did — and a cursor
      // resuming from a persisted token met it again on every reopen, dead for good. Its
      // demand is released, and it is acknowledged as applied — there is nothing to apply,
      // and left unacknowledged it would hold the cursor's position before it for good
      // ([[tools.AppliedPrefix]] moves only past a contiguous run of acknowledged events).
      case ChangeEventDecoder.PostImage.Undecodable  => ack(); moviesDemand.applied()
    }
  }

  /** The movies doc has no showtimes — reread the film STITCHED (via `reread`, the
   *  same by-id read the side cursors use) before fanning out, so consumers get a
   *  full row. A failed read fans out NOTHING (an empty-cinema record here is what the
   *  projector turns into a screenings wipe) and holds the position — see `MovieChangeApply.applyReread`.
   *
   *  COALESCE with a same-film apply already queued — by an earlier movies event,
   *  or by the screenings/movie_slots cursors sharing [[MovieChangeApply]]' `pending`. This is the
   *  common case, not an edge case: `dropCinemaSlots` writes `retainedSynopses` to
   *  `movies` in the SAME tick it deletes the dropped venue's screenings/movie_slots
   *  rows, so a slot drop used to buy the film TWO re-projections (one bought here,
   *  one by the coalesced side burst) instead of one. `reread(dto._id)` re-reads the
   *  WHOLE film — movies row included — FRESH at call time regardless of which event's
   *  apply actually runs, so whichever one fires sees every write of the burst,
   *  including a SECOND, independent `movies`-doc write racing the first one's still-
   *  queued apply. (An earlier version of this branch called `decode(dto)`, which
   *  re-stitched the side collections fresh but reused the TRIGGERING event's own
   *  captured document for the movies-level fields — so a second real movies-doc write
   *  landing before the first apply's `remove` ran could be coalesced away and its
   *  content silently lost, since nothing about that path re-read `movies` itself.
   *  `reread` closes that window the same way the side cursors' own re-read already
   *  does, at the cost of one extra point read per movies apply.) */
  private def applyMoviesUpsert(filmId: String, ack: () => Unit): Unit =
    if (!changeApply.request(filmId, ChangeStreamLiveness.Movies, moviesDemand, ack, debounced = false)) {
      changeStreamMetrics.recordCoalescedChange(); moviesDemand.applied()
    }

  /** No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't back-fill). Surface
   *  its _id so consumers can drop the row. Never coalesced: once a row is gone
   *  there is nothing to re-read, and every delete must still reach the fan-out.
   *
   *  And a delete is a coalescing BARRIER: it takes the id's queued entry out of `pending`, so a
   *  later event for the same id queues its own apply AFTER the delete instead of riding
   *  one queued before it. Ids are derived from the creation key (`FilmId.fresh`), so a
   *  film deleted and re-created under the same key comes back with the same id — riding
   *  the earlier apply would dispatch the re-created film first and the delete last,
   *  leaving every listener on "deleted" for a film that exists. */
  private def applyMoviesDelete(deletedId: Option[String], ack: () => Unit): Unit = {
    // Only a re-read already QUEUED is behind the barrier: one the debounce still holds is
    // queued after this delete, so it runs after it and reads whatever the key holds then.
    deletedId.foreach(changeApply.barrier)
    changeApply.applyOffLoop(ChangeStreamLiveness.Movies, moviesDemand) {
      deletedId.foreach(fanout.dispatchDelete)
      ack()
    }
  }

  /** Stop the cursor: unsubscribed, its demand window closed. */
  def unsubscribe(): Unit = {
    Option(changeSub.getAndSet(null)).foreach(_.unsubscribe())
    moviesDemand.closed()
  }

  /** Whether the cursor is currently running. */
  def isWatching: Boolean = changeSub.get() != null

  /** Count each change event by op, and each UPDATE by which field kind changed,
   *  onto the injected sink (noop unless the worker wired the Prometheus one). Best
   *  effort — instrumentation must never break the stream. */
  private def recordChangeMetrics(change: ChangeStreamDocument[BsonDocument]): Unit = Try {
    import scala.jdk.CollectionConverters._
    val op = ChangeStreamMetrics.opOf(change)
    changeStreamMetrics.recordEvent(op)
    if (op == ChangeStreamMetrics.Op.Update) {
      val desc    = Option(change.getUpdateDescription)
      val updated = desc.flatMap(d => Option(d.getUpdatedFields)).map(_.keySet.asScala.toSet).getOrElse(Set.empty[String])
      val removed = desc.flatMap(d => Option(d.getRemovedFields)).map(_.asScala.toSet).getOrElse(Set.empty[String])
      ChangeStreamMetrics.updateKinds(updated, removed).foreach(changeStreamMetrics.recordUpdateKind)
    }
  }.recover { case exception => logger.warn(s"change-stream metrics failed: ${exception.getMessage}") }.getOrElse(())
}
