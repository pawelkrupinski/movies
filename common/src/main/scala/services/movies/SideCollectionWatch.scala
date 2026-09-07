package services.movies

import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.mongodb.scala.{Document, MongoCollection, Observer, Subscription}
import play.api.Logging

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
 * What an event carries: insert/update/replace deliver the document, whose `filmIdOf`
 * names the film; a DELETE carries only the composite `_id`, from which
 * [[SlotKeyed.filmIdOf]] recovers the prefix. `onChange` is called ON THE DRIVER'S I/O
 * THREAD — the caller hands the blocking re-read off (see `MovieChangeStream`), which is
 * why the demand window is the caller's: only the caller knows when an event is APPLIED.
 *
 * Requires a replica set (a single-node RS counts), like the `movies` stream.
 */
final class SideCollectionWatch[Dto: ClassTag](
  name:        String,
  collection:  MongoCollection[Dto],
  filmIdOf:    Dto => String,
  resumeToken: ChangeStreamResumeToken,
  metrics:     SideCollectionChangeMetrics,
  // The reopen driver, keyed by `name`. Production schedules on a daemon thread; a spec
  // can hand over one that fires when it says so.
  reopenDriver: (String, () => Unit) => ChangeStreamReopen = ChangeStreamReopen.onDaemonScheduler
) extends Logging {

  /** Open the cursor; the handle stops reopening, persists the final position and
   *  unsubscribes. */
  def watch(onChange: String => Unit, demand: ChangeStreamDemand): AutoCloseable = {
    val subRef = new AtomicReference[Subscription]()
    // A terminal error is the END of a cursor — the driver never brings it back, and unlike
    // the movies stream there is not even a later registration to re-open this one. Without
    // this driver a single blip left every change on the collection unseen until the process
    // restarted.
    def open(): Unit = {
      // Resume from the last persisted token (a restart / prior terminal error) so changes
      // that landed while down are replayed; else open at "now".
      val resumeFrom = resumeToken.load()
      val base       = collection.watch()
      resumeFrom.fold(base)(t => base.resumeAfter(Document(t)))
        .subscribe(new Observer[ChangeStreamDocument[Dto]] {
          override def onSubscribe(s: Subscription): Unit = { subRef.set(s); demand.opened(s) }
          override def onNext(change: ChangeStreamDocument[Dto]): Unit = {
            reopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
            // Count the event BEFORE anything can drop it: an event the caller's `onChange`
            // throws on, or one the resume-token save fails behind, still cost a projection.
            metrics.recordChangeEvent(
              ChangeStreamMetrics.normalizeOp(Option(change.getOperationType).map(_.getValue).getOrElse("")))
            // Advance the resume position BEFORE ringing onChange, so a re-read can never
            // observe the change before the token moves past it.
            resumeToken.advance(change.getResumeToken)
            val filmId = Option(change.getFullDocument).map(filmIdOf).orElse(
              Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
                .map(v => if (v.isString) v.asString.getValue else v.toString)
                .map(SlotKeyed.filmIdOf)) // a delete carries no post-image — split the _id
            filmId.foreach(fid => try onChange(fid)
              catch { case e: Throwable => logger.warn(s"$name watch onChange($fid) failed: ${e.getMessage}") })
            resumeToken.save(force = false) // time-throttled, fire-and-forget
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
