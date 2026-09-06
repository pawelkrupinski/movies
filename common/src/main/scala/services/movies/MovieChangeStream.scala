package services.movies

import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import org.bson.BsonDocument
import org.mongodb.scala.{Document, MongoCollection, Observer, Subscription}
import play.api.Logging

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.util.Try

/**
 * The `movies` change-stream SUBSCRIPTION: one shared cursor, decoded once and fanned
 * out to every listener, applied off the driver's I/O loop, resumed from a persisted
 * token and reopened after a terminal error. [[MongoMovieRepository]] composes one and
 * delegates `watchChanges` to it — the subscription changes for reasons (backpressure,
 * resume, reopen, coalescing) that persistence never does, which is why it lives here.
 *
 * `UPDATE_LOOKUP` makes insert/update/replace events carry the full post-image (not
 * just the delta), so we always hand a complete row to `onUpsert`. A DELETE has no
 * `fullDocument`, so we surface its `documentKey._id` to `onDelete` instead (what the
 * cache's periodic backstop used to be the only path for, and what the /debug live
 * view needs so a merged-away row disappears). The driver auto-resumes across
 * transient blips; a TERMINAL error is reopened on a backoff by [[ChangeStreamReopen]].
 * Requires a replica set (a single-node RS counts); on a standalone Mongo the stream
 * errors out and the caller falls back to its backstop.
 *
 * ONE shared change-stream cursor feeds every registered listener through a fan-out,
 * rather than a cursor per caller. The worker attaches two consumers (MovieCache +
 * ReadModelProjector); a cursor-per-caller decoded every write twice, and a profiler
 * showed that async change-stream I/O completion was the worker's dominant CPU cost.
 * Decode once here, dispatch to all. The cursor starts on the first listener and
 * stops when the last one detaches.
 *
 * `source` is the seam: production opens the collection's cursor
 * ([[MovieChangeStream.Source.ofCollection]]); a spec pushes events by hand.
 * `decode` turns a delivered post-image into the row consumers get (the repository's
 * stitched decode — showtimes and slots live in their own collections), and `reread`
 * re-reads a film by id after one of ITS `screenings` documents changed.
 */
final class MovieChangeStream(
  source:              MovieChangeStream.Source,
  screenings:          Option[ScreeningsRepository],
  decode:              StoredMovieDto => Option[StoredMovieRecord],
  reread:              String => Option[StoredMovieRecord],
  resumeToken:         ChangeStreamResumeToken,
  changeStreamMetrics: ChangeStreamMetrics,
  screeningsMetrics:   ScreeningsMetrics,
  changeDemandWindow:  Int
) extends Logging with AutoCloseable {

  private val movieChanges = new ChangeStreamFanout[StoredMovieRecord]("MovieRepository")
  private val changeSub    = new AtomicReference[Subscription]()
  private val changeLock   = new AnyRef
  // Applies change-stream events OFF the Mongo driver's Netty I/O event loops. The
  // apply does a blocking stitch read (`decode` / `reread`) plus the synchronized
  // read-model projection; running that on the I/O loops made the two loops contend
  // the projection monitor and busy-spin their wakeup eventfds (~24cc, ~0 voluntary
  // ctx-switches — proven on-box), flooring the shared-CPU credit. A SINGLE thread
  // keeps events applied strictly in order.
  //
  // Its queue is UNBOUNDED, so what keeps the backlog finite is the demand window each
  // cursor opens with — see [[ChangeStreamDemand]]. Every `changeApply.execute` here
  // must therefore be paired with an `applied()` in the task's `finally`, or that
  // cursor stalls; `applyBacklog` is the invariant made observable.
  private val changeApply  = tools.DaemonExecutors.singleThreadExecutor("movie-change-apply")
  private val backlog      = new AtomicInteger(0)
  // Demand windows: one per cursor, since each is a separate subscription. Both drain
  // into `changeApply`, so the queue is capped at the sum of the two windows.
  private val moviesDemand     = new ChangeStreamDemand(changeDemandWindow)
  private val screeningsDemand = new ChangeStreamDemand(changeDemandWindow)
  // Read-split only: a second cursor on `screenings`. A showtime change writes only
  // `screenings` (movies stays put), so without this the projector would never see it.
  private val screeningsWatch = new AtomicReference[Option[AutoCloseable]](None)

  /** Change events handed to the apply thread but not yet applied — the depth of the
   *  queue that used to be the leak. Bounded by the two demand windows; before
   *  backpressure it was bounded only by heap. Public because it is the observable form
   *  of that invariant: the integration spec asserts on it, and it is the number worth
   *  putting behind a gauge if this ever needs watching in prod. */
  def applyBacklog: Int = backlog.get()

  /** Enqueue one change-stream apply: count it into the backlog, and release a unit of
   *  the cursor's demand once it has actually run. Every hand-off to `changeApply` goes
   *  through here so neither half can be forgotten at a call site. */
  private def applyOffLoop(demand: ChangeStreamDemand)(work: => Unit): Unit = {
    backlog.incrementAndGet()
    changeApply.execute { () =>
      try work
      finally { backlog.decrementAndGet(); demand.applied() }
    }
  }

  // Film ids with a screenings apply already QUEUED AND NOT YET STARTED. The set is the
  // whole coalescing mechanism — see `applyScreeningsChange`.
  private val screeningsApplyPending = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

  /** One `screenings` change: re-read the film (stitched) and fan it out, COALESCING the
   *  events that name a film an apply is already queued for.
   *
   *  Why coalescing is the point rather than a nicety. The screenings cursor rings once per
   *  changed screenings DOCUMENT, i.e. once per (film, cinema slot) — so a film that really
   *  does change at every venue rings once per venue, and every ring costs a blocking stitch
   *  read plus a full re-projection OF THE SAME FILM. The widest US film carries 3,327 slots.
   *  Dropping the redundant WRITES (see `ScreeningsSplit.changedSlots`) removed the rows
   *  that never moved; it cannot remove these, because these rows genuinely did move. The
   *  answer is that the apply does not need to run per row: it re-reads the film's CURRENT
   *  state, so one read after the last of a burst sees everything the burst did.
   *
   *  Correctness rests on the ORDER of `remove` and the read: the id is removed BEFORE the
   *  re-read, so an event that lands while we are reading finds the set clear, enqueues its
   *  own apply, and gets a read that is guaranteed to be after its own write. Removing after
   *  the read would let exactly that event be swallowed by an apply that could not have seen
   *  it. The cost of the safe order is at most one extra apply per burst.
   *
   *  A COALESCED EVENT MUST STILL RELEASE ITS DEMAND. Every delivered event owes the cursor
   *  one `applied()` or the window closes and the stream stalls for good ([[ChangeStreamDemand]]),
   *  and an event that rides an already-queued apply never reaches that task's `finally`.
   *
   *  AFTER `close()` an enqueue is dropped on the floor (`dropRejectedAfterShutdown`), so the id
   *  stays in the set and its demand is never released. That is deliberate, not an oversight: the
   *  only reachable case is a repository being discarded, whose cursor nobody is waiting on any
   *  more. Anything that resurrects a closed repository would have to clear the set first. */
  private def applyScreeningsChange(filmId: String): Unit =
    if (screeningsApplyPending.add(filmId))
      applyOffLoop(screeningsDemand) {
        screeningsApplyPending.remove(filmId)
        reread(filmId).foreach(movieChanges.dispatchUpsert)
      }
    else {
      screeningsMetrics.recordCoalescedChange()
      screeningsDemand.applied()
    }

  // A change stream's onError is TERMINAL — nothing brings the cursor back on its own, and
  // `ensureWatching` only runs on REGISTRATION, which the worker does twice at boot and never
  // again. Without this driver one terminal error killed the worker's stream until the process
  // restarted (see [[ChangeStreamReopen]] for the outage that proved it). Skips the reopen once
  // the last listener has detached, so an idle repository stays idle.
  private val changeReopen = ChangeStreamReopen.onDaemonScheduler("MovieRepository",
    () => if (!movieChanges.isEmpty) ensureWatching())

  /** Attach a consumer, starting the shared cursor if it isn't running; the returned
   *  handle detaches just that consumer and stops the cursor once none remain. */
  def watch(onUpsert: StoredMovieRecord => Unit, onDelete: String => Unit): AutoCloseable = {
    val handle = movieChanges.register(onUpsert, onDelete)
    ensureWatching()
    new AutoCloseable { override def close(): Unit = { handle.close(); stopWatchingIfIdle() } }
  }

  /** Start the single shared cursor if it isn't already running. Each event is
   *  decoded once and fanned out to every listener; a delete (no post-image) is
   *  surfaced by `_id`. A terminal error clears the subscription and schedules a
   *  reopen on a backoff (a later registration re-opens too), and existing listeners
   *  fall back to their periodic backstop (cache rehydrate / projector reconcile)
   *  meanwhile. */
  private def ensureWatching(): Unit = changeLock.synchronized {
    if (changeSub.get() == null) {
      // Resume from the last persisted token if we have one (a restart / prior terminal
      // error) so events missed while down are replayed; else open at "now".
      val resumeFrom = resumeToken.load()
      source.open(resumeFrom, new Observer[ChangeStreamDocument[StoredMovieDto]] {
        override def onSubscribe(s: Subscription): Unit = { changeSub.set(s); moviesDemand.opened(s) }
        override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit = {
          changeReopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
          recordChangeMetrics(change)
          // Advance the resume position BEFORE fanning out, so a consumer signal (a
          // downstream latch / write) can never observe an event before the token moves.
          // This stays on the I/O thread (a cheap atomic set) to preserve that ordering.
          resumeToken.advance(change.getResumeToken)
          val fullDocument = Option(change.getFullDocument)
          val deletedId    = Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
            .map(v => if (v.isString) v.asString.getValue else v.toString)
          // Apply OFF the Netty I/O loop: the stitch read + projection must not run
          // there (they made the loops contend + spin — see `changeApply`).
          applyOffLoop(moviesDemand) {
            fullDocument match {
              // The movies doc has no showtimes — stitch them back from `screenings`
              // (via `decode`) before fanning out, so consumers get a full row.
              // A failed slot read yields None and we fan out NOTHING: an empty-cinema
              // record here is what the projector turns into a screenings wipe.
              case Some(dto) => decode(dto).foreach(movieChanges.dispatchUpsert)
              // No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't back-fill).
              // Surface its _id so consumers can drop the row.
              case None      => deletedId.foreach(movieChanges.dispatchDelete)
            }
            // Persist the advanced position (time-throttled, fire-and-forget).
            resumeToken.save(force = false)
          }
        }
        override def onError(e: Throwable): Unit = {
          if (ChangeStreamResumeToken.isInvalid(e)) {
            logger.warn(s"MovieRepository change stream: resume token invalid (${e.getMessage}) — clearing it; " +
              "the next open starts fresh and the backstop resyncs the gap.")
            resumeToken.clear()
          } else
            logger.warn(s"MovieRepository change stream ended (${e.getMessage}) — a reopen resumes from the " +
              "persisted token; the backstop covers the meantime.")
          changeSub.set(null)
          moviesDemand.closed()
          changeReopen.failed()
        }
        override def onComplete(): Unit = { changeSub.set(null); moviesDemand.closed(); changeReopen.failed() }
      })
      logger.info(s"MongoMovieRepository: watching change stream (shared by all listeners)" +
        s"${if (resumeFrom.isDefined) ", resumed from persisted token" else ""}.")
      // Also watch `screenings`: a showtime change fires there, not on `movies`.
      // Re-read + stitch (`reread` already stitches) + fan out so the projector
      // re-projects the film — the re-read is a BLOCKING read, so run it (and the
      // fanout) on `changeApply`, never on the screenings cursor's I/O event loop.
      if (screeningsWatch.get().isEmpty)
        screeningsWatch.set(screenings.flatMap(_.watch(applyScreeningsChange, screeningsDemand)))
    }
  }

  /** Stop the shared cursor once no listener remains, so an idle repository
   *  (e.g. web /debug after every viewer disconnects) doesn't keep decoding
   *  every write for nothing. */
  private def stopWatchingIfIdle(): Unit = changeLock.synchronized {
    if (movieChanges.isEmpty) {
      // Persist the final position synchronously so the next process resumes from here.
      resumeToken.save(force = true)
      Option(changeSub.getAndSet(null)).foreach(_.unsubscribe())
      moviesDemand.closed()
      screeningsWatch.getAndSet(None).foreach(h => Try(h.close()))
    }
  }

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatching: Boolean = changeSub.get() != null

  /** Count each change event by op, and each UPDATE by which field kind changed,
   *  onto the injected sink (noop unless the worker wired the Prometheus one). Best
   *  effort — instrumentation must never break the stream. */
  private def recordChangeMetrics(change: ChangeStreamDocument[StoredMovieDto]): Unit = Try {
    import scala.jdk.CollectionConverters._
    val op = ChangeStreamMetrics.normalizeOp(Option(change.getOperationType).map(_.getValue).getOrElse(""))
    changeStreamMetrics.recordEvent(op)
    if (op == ChangeStreamMetrics.Op.Update) {
      val desc    = Option(change.getUpdateDescription)
      val updated = desc.flatMap(d => Option(d.getUpdatedFields)).map(_.keySet.asScala.toSet).getOrElse(Set.empty[String])
      val removed = desc.flatMap(d => Option(d.getRemovedFields)).map(_.asScala.toSet).getOrElse(Set.empty[String])
      ChangeStreamMetrics.updateKinds(updated, removed).foreach(changeStreamMetrics.recordUpdateKind)
    }
  }.recover { case exception => logger.warn(s"change-stream metrics failed: ${exception.getMessage}") }.getOrElse(())

  /** Tear the subscription down for good: no more reopens, the final resume position
   *  persisted synchronously, both demand windows closed, the apply thread stopped. */
  override def close(): Unit = {
    changeReopen.close(); resumeToken.save(force = true)
    moviesDemand.closed(); screeningsDemand.closed()
    changeApply.shutdown()
  }
}

object MovieChangeStream {
  /** Where the `movies` events come from. `resumeAfter` is the persisted position to
   *  reopen from (None opens at "now"); the observer receives every delivered change. */
  trait Source {
    def open(resumeAfter: Option[BsonDocument], observer: Observer[ChangeStreamDocument[StoredMovieDto]]): Unit
  }

  object Source {
    /** Production: the collection's own change stream, with post-images looked up. */
    def ofCollection(c: MongoCollection[StoredMovieDto]): Source = new Source {
      override def open(resumeAfter: Option[BsonDocument], observer: Observer[ChangeStreamDocument[StoredMovieDto]]): Unit = {
        val base = c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
        resumeAfter.fold(base)(t => base.resumeAfter(Document(t))).subscribe(observer)
      }
    }
  }
}
