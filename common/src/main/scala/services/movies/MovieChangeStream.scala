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
 * THREE CURSORS, ONE FAN-OUT. Under the read-split a film is three collections, and a
 * write to any of them must reach the same listeners: `movies` (the film itself),
 * `screenings` (a showtime change writes only there) and `movie_slots` (a venue's slot —
 * which lands WITHOUT a `movies` write whenever the film document is unchanged, the
 * usual case, and which the projection needs before it can emit that venue's row at
 * all). The two side cursors ring with a film id; the apply re-reads the film (`reread`
 * already stitches) and dispatches it as an upsert. Measured before the third cursor
 * (prod, 2026-09-07): 63 UK and 33 PL (film, venue) pairs whose slot row landed after
 * the film's last projection and were never projected again.
 *
 * `source` is the seam: production opens the collection's cursor
 * ([[MovieChangeStream.Source.ofCollection]]); a spec pushes events by hand.
 * `reread` re-reads a film by id, stitched (showtimes and slots live in their own
 * collections) — after one of ITS side-collection rows changed, or after its own
 * `movies`-doc apply is the one chosen to run, so a coalesced burst is always
 * resolved by a read that is fresh at the moment it actually executes, never a
 * captured document that a later write in the same burst could have made stale.
 */
final class MovieChangeStream(
  source:              MovieChangeStream.Source,
  screenings:          Option[ScreeningsRepository],
  slots:               Option[SlotsRepository],
  reread:              String => (Option[StoredMovieRecord], Boolean),
  // Marked before every `reread`, so a consumer that also writes films (the cache) can tell a
  // read taken before its own write from one taken after — see [[FilmWriteFence]].
  fence:               FilmWriteFence = new FilmWriteFence(),
  resumeToken:         ChangeStreamResumeToken,
  changeStreamMetrics: ChangeStreamMetrics,
  screeningsMetrics:   SideCollectionChangeMetrics,
  slotsMetrics:        SideCollectionChangeMetrics,
  changeDemandWindow:  Int,
  // Stamps the instant of each delivered event — injected so a spec can assert an AGE to the
  // second; production never passes it.
  clock:               java.time.Clock = java.time.Clock.systemUTC(),
  // How long after a failed re-read the film is read again — see `applyReread`. Doubled per
  // failure up to `RereadRetryMaxMillis`; a spec shortens it, production never passes it.
  rereadRetryMillis:   Long = MovieChangeStream.RereadRetryMillis,
  // Where a `movies` post-image the codec refuses is counted — see [[ChangeEventDecoder]].
  decodeFailures:      services.readmodel.DecodeFailureMetrics = services.readmodel.DecodeFailureMetrics.noop
) extends Logging with AutoCloseable {

  /** When each cursor last DELIVERED an event — the liveness signal an open-but-silent
   *  cursor has no other way of giving. Stamped on the driver's `onNext`, before the apply
   *  and before coalescing, so it says what the CURSOR did, not what the apply thread got
   *  round to. See [[ChangeStreamLiveness]]. */
  val liveness = new ChangeStreamLiveness(clock)

  // Post-images arrive undecoded (see `Source`) and are decoded here, so a document the codec
  // refuses is one skipped event rather than the end of the cursor.
  private val postImages   = ChangeEventDecoder.of[StoredMovieDto](ChangeStreamLiveness.Movies, MovieCodecs.registry, decodeFailures)
  private val movieChanges = new ChangeStreamFanout[MovieChangeStream.Delivery]("MovieRepository")
  private val changeSub    = new AtomicReference[Subscription]()
  private val changeLock   = new AnyRef
  // Applies change-stream events OFF the Mongo driver's Netty I/O event loops. The
  // apply does a blocking stitch read (`reread`) plus the synchronized
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
  // Demand windows: one per cursor, since each is a separate subscription. All drain
  // into `changeApply`, so the queue is capped at the sum of the windows.
  private val moviesDemand = new ChangeStreamDemand(changeDemandWindow)

  /** Change events handed to the apply thread but not yet applied — the depth of the
   *  queue that used to be the leak. Bounded by the demand windows; before backpressure
   *  it was bounded only by heap. Public because it is the observable form of that
   *  invariant: the integration spec asserts on it, and it is the number worth putting
   *  behind a gauge if this ever needs watching in prod. */
  def applyBacklog: Int = backlog.get()

  /** Enqueue one change-stream apply for an event `collection`'s cursor delivered: count it
   *  into the backlog and into that cursor's [[ChangeStreamLiveness]] apply lag, and release a
   *  unit of the cursor's demand once it has actually run. Every hand-off to `changeApply` goes
   *  through here so none of the three can be forgotten at a call site. */
  private def applyOffLoop(collection: String, demand: ChangeStreamDemand)(work: => Unit): Unit = {
    backlog.incrementAndGet()
    val ticket = liveness.queued(collection)
    changeApply.execute { () =>
      try work
      finally { backlog.decrementAndGet(); liveness.applied(collection, ticket); demand.applied() }
    }
  }

  // Film ids with an apply already QUEUED AND NOT YET STARTED. The set is the whole
  // coalescing mechanism — see `SideCursor.applyChange` below and the `movies` cursor's own
  // use of it in `ensureWatching`. ONE set for ALL THREE cursors (2026-09-15, was screenings
  // + movie_slots only): `dropCinemaSlots` writes `retainedSynopses` to `movies` in the SAME
  // tick it deletes the dropped venue's screenings/movie_slots rows, so a film's movies event
  // and its side-collection burst now arriving together are one re-read too, not two.
  private val sideApplyPending = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

  /** One side-collection cursor — `screenings` or `movie_slots` — with its own demand
   *  window and its own coalescing counter, ringing into the shared apply. */
  private final class SideCursor(
    collection: String,
    metrics:    SideCollectionChangeMetrics,
    open:       ((String, () => Unit) => Unit, ChangeStreamDemand) => Option[AutoCloseable]
  ) {
    val demand = new ChangeStreamDemand(changeDemandWindow)
    /** Held while one of this cursor's re-reads has failed and not yet been applied — see [[applyReread]]. */
    val hold   = new CursorHold(collection)
    private val handle = new AtomicReference[Option[AutoCloseable]](None)

    // The re-read is a BLOCKING read, so it (and the fanout) run on `changeApply`, never
    // on this cursor's I/O event loop.
    def ensureOpen(): Unit = if (handle.get().isEmpty) handle.set(open(applyChange, demand))
    def close(): Unit      = { handle.getAndSet(None).foreach(h => Try(h.close())); demand.closed() }

    /** One side-collection change: re-read the film (stitched) and fan it out, COALESCING
     *  the events that name a film an apply is already queued for.
     *
     *  Why coalescing is the point rather than a nicety. A side cursor rings once per changed
     *  DOCUMENT, i.e. once per (film, cinema slot) — so a film that really does change at
     *  every venue rings once per venue, and every ring costs a blocking stitch read plus a
     *  full re-projection OF THE SAME FILM. The widest US film carries 3,327 slots. Dropping
     *  the redundant WRITES (see `SlotKeyed.changedRows`) removed the rows that never moved;
     *  it cannot remove these, because these rows genuinely did move. The answer is that the
     *  apply does not need to run per row: it re-reads the film's CURRENT state, so one read
     *  after the last of a burst sees everything the burst did.
     *
     *  Correctness rests on the ORDER of `remove` and the read: the id is removed BEFORE the
     *  re-read, so an event that lands while we are reading finds the set clear, enqueues its
     *  own apply, and gets a read that is guaranteed to be after its own write. Removing after
     *  the read would let exactly that event be swallowed by an apply that could not have seen
     *  it. The cost of the safe order is at most one extra apply per burst.
     *
     *  A COALESCED EVENT MUST STILL RELEASE ITS DEMAND. Every delivered event owes its cursor
     *  one `applied()` or the window closes and the stream stalls for good ([[ChangeStreamDemand]]),
     *  and an event that rides an already-queued apply never reaches that task's `finally`. It
     *  releases ITS OWN cursor's demand, whichever cursor queued the apply it rides.
     *
     *  A coalesced event is NOT acknowledged to its cursor (`applied`), so that cursor's resume
     *  position does not move for it: the apply it rode was queued BEFORE it, and the events
     *  queued in between have not run yet — moving past it would move past them too. It is
     *  covered by the next acknowledged event, or replayed (a harmless re-read) after a restart.
     *
     *  `close()` closes the side cursors, but an event already in flight on a driver thread can
     *  still land after it: that enqueue is dropped on the floor (`dropRejectedAfterShutdown`), so
     *  the id stays in the set and its demand is never released. That is deliberate, not an
     *  oversight: the repository is being discarded and nobody is waiting on its cursor any more.
     *  Anything that resurrects a closed repository would have to clear the set first. */
    private def applyChange(filmId: String, applied: () => Unit): Unit = {
      liveness.delivered(collection)
      if (sideApplyPending.add(filmId))
        applyOffLoop(collection, demand) {
          sideApplyPending.remove(filmId)
          // this cursor's resume position moves only once the film is fanned out — see
          // `SideCollectionWatch` and `applyReread`
          applyReread(filmId, hold)(applied)
        }
      else {
        metrics.recordCoalescedChange()
        demand.applied()
      }
    }
  }

  /** The films whose change one cursor failed to apply, and so its resume position is held for.
   *  Touched only on `changeApply`, the single apply thread, so its reads and writes are ordered
   *  with every acknowledgement. */
  private final class CursorHold(val cursor: String) {
    private val failing = scala.collection.mutable.Set.empty[String]
    def held: Boolean = failing.nonEmpty
    /** Record `filmId`'s failure; true when it was not already failing (its retry is not yet running). */
    def fail(filmId: String): Boolean = {
      if (failing.isEmpty)
        logger.warn(s"MovieRepository change stream ($cursor): re-reading $filmId failed — its change is NOT " +
          "applied yet, and this cursor's resume position is held until it is, so a restart replays it.")
      failing.add(filmId)
    }
    def isFailing(filmId: String): Boolean = failing.contains(filmId)
    /** `filmId`'s current state has been fanned out: it no longer needs a replay. */
    def applied(filmId: String): Unit =
      if (failing.remove(filmId) && failing.isEmpty)
        logger.info(s"MovieRepository change stream ($cursor): every failed re-read is applied — releasing the held position.")
  }

  private val moviesHold = new CursorHold(ChangeStreamLiveness.Movies)
  private def holds: Seq[CursorHold] = moviesHold +: sideCursors.map(_.hold)

  private def advanceMovies(token: BsonDocument, generation: Long): Unit = {
    resumeToken.advance(token, generation)
    resumeToken.save(force = false) // time-throttled, fire-and-forget
  }

  /** One apply's re-read and fan-out, then `acknowledge` — the cursor's resume position moves
   *  only once the event is fully APPLIED, listeners included, so a shutdown cutting a fan-out
   *  short cannot have persisted its position first.
   *
   *  A re-read that FAILED is not a film that is gone ([[MovieRepository.findByIdChecked]]):
   *  nothing is fanned out, and the event is NOT applied. It is retried briefly (most failures
   *  are a blip), and if it still fails the cursor is HELD: no later event is acknowledged for
   *  the rest of this process either, because each cursor has ONE position and acknowledging a
   *  later event moves it past the failed one just the same. A held cursor keeps applying —
   *  the site stays live — and a restart resumes from before the failure and replays it (a
   *  harmless re-read of everything since). If that replay has fallen out of the oplog window,
   *  the invalid-token path starts fresh and the periodic backstop resyncs, as for any gap.
   *
   *  …and the FILM is read again later ([[rereadLater]]), until a read answers. The held
   *  position only helps the next process, and a worker runs for days: until then the film's
   *  projection kept whatever the failed event should have replaced — a changed showtime, which
   *  the read model's id-only sweeps never see. Once every failed film of a cursor has been read
   *  and fanned out (by its retry, or by any later apply of it — each reads the film's CURRENT
   *  state), nothing is left to replay, and the cursor acknowledges again from its next applied
   *  event. Held for good, one blip froze the persisted position for the rest of the process. */
  private def applyReread(filmId: String, hold: CursorHold)(acknowledge: () => Unit): Unit =
    if (rereadAndDispatch(filmId)) { if (!hold.held) acknowledge() }
    else if (hold.fail(filmId)) rereadLater(filmId, hold, rereadRetryMillis)

  /** Re-read `filmId` (a few quick attempts) and fan it out, with the fence mark the
   *  successful read was taken under; false when every read failed. */
  private def rereadAndDispatch(filmId: String): Boolean = {
    def markedRead() = { val mark = fence.mark(filmId); val (film, read) = reread(filmId); (film, read, mark) }
    var attempt = 1
    var (film, read, mark) = markedRead()
    while (!read && attempt < MovieChangeStream.RereadAttempts) {
      Thread.sleep(MovieChangeStream.RereadBackoffMillis * attempt)
      attempt += 1
      val next = markedRead(); film = next._1; read = next._2; mark = next._3
    }
    if (read) {
      film.foreach(f => movieChanges.dispatchUpsert(MovieChangeStream.Delivery(f, mark)))
      holds.foreach(_.applied(filmId))
    }
    read
  }

  // Schedules the late re-reads; the re-read itself still runs on `changeApply`, in order.
  private val rereadRetry = tools.DaemonExecutors.scheduler("movie-change-reread-retry")

  /** Read a film whose re-read failed again after `delayMillis`, doubling the delay while it
   *  keeps failing — ONE such chain per failing film and cursor, however many of its events
   *  fail meanwhile. It stops once any apply has read the film (it is no longer failing). It
   *  joins the coalescing set like any event: when an apply is already queued for the film,
   *  that apply's read serves, and this chain looks again after the next delay. It
   *  acknowledges nothing — its cursor is held — and owes no demand, since no cursor delivered it. */
  private def rereadLater(filmId: String, hold: CursorHold, delayMillis: Long): Unit = {
    val next = math.min(delayMillis * 2, MovieChangeStream.RereadRetryMaxMillis)
    scala.util.Try(rereadRetry.schedule((() =>
      if (!sideApplyPending.add(filmId)) rereadLater(filmId, hold, next)
      else {
        backlog.incrementAndGet()
        changeApply.execute { () =>
          try {
            sideApplyPending.remove(filmId)
            if (hold.isFailing(filmId) && !rereadAndDispatch(filmId)) {
              logger.warn(s"MovieRepository change stream (${hold.cursor}): re-reading $filmId still fails — trying again later.")
              rereadLater(filmId, hold, next)
            }
          } finally backlog.decrementAndGet()
        }
      }): Runnable, delayMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
      // Rejected only once `close()` has shut the scheduler: the repository is being discarded.
      .failed.foreach(exception => logger.debug(s"re-read retry of $filmId not scheduled: ${exception.getMessage}"))
  }

  // Read-split only: a showtime change writes only `screenings` and a venue's slot only
  // `movie_slots` (movies stays put), so without these the projector would never see either.
  private val sideCursors: Seq[SideCursor] = Seq(
    new SideCursor(ChangeStreamLiveness.Screenings, screeningsMetrics,
      (onChange, demand) => screenings.flatMap(_.watchApplied(onChange, demand))),
    new SideCursor(ChangeStreamLiveness.Slots, slotsMetrics,
      (onChange, demand) => slots.flatMap(_.watchApplied(onChange, demand))))

  // A change stream's onError is TERMINAL — nothing brings the cursor back on its own, and
  // `ensureWatching` only runs on REGISTRATION, which the worker does twice at boot and never
  // again. Without this driver one terminal error killed the worker's stream until the process
  // restarted (see [[ChangeStreamReopen]] for the outage that proved it). Skips the reopen once
  // the last listener has detached, so an idle repository stays idle.
  private val changeReopen = ChangeStreamReopen.onDaemonScheduler("MovieRepository",
    () => if (!movieChanges.isEmpty) ensureWatching())

  /** Attach a consumer, starting the shared cursor if it isn't running; the returned
   *  handle detaches just that consumer and stops the cursor once none remain. */
  def watch(onUpsert: StoredMovieRecord => Unit, onDelete: String => Unit): AutoCloseable =
    watchFenced((film, _) => onUpsert(film), onDelete)

  /** [[watch]] handing each upsert the [[FilmWriteFence]] mark its re-read was taken under. */
  def watchFenced(onUpsert: (StoredMovieRecord, Long) => Unit, onDelete: String => Unit): AutoCloseable = {
    val handle = movieChanges.register(d => onUpsert(d.film, d.mark), onDelete)
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
      source.open(resumeFrom, new Observer[ChangeStreamDocument[BsonDocument]] {
        override def onSubscribe(s: Subscription): Unit = {
          changeSub.set(s); moviesDemand.opened(s); liveness.watching(ChangeStreamLiveness.Movies)
        }
        override def onNext(change: ChangeStreamDocument[BsonDocument]): Unit = {
          changeReopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
          liveness.delivered(ChangeStreamLiveness.Movies)
          recordChangeMetrics(change)
          // The resume position moves only once this event is APPLIED — in its apply task,
          // AFTER the fan-out (see `applyReread`). Advancing HERE, at delivery, persisted a
          // position past every delivered-but-queued event (up to a demand window of them), so a
          // restart resumed after events that were never applied. The generation guards against
          // a `clear()` (invalid token) landing while this event is still queued.
          val token      = change.getResumeToken
          val generation = resumeToken.generation
          val deletedId    = Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
            .map(v => if (v.isString) v.asString.getValue else v.toString)
          // Apply OFF the Netty I/O loop: the stitch read + projection must not run
          // there (they made the loops contend + spin — see `changeApply`).
          postImages.postImage(change) match {
            // The movies doc has no showtimes — reread the film STITCHED (via `reread`, the
            // same by-id read the side cursors use) before fanning out, so consumers get a
            // full row. A failed read fans out NOTHING (an empty-cinema record here is what the
            // projector turns into a screenings wipe) and holds the position — see `applyReread`.
            //
            // COALESCE with a same-film apply already queued — by an earlier movies event,
            // or by the screenings/movie_slots cursors sharing this pending set. This is the
            // common case, not an edge case: `dropCinemaSlots` writes `retainedSynopses` to
            // `movies` in the SAME tick it deletes the dropped venue's screenings/movie_slots
            // rows, so a slot drop used to buy the film TWO re-projections (one bought here,
            // one by the coalesced side burst) instead of one. `reread(dto._id)` re-reads the
            // WHOLE film — movies row included — FRESH at call time regardless of which event's
            // apply actually runs, so whichever one fires sees every write of the burst,
            // including a SECOND, independent `movies`-doc write racing the first one's still-
            // queued apply. (An earlier version of this branch called `decode(dto)`, which
            // re-stitched the side collections fresh but reused the TRIGGERING event's own
            // captured document for the movies-level fields — so a second real movies-doc write
            // landing before the first apply's `remove` ran could be coalesced away and its
            // content silently lost, since nothing about that path re-read `movies` itself.
            // `reread` closes that window the same way the side cursors' own re-read already
            // does, at the cost of one extra point read per movies apply.)
            case ChangeEventDecoder.PostImage.Present(dto) =>
              if (sideApplyPending.add(dto._id))
                applyOffLoop(ChangeStreamLiveness.Movies, moviesDemand) {
                  sideApplyPending.remove(dto._id)
                  applyReread(dto._id, moviesHold)(() => advanceMovies(token, generation))
                }
              else { // not advanced — see `SideCursor.applyChange` on why a coalesced event must not be
                changeStreamMetrics.recordCoalescedChange()
                moviesDemand.applied()
              }
            // No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't back-fill). Surface
            // its _id so consumers can drop the row. Never coalesced: once a row is gone
            // there is nothing to re-read, and every delete must still reach the fan-out.
            //
            // And a delete is a coalescing BARRIER: it clears the id from the pending set, so a
            // later event for the same id queues its own apply AFTER the delete instead of riding
            // one queued before it. Ids are derived from the creation key (`FilmId.fresh`), so a
            // film deleted and re-created under the same key comes back with the same id — riding
            // the earlier apply would dispatch the re-created film first and the delete last,
            // leaving every listener on "deleted" for a film that exists. (The earlier apply's own
            // `remove` may then clear the later one's marker; that only costs an extra apply.)
            case ChangeEventDecoder.PostImage.Absent =>
              deletedId.foreach(sideApplyPending.remove)
              applyOffLoop(ChangeStreamLiveness.Movies, moviesDemand) {
                deletedId.foreach(movieChanges.dispatchDelete)
                if (!moviesHold.held) advanceMovies(token, generation)
              }
            // A document the codec refuses (counted and logged by the decoder). Nothing to apply,
            // and it must not END the cursor: decoded inside the driver, it did — and a cursor
            // resuming from a persisted token met it again on every reopen, dead for good. Its
            // demand is released; its position is NOT acknowledged, since nothing was applied —
            // the next applied event moves past it, and a restart replays it into the same skip.
            case ChangeEventDecoder.PostImage.Undecodable =>
              moviesDemand.applied()
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
      // Also watch the side collections: a showtime or slot change fires there, not on `movies`.
      sideCursors.foreach(_.ensureOpen())
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
      sideCursors.foreach(_.close())
    }
  }

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatching: Boolean = changeSub.get() != null

  /** Count each change event by op, and each UPDATE by which field kind changed,
   *  onto the injected sink (noop unless the worker wired the Prometheus one). Best
   *  effort — instrumentation must never break the stream. */
  private def recordChangeMetrics(change: ChangeStreamDocument[BsonDocument]): Unit = Try {
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
   *  persisted synchronously, every demand window closed, the apply thread stopped. */
  override def close(): Unit = changeLock.synchronized {
    changeReopen.close()
    rereadRetry.shutdownNow()
    Option(changeSub.getAndSet(null)).foreach(_.unsubscribe())
    moviesDemand.closed()
    // Let the applies already queued FINISH (bounded) before the final positions are saved:
    // saved first, a position misses them; and the apply thread is a daemon, so JVM exit
    // would otherwise cut a fan-out short.
    changeApply.shutdown()
    if (!changeApply.awaitTermination(MovieChangeStream.CloseDrainSeconds, java.util.concurrent.TimeUnit.SECONDS))
      logger.warn(s"MovieRepository change stream: applies still running after ${MovieChangeStream.CloseDrainSeconds}s " +
        "at close — saving the positions they have reached; a restart replays the rest.")
    resumeToken.save(force = true)
    // Each side handle stops its own reopen driver too — left open, a Mongo side cursor whose
    // client is closed under it would fail and reschedule its reopen for the life of the JVM.
    // Closing one persists its final position, so it comes after the drain as well.
    sideCursors.foreach(_.close())
  }
}

object MovieChangeStream {
  /** One re-read film on its way to the listeners, with the fence mark taken before the read. */
  private final case class Delivery(film: StoredMovieRecord, mark: Long)

  /** How many times an apply re-reads a film whose read failed before holding its cursor. */
  private[movies] val RereadAttempts      = 3
  private[movies] val RereadBackoffMillis = 100L
  /** When a film whose re-read failed is first read again, and the most its delay doubles to. */
  private[movies] val RereadRetryMillis    = 30_000L
  private[movies] val RereadRetryMaxMillis = 600_000L
  /** How long `close()` waits for queued applies before saving the final positions. */
  private[movies] val CloseDrainSeconds   = 10L

  /** Where the `movies` events come from. `resumeAfter` is the persisted position to
   *  reopen from (None opens at "now"); the observer receives every delivered change, its
   *  post-image UNDECODED — the stream decodes it, so a document the codec refuses cannot
   *  end the cursor (see [[ChangeEventDecoder]]). */
  trait Source {
    def open(resumeAfter: Option[BsonDocument], observer: Observer[ChangeStreamDocument[BsonDocument]]): Unit
  }

  object Source {
    /** Production: the collection's own change stream, with post-images looked up. */
    def ofCollection(c: MongoCollection[StoredMovieDto]): Source = new Source {
      override def open(resumeAfter: Option[BsonDocument], observer: Observer[ChangeStreamDocument[BsonDocument]]): Unit = {
        val base = c.watch[BsonDocument]().fullDocument(FullDocument.UPDATE_LOOKUP)
        resumeAfter.fold(base)(t => base.resumeAfter(Document(t))).subscribe(observer)
      }
    }
  }
}
