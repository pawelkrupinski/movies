package services.movies

import tools.ChangeStreamLiveness

import models.StoredMovieRecord

import play.api.Logging

import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/**
 * The APPLY side of [[MovieChangeStream]]: every film-change event its three cursors deliver
 * becomes a re-read of that film, coalesced with the film's other events and applied off the
 * driver's I/O loop on ONE thread, then fanned out to the listeners — from the changed venues
 * alone when every listener can take that ([[applyVenues]]), else the whole film re-read. A re-read
 * that fails holds its cursor's position ([[CursorHold]]) and is read again later until it answers.
 *
 * `reread` re-reads a film by id, stitched (showtimes and slots live in their own collections) —
 * after one of ITS side-collection rows changed, or after its own `movies`-doc apply is the one
 * chosen to run, so a coalesced burst is always resolved by a read that is fresh at the moment it
 * actually executes, never a captured document that a later write in the same burst could have
 * made stale.
 */
final class MovieChangeApply(
  fanout:              ChangeStreamFanout[MovieChangeApply.Delivery, MovieChangeApply.VenueDelivery],
  reread:              String => tools.ReadOutcome[StoredMovieRecord],
  // Marked before every `reread`, so a consumer that also writes films (the cache) can tell a
  // read taken before its own write from one taken after — see [[FilmWriteFence]].
  fence:               FilmWriteFence,
  liveness:            ChangeStreamLiveness,
  changeStreamMetrics: ChangeStreamMetrics,
  changeDemandWindow:  Int,
  // The debounce runs on it.
  clock:               java.time.Clock,
  // How long after a failed re-read the film is read again — see `applyReread`. Doubled per
  // failure up to `RereadRetryMaxMillis`.
  rereadRetryMillis:   Long,
  // How long a film's re-read waits for the rest of its burst — see [[MovieChangeStream.Debounce]].
  // None queues each film's re-read at once.
  debounce:            Option[MovieChangeStream.Debounce],
  // Reads a film's slots at some venues as a whole-film read stitches them — what lets a change
  // confined to a few venues' showtimes skip the whole-film re-read ([[applyVenues]]). None
  // re-reads every change whole.
  readVenues:          Option[(String, Set[models.Cinema]) => Option[VenueSlots]],
  // Waits out each film's debounce (built only when `debounce` is set).
  debounceScheduler:   () => java.util.concurrent.ScheduledExecutorService
) extends Logging {

  // Applies change-stream events OFF the Mongo driver's Netty I/O event loops. The
  // apply does a blocking stitch read (`reread`) plus the synchronized
  // read-model projection; running that on the I/O loops made the two loops contend
  // the projection monitor and busy-spin their wakeup eventfds (~24cc, ~0 voluntary
  // ctx-switches — proven on-box), flooring the shared-CPU credit. A SINGLE thread
  // keeps events applied strictly in order.
  //
  // What keeps the backlog finite is the demand window each cursor opens with — see
  // [[ChangeStreamDemand]]; the queue's own capacity sits above all three windows and is
  // only the backstop. Every `changeApply.execute` here must therefore be paired with an
  // `applied()` in the task's `finally`, or that cursor stalls; `applyBacklog` is the
  // invariant made observable.
  private val changeApply  = tools.DaemonExecutors.singleThreadExecutor("movie-change-apply",
    MovieChangeApply.applyQueueCapacity(changeDemandWindow))
  private val backlog      = new AtomicInteger(0)

  /** Change events handed to the apply thread but not yet applied — the depth of the
   *  queue that used to be the leak. Bounded by the demand windows; before backpressure
   *  it was bounded only by heap. Public because it is the observable form of that
   *  invariant: the integration spec asserts on it, and it is the number worth putting
   *  behind a gauge if this ever needs watching in prod. */
  def applyBacklog: Int = backlog.get()

  /** Block until every apply queued before this call has run — the apply thread is single and
   *  FIFO, so a no-op queued behind them runs only after them. The happens-before a spec needs to
   *  read what an apply wrote (its resume position, its liveness count) without betting on how
   *  long the apply thread takes to get there. Not a cursor's event, so it owes no demand.
   *  False when `timeoutSeconds` passed first, or the stream is closed. */
  private[movies] def awaitQueuedApplies(timeoutSeconds: Long = 10): Boolean = {
    val ran = new java.util.concurrent.CountDownLatch(1)
    scala.util.Try(changeApply.execute(() => ran.countDown())).isSuccess &&
      ran.await(timeoutSeconds, java.util.concurrent.TimeUnit.SECONDS)
  }

  /** Enqueue one change-stream apply for an event `collection`'s cursor delivered: count it
   *  into the backlog and into that cursor's [[ChangeStreamLiveness]] apply lag, and release a
   *  unit of the cursor's demand once it has actually run. Every hand-off to `changeApply` goes
   *  through here so none of the three can be forgotten at a call site. */
  private[movies] def applyOffLoop(collection: String, demand: ChangeStreamDemand)(work: => Unit): Unit = {
    backlog.incrementAndGet()
    val ticket = liveness.queued(collection)
    changeApply.execute { () =>
      try work
      finally { backlog.decrementAndGet(); liveness.applied(collection, ticket); demand.applied() }
    }
  }

  /** A film with a re-read pending: every event riding it — from any of the three cursors — as the
   *  acknowledgement its cursor's [[AppliedPrefix]] waits on, and, while the debounce holds it,
   *  when it is due. `queued` once it is handed to the apply thread; it still collects riders
   *  until the apply TAKES it, just before its read. */
  private final class Pending(val openedAt: Long, val cursor: String, val demand: ChangeStreamDemand, val hold: CursorHold) {
    val acks            = new java.util.concurrent.ConcurrentLinkedQueue[() => Unit]()
    @volatile var dueAt = openedAt
    // Open in the liveness record from now until the re-read is over — held by the debounce
    // included — so a prune sweep's heal verdict waits for it ([[ChangeStreamLiveness.reread]]).
    val ticket          = liveness.reread()
    // The venues every event riding this re-read changed showtimes at — or None once one rode it
    // that was not a `screenings` row at a known venue, and only the whole film will do.
    @volatile var venues: Option[Set[models.Cinema]] = Some(Set.empty)
    private val handedOff = new java.util.concurrent.atomic.AtomicBoolean(false)
    def queued: Boolean = handedOff.get()
    /** True for exactly one caller: the timer and `close` may race to queue it. */
    def handOff(): Boolean = handedOff.compareAndSet(false, true)
  }

  // The films with a re-read pending, and what rides each. The map is the whole coalescing
  // mechanism — see [[SideChangeCursor]]'s `applyChange` and the `movies` cursor's own use of it in
  // [[MoviesChangeCursor]]. ONE map for ALL THREE cursors (2026-09-15, was screenings + movie_slots
  // only): `dropCinemaSlots` writes `retainedSynopses` to `movies` in the SAME tick it deletes the
  // dropped venue's screenings/movie_slots rows, so a film's movies event and its side-collection
  // burst arriving together are one re-read too, not two.
  private val pending = new java.util.concurrent.ConcurrentHashMap[String, Pending]()

  /** Ride `filmId`'s pending re-read with `ack`, opening one if none is pending; the new one when
   *  this event opened it (its caller schedules it), else None. A `debounced` event pushes a re-read
   *  the debounce still holds to `quiet` after it — never past `cap` after it opened. Only the side
   *  cursors' events are: they come in a film's bursts, while a `movies` write is one enrichment
   *  landing, which opens a re-read due at once (and rides a held one without moving it). */
  private def ride(filmId: String, cursor: String, demand: ChangeStreamDemand, ack: () => Unit,
                   debounced: Boolean, venue: Option[models.Cinema]): Option[Pending] = {
    var opened = Option.empty[Pending]
    pending.compute(filmId, (_, existing) => {
      val now   = clock.millis()
      val entry = Option(existing).getOrElse { val fresh = new Pending(now, cursor, demand, holdOf(cursor)); opened = Some(fresh); fresh }
      entry.acks.add(ack)
      entry.venues = for { seen <- entry.venues; at <- venue } yield seen + at
      debounce.filter(_ => debounced && !entry.queued)
        .foreach(d => entry.dueAt = math.min(now + d.quiet.toMillis, entry.openedAt + d.cap.toMillis))
      entry
    })
    opened
  }

  /** Re-read `filmId` for an event `cursor` delivered, acknowledged by `ack` once the film is
   *  applied: true when the event opened the film's re-read (and it is scheduled), false when it
   *  rode one already pending — whose caller still owes its cursor's demand the event's
   *  `applied()`, since a rider never reaches that re-read's `finally`. See [[ride]]. */
  private[movies] def request(filmId: String, cursor: String, demand: ChangeStreamDemand, ack: () => Unit,
                              debounced: Boolean, venue: Option[models.Cinema] = None): Boolean =
    ride(filmId, cursor, demand, ack, debounced, venue) match {
      case Some(opened) => schedule(filmId, opened); true
      case None         => false
    }

  /** A delete is a coalescing BARRIER (see `MovieChangeStream`'s delete apply): `id`'s re-read, when
   *  one is already QUEUED, is taken out of `pending`, so a later event for the same id queues its
   *  own apply after the delete. One the debounce still holds is queued after the delete, so it runs
   *  after it and reads whatever the key holds then. */
  private[movies] def barrier(id: String): Unit =
    pending.computeIfPresent(id, (_, entry) => if (entry.queued) null else entry)

  /** Re-reads the debounce is holding for the rest of their film's burst. */
  def held: Int = pending.values().asScala.count(!_.queued)

  /** Queue every re-read the debounce holds, now — at close, so their events are applied before
   *  the final positions are saved, and wherever a caller must see the stream settled. */
  def releaseHeld(): Unit = pending.forEach((filmId, entry) => queue(filmId, entry))

  // Waits out a film's debounce; the re-read itself runs on `changeApply`, like every apply.
  private lazy val debouncer = debounceScheduler()

  /** Queue `entry`'s re-read once its film is due: at once without a debounce, else when its due
   *  time — moved by every event that rides it — has passed. */
  private def schedule(filmId: String, entry: Pending): Unit = {
      val wait = entry.dueAt - clock.millis()
      if (wait <= 0) queue(filmId, entry)
      else scala.util.Try(debouncer.schedule((() => schedule(filmId, entry)): Runnable, wait, java.util.concurrent.TimeUnit.MILLISECONDS))
        .failed.foreach(_ => queue(filmId, entry)) // shut down by `close`: queue it now, to drain with the rest
  }

  /** Hand `entry` to the apply thread. It is TAKEN — removed, if still this film's — before the
   *  read, so an event landing during the read opens its own re-read and gets a read after its own
   *  write; the acknowledgements riding it are released once the film is applied. */
  private def queue(filmId: String, entry: Pending): Unit = if (entry.handOff()) {
    applyOffLoop(entry.cursor, entry.demand) {
      pending.remove(filmId, entry)
      applyChange(filmId, entry.venues, entry.hold, drain(entry), () => liveness.finished(entry.ticket))
    }
  }

  private def drain(entry: Pending): Seq[() => Unit] =
    Iterator.continually(entry.acks.poll()).takeWhile(_ != null).toSeq

  /** The films whose re-read one cursor failed, and the acknowledgements of the events waiting on
   *  them: those events are not applied, so their cursor's position cannot move past them
   *  ([[AppliedPrefix]]) and a restart replays them. Touched only on `changeApply`, the single
   *  apply thread. */
  private final class CursorHold(val cursor: String) {
    private val failing = scala.collection.mutable.Map.empty[String, Seq[() => Unit]]
    /** Record `filmId`'s failure with the events waiting on it; true when it was not already
     *  failing (its retry is not yet running). */
    def fail(filmId: String, acks: Seq[() => Unit]): Boolean = {
      if (failing.isEmpty)
        logger.warn(s"MovieRepository change stream ($cursor): re-reading $filmId failed — its change is NOT " +
          "applied yet, and this cursor's resume position stays before it until it is, so a restart replays it.")
      val first = !failing.contains(filmId)
      failing.update(filmId, failing.getOrElse(filmId, Seq.empty) ++ acks)
      first
    }
    def isFailing(filmId: String): Boolean = failing.contains(filmId)
    /** `filmId`'s current state has been fanned out: the events that waited on it are applied. */
    def applied(filmId: String): Unit =
      failing.remove(filmId).foreach { acks =>
        acks.foreach(_())
        if (failing.isEmpty) logger.info(s"MovieRepository change stream ($cursor): every failed re-read is applied.")
      }
  }

  // One per cursor, in the order the stream runs them.
  private val holds: Seq[CursorHold] = ChangeStreamLiveness.Collections.map(new CursorHold(_))
  private def holdOf(cursor: String): CursorHold =
    holds.find(_.cursor == cursor).getOrElse(throw new IllegalArgumentException(s"no change-stream cursor $cursor"))

  /** Apply a change confined to some venues' showtimes from those venues alone: their slots read
   *  and offered to every listener as a [[VenueSlots]]. True when every listener applied it; false —
   *  and the caller re-reads the film whole, for every listener — when it was not confined to known
   *  venues, touched too many, or its film is failing a re-read (whose retry must read it whole),
   *  when the venue read failed, or when any listener declined. A whole re-read after a partial
   *  apply is harmless: it is a superset of every part. A US wide release carries thousands of
   *  venues' showtimes, and a change at one of them re-read every one: most of a busy US worker's
   *  change-apply CPU (JFR, 2026-10-01). */
  private def applyVenues(filmId: String, venues: Option[Set[models.Cinema]]): Boolean = {
    import ChangeStreamMetrics.Apply.Reason
    val reason = readVenues match {
      case None       => Reason.Unsupported
      case Some(read) => venues.filter(_.nonEmpty) match {
        case None                                                    => Reason.NotShowtimes
        case Some(at) if at.sizeIs > MovieChangeApply.MaxVenuesApplied       => Reason.TooManyVenues
        case Some(_) if holds.exists(_.isFailing(filmId))            => Reason.Failing
        case Some(at)                                                => deliverVenues(filmId, at, read)
      }
    }
    val applied = reason == Reason.Applied
    changeStreamMetrics.recordApply(if (applied) ChangeStreamMetrics.Apply.Venues else ChangeStreamMetrics.Apply.Film, reason)
    applied
  }

  /** Read the film's slots at `at` under a fence mark and dispatch them to the listeners: why it did not apply, or
   *  `Applied`. */
  private def deliverVenues(filmId: String, at: Set[models.Cinema],
                            read: (String, Set[models.Cinema]) => Option[VenueSlots]): String = {
    import ChangeStreamMetrics.Apply.Reason
    val mark = fence.mark(filmId)
    read(filmId, at) match {
      case None        => Reason.VenueReadFailed
      case Some(slots) =>
        val declined = fanout.dispatchPart(MovieChangeApply.VenueDelivery(slots, mark))
          .collect { case VenueVerdict.Declined(why) => why }
        declined.foreach(changeStreamMetrics.recordVenueDecline)
        if (declined.isEmpty) Reason.Applied else Reason.Declined
    }
  }

  /** Apply a film's change from its venues alone when every listener can, else re-read it whole. */
  private def applyChange(filmId: String, venues: Option[Set[models.Cinema]], hold: CursorHold,
                          acks: Seq[() => Unit], finished: () => Unit): Unit =
    try { if (applyVenues(filmId, venues)) acks.foreach(_()) else applyReread(filmId, hold)(acks) }
    finally finished()

  /** One apply's re-read and fan-out, then the `acks` of every event it covers — a cursor's resume
   *  position moves only once an event is fully APPLIED, listeners included, so a shutdown
   *  cutting a fan-out short cannot have persisted its position first.
   *
   *  A re-read that FAILED is not a film that is gone ([[MovieRepository.findByIdChecked]]):
   *  nothing is fanned out, and the events are NOT applied. It is retried briefly (most failures
   *  are a blip), and if it still fails their acknowledgements wait in the cursor's [[CursorHold]]:
   *  the position cannot move past them ([[AppliedPrefix]]), so a restart resumes from before the
   *  failure and replays it (a harmless re-read of everything since), while the cursor keeps
   *  applying later events — the site stays live. If that replay has fallen out of the oplog
   *  window, the invalid-token path starts fresh and the periodic backstop resyncs, as for any gap.
   *
   *  …and the FILM is read again later ([[rereadLater]]), until a read answers: a worker runs for
   *  days, and until then the film's projection kept whatever the failed event should have
   *  replaced — a changed showtime, which the read model's id-only sweeps never see. Once a read
   *  of the film is fanned out (by its retry, or by any later apply of it — each reads the film's
   *  CURRENT state), its waiting events are applied and acknowledged. */
  private def applyReread(filmId: String, hold: CursorHold)(acks: Seq[() => Unit]): Unit =
    if (rereadAndDispatch(filmId)) acks.foreach(_())
    else if (hold.fail(filmId, acks)) rereadLater(filmId, hold, rereadRetryMillis)

  /** Re-read `filmId` (a few quick attempts) and fan it out, with the fence mark the
   *  successful read was taken under; false when every read failed. */
  private def rereadAndDispatch(filmId: String): Boolean = {
    def markedRead() = { val mark = fence.mark(filmId); val read = reread(filmId); (read.answered, !read.isFailed, mark) }
    var attempt = 1
    var (film, read, mark) = markedRead()
    while (!read && attempt < MovieChangeApply.RereadAttempts) {
      Thread.sleep(MovieChangeApply.RereadBackoffMillis * attempt)
      attempt += 1
      val next = markedRead(); film = next._1; read = next._2; mark = next._3
    }
    if (read) {
      film.foreach(f => fanout.dispatchUpsert(MovieChangeApply.Delivery(f, mark)))
      holds.foreach(_.applied(filmId))
    }
    read
  }

  // Schedules the late re-reads; the re-read itself still runs on `changeApply`, in order.
  private val rereadRetry = tools.DaemonExecutors.scheduler("movie-change-reread-retry")

  /** Read a film whose re-read failed again after `delayMillis`, doubling the delay while it
   *  keeps failing — ONE such chain per failing film and cursor, however many of its events
   *  fail meanwhile. It stops once any apply has read the film (it is no longer failing). It
   *  defers to any re-read pending for the film: that read serves, and this chain looks again
   *  after the next delay. Its success acknowledges the events waiting on the film (through the
   *  [[CursorHold]]); it owes no demand, since no cursor delivered it. */
  private def rereadLater(filmId: String, hold: CursorHold, delayMillis: Long): Unit = {
    val next = math.min(delayMillis * 2, MovieChangeApply.RereadRetryMaxMillis)
    scala.util.Try(rereadRetry.schedule((() =>
      if (pending.containsKey(filmId)) rereadLater(filmId, hold, next)
      else {
        backlog.incrementAndGet()
        changeApply.execute { () =>
          try {
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

  /** No more late re-reads: the stream is closing. */
  private[movies] def stopRetries(): Unit = rereadRetry.shutdownNow()

  /** Drain at close: a re-read the debounce still holds is queued now, so it drains with the rest
   *  and its events are applied before the final positions are saved; then the applies already
   *  queued FINISH (bounded) — saved first, a position misses them; and the apply thread is a
   *  daemon, so JVM exit would otherwise cut a fan-out short. */
  private[movies] def drainForClose(): Unit = {
    if (debounce.isDefined) debouncer.shutdownNow()
    releaseHeld()
    changeApply.shutdown()
    if (!changeApply.awaitTermination(MovieChangeApply.CloseDrainSeconds, java.util.concurrent.TimeUnit.SECONDS))
      logger.warn(s"MovieRepository change stream: applies still running after ${MovieChangeApply.CloseDrainSeconds}s " +
        "at close — saving the positions they have reached; a restart replays the rest.")
  }
}

object MovieChangeApply {
  /** Where each film's debounce is waited out when no spec steps it by hand: a daemon scheduler of its own. */
  val DebounceScheduler: () => java.util.concurrent.ScheduledExecutorService =
    () => tools.DaemonExecutors.scheduler("movie-change-debounce")

  /** The apply queue's capacity: the three cursors' demand windows, each event at most one task,
   *  plus room for the re-read retries that owe no demand. */
  private[movies] def applyQueueCapacity(window: Int): Int = math.min(Int.MaxValue.toLong, 3L * window + 4096L).toInt

  /** One re-read film on its way to the listeners, with the fence mark taken before the read. */
  private[movies] final case class Delivery(film: StoredMovieRecord, mark: Long)

  /** Some of a film's venues on their way to the listeners, with the fence mark taken before the read. */
  private[movies] final case class VenueDelivery(venues: VenueSlots, mark: Long)

  /** The most venues one apply reads alone; a burst across more re-reads the film whole. */
  private[movies] val MaxVenuesApplied = 16

  /** How many times an apply re-reads a film whose read failed before holding its cursor. */
  private[movies] val RereadAttempts      = 3
  private[movies] val RereadBackoffMillis = 100L
  /** When a film whose re-read failed is first read again, and the most its delay doubles to. */
  private[movies] val RereadRetryMillis    = 30_000L
  private[movies] val RereadRetryMaxMillis = 600_000L
  /** How long `close()` waits for queued applies before saving the final positions. */
  private[movies] val CloseDrainSeconds   = 10L
}
