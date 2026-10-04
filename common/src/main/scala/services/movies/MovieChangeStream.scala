package services.movies

import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import org.bson.BsonDocument
import org.mongodb.scala.{Document, MongoCollection, Observer, Subscription}
import play.api.Logging

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*
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
  reread:              String => tools.ReadOutcome[StoredMovieRecord],
  // Marked before every `reread`, so a consumer that also writes films (the cache) can tell a
  // read taken before its own write from one taken after — see [[FilmWriteFence]].
  fence:               FilmWriteFence = new FilmWriteFence(),
  resumeToken:         ChangeStreamResumeToken,
  changeStreamMetrics: ChangeStreamMetrics,
  screeningsMetrics:   SideCollectionChangeMetrics,
  slotsMetrics:        SideCollectionChangeMetrics,
  changeDemandWindow:  Int,
  // The repository's clock: the debounce and venue waits run on it, and the liveness ages from it.
  clock:               java.time.Clock,
  // The sequence the repository stamps `updatedAt` from — the liveness takes every catch-up floor
  // from it, so a floor and a row's stamp are strictly ordered (see [[ChangeStreamLiveness.now]]).
  stamps:              tools.MonotonicStampSequence,
  // How long after a failed re-read the film is read again — see `applyReread`. Doubled per
  // failure up to `RereadRetryMaxMillis`; a spec shortens it, production never passes it.
  rereadRetryMillis:   Long = MovieChangeStream.RereadRetryMillis,
  // Where a `movies` post-image the codec refuses is counted — see [[ChangeEventDecoder]].
  decodeFailures:      services.readmodel.DecodeFailureMetrics = services.readmodel.DecodeFailureMetrics.noop,
  // How long a film's re-read waits for the rest of its burst — see [[MovieChangeStream.Debounce]].
  // None queues each film's re-read at once, as every repository but the worker's does.
  debounce:            Option[MovieChangeStream.Debounce] = None,
  // Reads a film's slots at some venues as a whole-film read stitches them — what lets a change
  // confined to a few venues' showtimes skip the whole-film re-read ([[applyVenues]]). None (every
  // repository but the worker's) re-reads every change whole.
  readVenues:          Option[(String, Set[models.Cinema]) => Option[VenueSlots]] = None,
  // How often a venue apply a listener was not ready for asks again, and how long it waits at most.
  venueRetryMillis:    Long = MovieChangeStream.VenueRetryMillis,
  venueWaitMillis:     Long = MovieChangeStream.VenueWaitMillis,
  // Waits out each film's debounce (built only when `debounce` is set) — injected so a spec can
  // step a debounce boundary on a hand-moved `clock` instead of sleeping across it.
  debounceScheduler:   () => java.util.concurrent.ScheduledExecutorService = () => tools.DaemonExecutors.scheduler("movie-change-debounce")
) extends Logging with AutoCloseable {

  /** When each cursor last DELIVERED an event — the liveness signal an open-but-silent
   *  cursor has no other way of giving. Stamped on the driver's `onNext`, before the apply
   *  and before coalescing, so it says what the CURSOR did, not what the apply thread got
   *  round to. See [[ChangeStreamLiveness]]. */
  val liveness = new ChangeStreamLiveness(clock, Some(stamps))

  // Post-images arrive undecoded (see `Source`) and are decoded here, so a document the codec
  // refuses is one skipped event rather than the end of the cursor.
  private val postImages   = ChangeEventDecoder.of[StoredMovieDto](ChangeStreamLiveness.Movies, MovieCodecs.registry, decodeFailures)
  private val movieChanges = new ChangeStreamFanout[MovieChangeStream.Delivery, MovieChangeStream.VenueDelivery]("MovieRepository")
  private val changeSub    = new AtomicReference[Subscription]()
  private val changeLock   = new AnyRef
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
    MovieChangeStream.applyQueueCapacity(changeDemandWindow))
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
  private def applyOffLoop(collection: String, demand: ChangeStreamDemand)(work: => Unit): Unit = {
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
  // mechanism — see `SideCursor.applyChange` below and the `movies` cursor's own use of it in
  // `ensureWatching`. ONE map for ALL THREE cursors (2026-09-15, was screenings + movie_slots
  // only): `dropCinemaSlots` writes `retainedSynopses` to `movies` in the SAME tick it deletes the
  // dropped venue's screenings/movie_slots rows, so a film's movies event and its side-collection
  // burst arriving together are one re-read too, not two.
  private val pending = new java.util.concurrent.ConcurrentHashMap[String, Pending]()

  /** Ride `filmId`'s pending re-read with `ack`, opening one if none is pending; the new one when
   *  this event opened it (its caller schedules it), else None. A `debounced` event pushes a re-read
   *  the debounce still holds to `quiet` after it — never past `cap` after it opened. Only the side
   *  cursors' events are: they come in a film's bursts, while a `movies` write is one enrichment
   *  landing, which opens a re-read due at once (and rides a held one without moving it). */
  private def ride(filmId: String, cursor: String, demand: ChangeStreamDemand, hold: CursorHold, ack: () => Unit,
                   debounced: Boolean, venue: Option[models.Cinema] = None): Option[Pending] = {
    var opened = Option.empty[Pending]
    pending.compute(filmId, (_, existing) => {
      val now   = clock.millis()
      val entry = Option(existing).getOrElse { val fresh = new Pending(now, cursor, demand, hold); opened = Some(fresh); fresh }
      entry.acks.add(ack)
      entry.venues = for { seen <- entry.venues; at <- venue } yield seen + at
      debounce.filter(_ => debounced && !entry.queued)
        .foreach(d => entry.dueAt = math.min(now + d.quiet.toMillis, entry.openedAt + d.cap.toMillis))
      entry
    })
    opened
  }

  /** Re-reads the debounce is holding for the rest of their film's burst. */
  def held: Int = pending.values().asScala.count(!_.queued) + waiting.get()

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
      applyOrWait(filmId, entry.venues, entry.hold, drain(entry), clock.millis(), () => liveness.finished(entry.ticket))
    }
  }

  private def drain(entry: Pending): Seq[() => Unit] =
    Iterator.continually(entry.acks.poll()).takeWhile(_ != null).toSeq

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
     *  Correctness rests on the ORDER of `remove` and the read: the film's entry is taken BEFORE
     *  the re-read, so an event that lands while we are reading finds none, opens its own
     *  re-read, and gets a read that is guaranteed to be after its own write. Removing after
     *  the read would let exactly that event be swallowed by an apply that could not have seen
     *  it. The cost of the safe order is at most one extra apply per burst.
     *
     *  Coalescing only while an apply is QUEUED caught just the events that arrived behind a busy
     *  apply thread: a venue scrape writes a film's rows seconds apart, a Flicks venue lands in
     *  day-chunks, and each missed the window and bought its own full re-read — ~0.6 a second on
     *  the US, each decoding every venue's showtimes of the film (its widest: 2,466 screenings
     *  rows, ~50 MB). The `debounce` holds the opening event's apply so the rest of the burst
     *  finds it pending — see [[MovieChangeStream.Debounce]] for what it saves per country.
     *
     *  A COALESCED EVENT MUST STILL RELEASE ITS DEMAND. Every delivered event owes its cursor
     *  one `applied()` or the window closes and the stream stalls for good ([[ChangeStreamDemand]]),
     *  and an event that rides an already-queued apply never reaches that task's `finally`. It
     *  releases ITS OWN cursor's demand, whichever cursor queued the apply it rides.
     *
     *  A coalesced event is acknowledged to its cursor (`applied`) with the re-read it rode, whose
     *  read came after its write. The re-reads finish out of delivery order — one film's waits
     *  out its burst while a quieter film's runs — so the cursor's position moves only past a
     *  contiguous run of acknowledged events ([[AppliedPrefix]]): a restart replays every event
     *  still waiting, as a harmless re-read of the film's current state.
     *
     *  `close()` closes the side cursors, but an event already in flight on a driver thread can
     *  still land after it: that enqueue is dropped on the floor (`dropRejectedAfterShutdown`), so
     *  the film's entry stays pending and its demand is never released. That is deliberate, not an
     *  oversight: the repository is being discarded and nobody is waiting on its cursor any more.
     *  Anything that resurrects a closed repository would have to clear `pending` first. */
    private def applyChange(rowId: String, applied: () => Unit): Unit = {
      liveness.delivered(collection)
      val filmId = SlotKeyed.filmIdOf(rowId)
      // A `screenings` row at a known venue can be applied from that venue alone — see `applyVenues`.
      val venue  = Option.when(collection == ChangeStreamLiveness.Screenings && rowId != filmId)(SlotKeyed.slotKeyOf(rowId))
        .flatMap(models.Source.byWireKey).collect { case showing: models.CinemaShowing => showing.cinema }
      // Either way the event is acknowledged only once the film is fanned out — by the re-read it
      // opened or the one it rides — see `SideCollectionWatch` and `applyReread`.
      ride(filmId, collection, demand, hold, applied, debounced = true, venue) match {
        case Some(opened) => schedule(filmId, opened)
        case None         => metrics.recordCoalescedChange(); demand.applied()
      }
    }
  }

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

  private val moviesHold = new CursorHold(ChangeStreamLiveness.Movies)
  private def holds: Seq[CursorHold] = moviesHold +: sideCursors.map(_.hold)

  private def advanceMovies(token: BsonDocument, generation: Long): Unit = {
    resumeToken.advance(token, generation)
    resumeToken.save(force = false) // time-throttled, fire-and-forget
  }
  // The movies cursor's delivered events: its position moves only past a contiguous run of applied ones.
  private val moviesPrefix = new AppliedPrefix(advanceMovies)

  /** Apply a change confined to some venues' showtimes from those venues alone: their slots read
   *  and offered to every listener as a [[VenueSlots]]. True when every listener applied it; false —
   *  and the caller re-reads the film whole, for every listener — when it was not confined to known
   *  venues, touched too many, or its film is failing a re-read (whose retry must read it whole),
   *  when the venue read failed, or when any listener declined. A whole re-read after a partial
   *  apply is harmless: it is a superset of every part. A US wide release carries thousands of
   *  venues' showtimes, and a change at one of them re-read every one: most of a busy US worker's
   *  change-apply CPU (JFR, 2026-10-01). */
  private def applyVenues(filmId: String, venues: Option[Set[models.Cinema]]): MovieChangeStream.VenueOutcome = {
    import ChangeStreamMetrics.Apply.Reason
    import MovieChangeStream.VenueOutcome
    val reason = readVenues match {
      case None       => Reason.Unsupported
      case Some(read) => venues.filter(_.nonEmpty) match {
        case None                                                    => Reason.NotShowtimes
        case Some(at) if at.sizeIs > MovieChangeStream.MaxVenuesApplied => Reason.TooManyVenues
        case Some(_) if holds.exists(_.isFailing(filmId))            => Reason.Failing
        case Some(at) =>
          val mark = fence.mark(filmId)
          read(filmId, at) match {
            case None        => Reason.VenueReadFailed
            case Some(slots) =>
              val unapplied = movieChanges.dispatchPart(MovieChangeStream.VenueDelivery(slots, mark))
              val declined  = unapplied.collect { case VenueVerdict.Declined(why) => why }
              declined.foreach(changeStreamMetrics.recordVenueDecline)
              if (unapplied.isEmpty) Reason.Applied
              else if (declined.isEmpty) return VenueOutcome.NotYet
              else Reason.Declined
          }
      }
    }
    val applied = reason == Reason.Applied
    changeStreamMetrics.recordApply(if (applied) ChangeStreamMetrics.Apply.Venues else ChangeStreamMetrics.Apply.Film, reason)
    if (applied) VenueOutcome.Applied else VenueOutcome.Whole
  }

  /** Apply a film's venues — or, when a listener is not ready for them yet, ask again every
   *  `venueRetryMillis` until it is, or until `venueWaitMillis` after the first ask, and then re-read
   *  it whole. The events stay unacknowledged the whole time, so
   *  a restart replays them; the wait counts as held ([[held]]), so a caller settling the stream waits
   *  for it too. A later change to the film queues its own apply meanwhile, as always. */
  private def applyOrWait(filmId: String, venues: Option[Set[models.Cinema]], hold: CursorHold,
                          acks: Seq[() => Unit], firstAsked: Long, finished: () => Unit): Unit = {
    // Once the film is applied, or its re-read has failed and is left to `rereadLater` — not while
    // it waits for a listener.
    def over(apply: => Unit): Unit = try apply finally finished()
    val outcome = try applyVenues(filmId, venues) catch { case thrown: Throwable => finished(); throw thrown }
    outcome match {
      case MovieChangeStream.VenueOutcome.Applied => over(acks.foreach(_()))
      case MovieChangeStream.VenueOutcome.NotYet if clock.millis() - firstAsked < venueWaitMillis =>
        waiting.incrementAndGet()
        scala.util.Try(rereadRetry.schedule((() => {
          backlog.incrementAndGet()
          changeApply.execute { () =>
            try applyOrWait(filmId, venues, hold, acks, firstAsked, finished)
            finally { waiting.decrementAndGet(); backlog.decrementAndGet() }
          }
        }): Runnable, venueRetryMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
          .failed.foreach { _ => waiting.decrementAndGet(); over(applyReread(filmId, hold)(acks)) } // shutting down: apply it whole now
      case MovieChangeStream.VenueOutcome.NotYet =>
        changeStreamMetrics.recordApply(ChangeStreamMetrics.Apply.Film, ChangeStreamMetrics.Apply.Reason.WaitExpired)
        over(applyReread(filmId, hold)(acks))
      case MovieChangeStream.VenueOutcome.Whole => over(applyReread(filmId, hold)(acks))
    }
  }

  // Venue applies waiting for a listener to be ready for them — see `applyOrWait`.
  private val waiting = new AtomicInteger(0)

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
   *  defers to any re-read pending for the film: that read serves, and this chain looks again
   *  after the next delay. Its success acknowledges the events waiting on the film (through the
   *  [[CursorHold]]); it owes no demand, since no cursor delivered it. */
  private def rereadLater(filmId: String, hold: CursorHold, delayMillis: Long): Unit = {
    val next = math.min(delayMillis * 2, MovieChangeStream.RereadRetryMaxMillis)
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

  // Read-split only: a showtime change writes only `screenings` and a venue's slot only
  // `movie_slots` (movies stays put), so without these the projector would never see either.
  private val sideCursors: Seq[SideCursor] = Seq(
    new SideCursor(ChangeStreamLiveness.Screenings, screeningsMetrics,
      (onChange, demand) => screenings.flatMap(_.watchRowsApplied(onChange, demand))),
    new SideCursor(ChangeStreamLiveness.Slots, slotsMetrics,
      (onChange, demand) => slots.flatMap(_.watchRowsApplied(onChange, demand))))

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
  def watchFenced(onUpsert: (StoredMovieRecord, Long) => Unit, onDelete: String => Unit,
                  onVenues: (VenueSlots, Long) => VenueVerdict = (_, _) => VenueVerdict.Declined(ChangeStreamFanout.NoPartHandler)): AutoCloseable = {
    val handle = attachFenced(onUpsert, onDelete, onVenues)
    ensureWatching()
    handle
  }

  /** [[watchFenced]] without opening the cursor: the consumer is attached now and dispatched every
   *  event from the cursor's first, whenever another consumer's watch — or [[open]] — opens it. For a
   *  consumer that must see everything a cursor another one opens applies: attached any later, it
   *  misses every event applied before it, and the resume position moves past them. */
  def attachFenced(onUpsert: (StoredMovieRecord, Long) => Unit, onDelete: String => Unit,
                   onVenues: (VenueSlots, Long) => VenueVerdict = (_, _) => VenueVerdict.Declined(ChangeStreamFanout.NoPartHandler)): AutoCloseable = {
    val handle = movieChanges.register(d => onUpsert(d.film, d.mark), onDelete, d => onVenues(d.venues, d.mark))
    new AutoCloseable { override def close(): Unit = { handle.close(); stopWatchingIfIdle() } }
  }

  /** Open the shared cursor for the consumers [[attachFenced]] attached, if it is not running; with
   *  none attached, an idle repository stays idle. */
  def open(): Unit = if (!movieChanges.isEmpty) ensureWatching()

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
      resumeToken.openFrom() match {
        case ChangeStreamResumeToken.Position.Deferred => changeReopen.failed()
        case ChangeStreamResumeToken.Position.At(resumeFrom) => open(resumeFrom)
      }
    }
  }

  /** Open the shared cursor after `resumeFrom` (at "now" when `None`). Called under `changeLock`. */
  private def open(resumeFrom: Option[BsonDocument]): Unit = {
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
        val ack        = moviesPrefix.deliver(token, generation)
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
          // or by the screenings/movie_slots cursors sharing `pending`. This is the
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
            ride(dto._id, ChangeStreamLiveness.Movies, moviesDemand, moviesHold, ack, debounced = false) match {
              case Some(opened) => schedule(dto._id, opened)
              case None         => changeStreamMetrics.recordCoalescedChange(); moviesDemand.applied()
            }
          // No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't back-fill). Surface
          // its _id so consumers can drop the row. Never coalesced: once a row is gone
          // there is nothing to re-read, and every delete must still reach the fan-out.
          //
          // And a delete is a coalescing BARRIER: it takes the id's queued entry out of `pending`, so a
          // later event for the same id queues its own apply AFTER the delete instead of riding
          // one queued before it. Ids are derived from the creation key (`FilmId.fresh`), so a
          // film deleted and re-created under the same key comes back with the same id — riding
          // the earlier apply would dispatch the re-created film first and the delete last,
          // leaving every listener on "deleted" for a film that exists.
          case ChangeEventDecoder.PostImage.Absent =>
            // Only a re-read already QUEUED is behind the barrier: one the debounce still holds is
            // queued after this delete, so it runs after it and reads whatever the key holds then.
            deletedId.foreach(id => pending.computeIfPresent(id, (_, entry) => if (entry.queued) null else entry))
            applyOffLoop(ChangeStreamLiveness.Movies, moviesDemand) {
              deletedId.foreach(movieChanges.dispatchDelete)
              ack()
            }
          // A document the codec refuses (counted and logged by the decoder). Nothing to apply,
          // and it must not END the cursor: decoded inside the driver, it did — and a cursor
          // resuming from a persisted token met it again on every reopen, dead for good. Its
          // demand is released, and it is acknowledged as applied — there is nothing to apply,
          // and left unacknowledged it would hold the cursor's position before it for good
          // ([[AppliedPrefix]] moves only past a contiguous run of acknowledged events).
          case ChangeEventDecoder.PostImage.Undecodable =>
            ack(); moviesDemand.applied()
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
    // A re-read the debounce still holds is queued now, so it drains with the rest and its events
    // are applied before the final positions are saved.
    if (debounce.isDefined) debouncer.shutdownNow()
    releaseHeld()
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
  /** The apply queue's capacity: the three cursors' demand windows, each event at most one task,
   *  plus room for the re-read retries and venue waits that owe no demand. */
  private[movies] def applyQueueCapacity(window: Int): Int = math.min(Int.MaxValue.toLong, 3L * window + 4096L).toInt

  /** One re-read film on its way to the listeners, with the fence mark taken before the read. */
  private final case class Delivery(film: StoredMovieRecord, mark: Long)

  /** Some of a film's venues on their way to the listeners, with the fence mark taken before the read. */
  private final case class VenueDelivery(venues: VenueSlots, mark: Long)

  /** How a film's venue apply went: applied, a listener not ready for it yet, or the film owed whole. */
  private enum VenueOutcome { case Applied, NotYet, Whole }

  /** How often a venue apply a listener was not ready for asks again, and for how long at most —
   *  the projector learns a booted worker's rows from the first corpus census pass, two minutes after
   *  boot and a few seconds long, then every 15 minutes; past this, the film is re-read whole. */
  private[movies] val VenueRetryMillis = 5000L
  private[movies] val VenueWaitMillis  = 30 * 60 * 1000L

  /** The most venues one apply reads alone; a burst across more re-reads the film whole. */
  private[movies] val MaxVenuesApplied = 16

  /** How many times an apply re-reads a film whose read failed before holding its cursor. */
  private[movies] val RereadAttempts      = 3
  private[movies] val RereadBackoffMillis = 100L
  /** When a film whose re-read failed is first read again, and the most its delay doubles to. */
  private[movies] val RereadRetryMillis    = 30_000L

  /** How long a film's re-read waits for the rest of its burst: each event pushes it to `quiet`
   *  after that event, never past `cap` after the burst's first. A venue scrape writes a film's rows
   *  seconds apart, a Flicks venue lands in day-chunks, and a wide release is written by one venue
   *  after another; every event re-read the WHOLE film (the widest US one: 3,167 screenings rows).
   *  Measured over ten minutes of each country's own writes (2026-09-30), 30 s / 2 min saved the
   *  re-read work US 60%, UK 43%, DE 23% — and PL ~1%, ES 0%, where a film's changes rarely cluster,
   *  so there it would be delay for nothing ([[forCountry]]). */
  final case class Debounce(quiet: scala.concurrent.duration.FiniteDuration, cap: scala.concurrent.duration.FiniteDuration)

  object Debounce {
    val Worker: Debounce = Debounce(scala.concurrent.duration.Duration(30, "seconds"), scala.concurrent.duration.Duration(2, "minutes"))
    /** The worker's debounce for `country`: on where its films' changes cluster, off elsewhere. */
    def forCountry(country: models.Country): Option[Debounce] =
      Option.when(Set[models.Country](models.Country.UnitedStates, models.Country.UnitedKingdom, models.Country.Germany)(country))(Worker)
  }
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
