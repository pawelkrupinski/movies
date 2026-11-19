package services.movies

import models.StoredMovieRecord

import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.bson.BsonDocument
import org.mongodb.scala.Observer

/**
 * The `movies` change-stream SUBSCRIPTION: one shared cursor, decoded once and fanned
 * out to every listener, applied off the driver's I/O loop, resumed from a persisted
 * token and reopened after a terminal error. [[MongoMovieChangeFeed]] builds one over a Mongo store and
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
 * ([[MongoMovieChangeFeed]]); a spec pushes events by hand.
 * `reread` re-reads a film by id, stitched (showtimes and slots live in their own
 * collections) — see [[FilmRereads]], the apply side every cursor rings into.
 *
 * Its parts: the `movies` cursor ([[MoviesChangeCursor]]), the two side cursors
 * ([[SideChangeCursor]]), and the apply side ([[FilmRereads]]): this class owns the
 * listeners, starts the cursors on the first and stops them after the last, and closes it all.
 */
final class MovieChangeStream(
  source:              MovieChangeStream.Source,
  // The side collections whose applied rows the two side cursors watch (`screenings`, `movie_slots`); None, unsplit.
  screenings:          Option[SlotKeyedRows],
  slots:               Option[SlotKeyedRows],
  reread:              String => tools.ReadOutcome[StoredMovieRecord],
  // Marked before every `reread`, so a consumer that also writes films (the cache) can tell a
  // read taken before its own write from one taken after — see [[FilmWriteFence]].
  fence:               FilmWriteFence = new FilmWriteFence(),
  resumeToken:         ChangeStreamResumeToken,
  changeStreamMetrics: ChangeStreamMetrics,
  screeningsMetrics:   SideCollectionChangeMetrics,
  slotsMetrics:        SideCollectionChangeMetrics,
  changeDemandWindow:  Int,
  // The repository's clock: the debounce runs on it.
  clock:               java.time.Clock,
  /** When each cursor last DELIVERED an event — the liveness signal an open-but-silent
   *  cursor has no other way of giving. Stamped on the driver's `onNext`, before the apply
   *  and before coalescing, so it says what the CURSOR did, not what the apply thread got
   *  round to. See [[ChangeStreamLiveness]]. Built by the store over the sequence it stamps
   *  `updatedAt` from, so a catch-up floor and a row's stamp are strictly ordered
   *  ([[ChangeStreamLiveness.now]]). */
  val liveness:        ChangeStreamLiveness,
  // The repository's monotonic clock: the post-image decoder's log throttle times on it.
  stopwatch:           tools.Stopwatch,
  // How long after a failed re-read the film is read again — see `applyReread`. Doubled per
  // failure up to `RereadRetryMaxMillis`; a spec shortens it, production never passes it.
  rereadRetryMillis:   Long = FilmRereads.RereadRetryMillis,
  // Where a `movies` post-image the codec refuses is counted — see [[ChangeEventDecoder]].
  decodeFailures:      services.movies.DecodeFailureMetrics = services.movies.DecodeFailureMetrics.noop,
  // How long a film's re-read waits for the rest of its burst — see [[MovieChangeStream.Debounce]].
  // None queues each film's re-read at once, as every repository but the worker's does.
  debounce:            Option[MovieChangeStream.Debounce] = None,
  // Reads a film's slots at some venues as a whole-film read stitches them — what lets a change
  // confined to a few venues' showtimes skip the whole-film re-read ([[applyVenues]]). None (every
  // repository but the worker's) re-reads every change whole.
  readVenues:          Option[(String, Set[models.Cinema]) => Option[VenueSlots]] = None,
  // Waits out each film's debounce (built only when `debounce` is set) — injected so a spec can
  // step a debounce boundary on a hand-moved `clock` instead of sleeping across it.
  debounceScheduler:   () => java.util.concurrent.ScheduledExecutorService = FilmRereads.DebounceScheduler
) extends AutoCloseable {

  private val movieChanges = new ChangeStreamFanout[FilmRereads.Delivery, FilmRereads.VenueDelivery]("MovieRepository")
  private val changeLock   = new AnyRef

  // Every event of the three cursors becomes a re-read of its film here, coalesced and applied on one thread.
  private val rereads = new FilmRereads(movieChanges, reread, fence, liveness, changeStreamMetrics, changeDemandWindow,
    clock, rereadRetryMillis, debounce, readVenues, debounceScheduler)

  /** Change events handed to the apply thread but not yet applied — see [[FilmRereads.applyBacklog]]. */
  def applyBacklog: Int = rereads.applyBacklog

  /** Block until every apply queued before this call has run — see [[FilmRereads.awaitQueuedApplies]]. */
  private[movies] def awaitQueuedApplies(timeoutSeconds: Long = 10): Boolean = rereads.awaitQueuedApplies(timeoutSeconds)

  /** Re-reads the debounce is holding for the rest of their film's burst. */
  def held: Int = rereads.held

  /** Queue every re-read the debounce holds, now — see [[FilmRereads.releaseHeld]]. */
  def releaseHeld(): Unit = rereads.releaseHeld()

  // Read-split only: a showtime change writes only `screenings` and a venue's slot only
  // `movie_slots` (movies stays put), so without these the projector would never see either.
  private val sideCursors: Seq[SideChangeCursor] = Seq(
    new SideChangeCursor(ChangeStreamLiveness.Screenings, screeningsMetrics, screenings, rereads, liveness, changeDemandWindow),
    new SideChangeCursor(ChangeStreamLiveness.Slots, slotsMetrics, slots, rereads, liveness, changeDemandWindow))

  // Set while an open deferred on an unreadable resume position waits on `changeReopen`. A
  // registration meanwhile leaves the open to that reopen rather than re-reading the position:
  // every read counts against [[ChangeStreamResumeToken.MaxDeferredOpens]], which the backoff is
  // meant to space out — the boot's back-to-back registrations spent two of them in milliseconds.
  // Declared before `changeReopen`, whose reopen reads it.
  private val openDeferred = new java.util.concurrent.atomic.AtomicBoolean(false)

  // A change stream's onError is TERMINAL — nothing brings the cursor back on its own, and
  // `ensureWatching` only runs on REGISTRATION, which the worker does twice at boot and never
  // again. Without this driver one terminal error killed the worker's stream until the process
  // restarted (see [[ChangeStreamReopen]] for the outage that proved it). Skips the reopen once
  // the last listener has detached, so an idle repository stays idle.
  // `@unchecked` for -Wsafe-init: the reopen reaches `changeReopen` itself (through `ensureWatching`), a cycle the
  // checker cannot see is safe; it runs only from the scheduler `failed()` arms, never during construction.
  private val changeReopen = ChangeStreamReopen.onDaemonScheduler("MovieRepository",
    (() => { openDeferred.set(false); if (!movieChanges.isEmpty) ensureWatching() }): @unchecked)

  private val moviesCursor = new MoviesChangeCursor((resumeFrom, observer) => source.open(resumeFrom, observer), resumeToken,
    changeReopen, rereads, movieChanges, liveness, changeStreamMetrics, changeDemandWindow, decodeFailures, stopwatch)

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
   *  fall back to their periodic backstop (cache rehydrate / sweeper reconcile)
   *  meanwhile. */
  private def ensureWatching(): Unit = changeLock.synchronized {
    if (!moviesCursor.isWatching && !openDeferred.get()) {
      // Resume from the last persisted token if we have one (a restart / prior terminal
      // error) so events missed while down are replayed; else open at "now".
      resumeToken.openFrom() match {
        case ChangeStreamResumeToken.Position.Deferred => openDeferred.set(true); changeReopen.failed()
        case ChangeStreamResumeToken.Position.At(resumeFrom) => open(resumeFrom)
      }
    }
  }

  /** Open the shared cursor after `resumeFrom` (at "now" when `None`). Called under `changeLock`. */
  private def open(resumeFrom: Option[BsonDocument]): Unit = {
    moviesCursor.open(resumeFrom)
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
      moviesCursor.unsubscribe()
      sideCursors.foreach(_.close())
    }
  }

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatching: Boolean = moviesCursor.isWatching

  /** Tear the subscription down for good: no more reopens, the final resume position
   *  persisted synchronously, every demand window closed, the apply thread stopped. */
  override def close(): Unit = changeLock.synchronized {
    changeReopen.close()
    rereads.stopRetries()
    moviesCursor.unsubscribe()
    // A re-read the debounce still holds is queued now, so it drains with the rest and its events
    // are applied before the final positions are saved; and the applies already queued FINISH
    // (bounded) before them: saved first, a position misses them ([[FilmRereads.drainForClose]]).
    rereads.drainForClose()
    resumeToken.save(force = true)
    // Each side handle stops its own reopen driver too — left open, a Mongo side cursor whose
    // client is closed under it would fail and reschedule its reopen for the life of the JVM.
    // Closing one persists its final position, so it comes after the drain as well.
    sideCursors.foreach(_.close())
  }
}

object MovieChangeStream {

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

  /** Where the `movies` events come from. `resumeAfter` is the persisted position to
   *  reopen from (None opens at "now"); the observer receives every delivered change, its
   *  post-image UNDECODED — the stream decodes it, so a document the codec refuses cannot
   *  end the cursor (see [[ChangeEventDecoder]]). */
  trait Source {
    def open(resumeAfter: Option[BsonDocument], observer: Observer[ChangeStreamDocument[BsonDocument]]): Unit
  }
}
