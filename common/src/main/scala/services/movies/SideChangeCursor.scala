package services.movies

import java.util.concurrent.atomic.AtomicReference

import scala.util.Try

import tools.ChangeStreamLiveness

/** One of [[MovieChangeStream]]'s side-collection cursors — `screenings` or `movie_slots` — with its
 *  own demand window and its own coalescing counter, ringing into the shared apply ([[MovieChangeApply]]).
 *  Read-split only: a showtime change writes only `screenings` and a venue's slot only `movie_slots`
 *  (movies stays put), so without these the projector would never see either. */
final class SideChangeCursor(
  collection:         String,
  metrics:            SideCollectionChangeMetrics,
  // The side collection whose applied rows this cursor watches; None (no split) opens nothing.
  rows:               Option[SlotKeyedRows],
  changeApply:        MovieChangeApply,
  liveness:           ChangeStreamLiveness,
  changeDemandWindow: Int
) {
  private val demand = new ChangeStreamDemand(changeDemandWindow)
  private val handle = new AtomicReference[Option[AutoCloseable]](None)

  // The re-read is a BLOCKING read, so it (and the fanout) run on `changeApply`, never
  // on this cursor's I/O event loop.
  def ensureOpen(): Unit = if (handle.get().isEmpty) handle.set(rows.flatMap(_.watchRowsApplied(applyChange, demand)))
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
   *  Anything that resurrects a closed repository would have to clear `MovieChangeApply`' `pending` first. */
  private def applyChange(rowId: String, applied: () => Unit): Unit = {
    liveness.delivered(collection)
    val filmId = SlotKeyed.filmIdOf(rowId)
    // A `screenings` row at a known venue can be applied from that venue alone — see `MovieChangeApply.applyVenues`.
    val venue  = Option.when(collection == ChangeStreamLiveness.Screenings && rowId != filmId)(SlotKeyed.slotKeyOf(rowId))
      .flatMap(models.Source.byWireKey).collect { case showing: models.CinemaShowing => showing.cinema }
    // Either way the event is acknowledged only once the film is fanned out — by the re-read it
    // opened or the one it rides — see `SideCollectionWatch` and `MovieChangeApply.applyReread`.
    if (!changeApply.request(filmId, collection, demand, applied, debounced = true, venue)) {
      metrics.recordCoalescedChange(); demand.applied()
    }
  }
}
