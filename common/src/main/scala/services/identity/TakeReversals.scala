package services.identity

import services.movies.ListingKey

/**
 * Take changes that UNDO a recent one: a listing moved back to the film (or the no-film) it held before a change at most
 * [[window]] written projections earlier. Two projections in a row disagreeing on a listing is two rules undoing each
 * other — an OSCILLATION. UK Chichester's "The Ties That Bind Us" flipped between the 2008 and the 2025 film on every pod
 * start of 2026-10-13/14 (18 reversals in 41 h): the periodic projection's exact-top-hit own match took the 2008 film,
 * and the changed-scope projection's families-and-poster correction took the 2025 one back 17–20 s later. The change
 * counter saw two ordinary changes; this sees the reversal. A take lost one day and regained the next (DE "Der mit dem
 * Wolf tanzt (35-jähriges Jubiläum)", lost to a gap, regained by a fix) is a correction, never counted: its two
 * changes are many projections apart.
 *
 * Counts in projections, not time: each [[note]] is one written projection that moved a take. Remembers the [[capacity]] most recently
 * changed listings (a projection logs at most 2,000 changes); the least recent are forgotten first.
 */
final class TakeReversals(window: Int = 3, capacity: Int = 20000) {
  private final case class Held(film: Option[Int], projection: Long)
  private var projection = 0L
  private val heldBefore = new java.util.LinkedHashMap[ListingKey, Held](16, 0.75f, true) {
    override def removeEldestEntry(eldest: java.util.Map.Entry[ListingKey, Held]): Boolean = size() > capacity
  }

  /** Notes one written projection's `changes` (none when the caller notes only projections that moved one) and returns those that move a listing back to the film it held before a
   *  change at most [[window]] projections earlier. */
  def note(changes: Seq[TakeChange]): Seq[TakeChange] = synchronized {
    projection += 1
    changes.filter { change =>
      val reversal = Option(heldBefore.get(change.listing)).exists(h => h.film == change.after && projection - h.projection <= window)
      heldBefore.put(change.listing, Held(change.before, projection))
      reversal
    }
  }
}
