package services.movies

import models.{MovieRecord, Showtime, SourceData}

/** Groundwork for the index-only cache migration (Phase 1).
 *
 *  Today the write-through no-op guard compares a freshly-built `MovieRecord`
 *  against the resident one with a full `==`, which needs the showtime LISTS in
 *  memory. The end state holds only a per-slot digest of the showtimes (the lists
 *  live in Mongo `screenings`). This object provides the digest and a "lean
 *  equality" that decides skip/write from (all non-showtime fields) + (showtime
 *  digests) — exactly what the future guard will use.
 *
 *  Phase 1 wires [[leanEqual]] in SHADOW next to the real `==` and logs any
 *  disagreement. The only dangerous disagreement is a FALSE SKIP: `leanEqual`
 *  true while `==` is false (a real showtime change hashed to the same digest →
 *  a write wrongly suppressed → stale read model). Days of zero false-skips gates
 *  Phase 2 (flip the guard) and Phase 3 (drop the resident lists). A 32-bit digest
 *  matches the pattern `ReadModelProjector` already trusts for skip-unchanged. */
object ShowtimesDigest {

  /** Stable, order-SENSITIVE content hash of a slot's showtimes — mirrors what the
   *  current `Seq[Showtime] ==` compares (slots are stored in canonical order by
   *  `sortShowtimes`, so equal content ⇒ equal digest). */
  def digest(showtimes: Seq[Showtime]): Int =
    showtimes.iterator.map(s => (s.dateTime, s.room, s.format, s.bookingUrl)).toVector.hashCode

  /** True iff `a` and `b` are equal for write-guard purposes when showtimes are
   *  represented only by [[digest]]: every non-showtime field equal AND every slot's
   *  showtime digest equal. Equivalent to `a == b` unless a digest collision hides a
   *  real showtime difference — precisely the risk Phase 1's shadow measures. */
  def leanEqual(a: MovieRecord, b: MovieRecord): Boolean =
    a.copy(data = Map.empty) == b.copy(data = Map.empty) &&    // all top-level non-`data` fields
    a.data.keySet == b.data.keySet &&
    a.data.forall { case (source, sdA) =>
      b.data.get(source).exists(sdB => slotLeanEqual(sdA, sdB))
    }

  private def slotLeanEqual(a: SourceData, b: SourceData): Boolean =
    a.copy(showtimes = Nil) == b.copy(showtimes = Nil) &&     // all non-showtime slot fields
    digest(a.showtimes) == digest(b.showtimes)
}
