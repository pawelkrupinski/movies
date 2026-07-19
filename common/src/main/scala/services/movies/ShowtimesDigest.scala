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

  /** Digest of a slot with no showtimes — shared so an absent slot and a present-but-empty
   *  slot compare equal. */
  val EmptyDigest: Int = digest(Seq.empty)

  /** A slot's effective showtime digest: from resident showtimes when present, else the
   *  STORED `showtimesDigest` (slot was stripped for the cache). Lets the guard +
   *  screenings-diff compare a stripped record against a freshly-scraped full one. */
  def slotDigest(sd: SourceData): Int =
    if (sd.showtimes.nonEmpty) digest(sd.showtimes) else sd.showtimesDigest.getOrElse(EmptyDigest)

  /** Strip a record for cache residency: drop each slot's showtime LIST (they live in Mongo
   *  `screenings`), keep only its digest. Idempotent + authoritative. */
  def stripForCache(record: MovieRecord): MovieRecord =
    record.copy(data = record.data.view.mapValues { sd =>
      if (sd.showtimes.isEmpty && sd.showtimesDigest.isDefined) sd
      else sd.copy(showtimes = Nil, showtimesDigest = Some(slotDigest(sd)))
    }.toMap)

  /** Whole-record content digest, for a VIEWER that has to decide "did anything I
   *  render change?" from two renders of the same film (the /debug table's
   *  change-stream no-op guard). Combines the record's own hashCode with the
   *  per-slot showtime digest — the same two components [[leanEqual]] compares —
   *  because `MovieRecord.hashCode` alone is showtime-BLIND by design (see
   *  `SourceData.equals`), so a showtime-only change would otherwise hash equal.
   *  Order-independent: `data` is a Map. */
  def recordDigest(record: MovieRecord): Int =
    (record, record.data.view.mapValues(slotDigest).toMap).hashCode

  /** The write-guard: equal non-showtime fields (via SourceData's showtime-agnostic `==`)
   *  AND equal per-slot showtime digest. Unlike `==`, this DOES detect showtime changes —
   *  that's its whole job. Works whether either side is stripped or full. */
  def leanEqual(a: MovieRecord, b: MovieRecord): Boolean =
    a.copy(data = Map.empty) == b.copy(data = Map.empty) &&
    a.data.keySet == b.data.keySet &&
    a.data.forall { case (source, sdA) =>
      b.data.get(source).exists(sdB => sdA == sdB && slotDigest(sdA) == slotDigest(sdB))
    }
}
