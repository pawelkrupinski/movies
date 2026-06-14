package services.movies

import models.{MovieRecord, Showtime, Source, SourceData}

/**
 * Pure merge primitive — combines a `victim` row's per-source data onto a
 * `canonical` row. Used by:
 *   - `CaffeineMovieCache.put`'s tmdbId gate (runtime, prevents fresh
 *     duplicates from being persisted).
 *   - The same-tmdbId backfill script (one-shot, collapses legacy
 *     duplicates).
 *
 * Single source of truth so the runtime gate and the backfill agree on what
 * "merge two rows of the same film" means.
 *
 * Rule of thumb:
 *   - Enrichment-side single-source fields (tmdbId, imdbId, ratings,
 *     MC/RT/Filmweb URLs) come from the canonical. Both rows necessarily
 *     have the same tmdbId-derived data once they're identified as the
 *     same film; preferring the canonical avoids churn.
 *   - `data` is unioned per-source: when only one row has a slot for source
 *     S, that slot survives unchanged; when BOTH rows have a slot for the
 *     same source (the regression case — a cinema reports the film twice
 *     in the same tick under variant titles that resolve to the same
 *     tmdbId, e.g. "Diabeł ubiera się u Prady 2" + "Diabeł ubiera się u
 *     Prady 2 ukraiński dubbing" from CinemaCity Poznań Plaza), the two
 *     slots' **showtimes are merged** (deduplicated, time-sorted) and the
 *     canonical slot's metadata fields (filmUrl, posterUrl, …) are kept.
 *
 *     The previous right-biased `++` lost data: the second-resolved variant
 *     (often the dub, with one-off late screenings) overwrote the first
 *     variant's full schedule, so the user's main page listed Prada with
 *     only the dub's handful of showings for that cinema. The row was
 *     "still there" but its slot had silently regressed.
 */
object MovieRecordMerge {

  def union(canonical: MovieRecord, victim: MovieRecord): MovieRecord =
    canonical.copy(data = mergeData(canonical.data, victim.data))

  /** Fold a set of rows of the SAME film into one. The enriched row (the first
   *  carrying a `tmdbId`) is the canonical, so its single-source enrichment
   *  fields survive; every other row's per-source `data` is unioned onto it.
   *  Used wherever several stored rows resolve to one merge key at once — the
   *  cache's `rehydrate` (a late merge-key rule makes two documents collide) and the
   *  rule-change `NormalizationRebuilder` — so both agree on the canonical pick
   *  and the union order. `records` must be non-empty. */
  def unionAll(records: Seq[MovieRecord]): MovieRecord = {
    require(records.nonEmpty, "MovieRecordMerge.unionAll: no records")
    val canonical = records.find(_.tmdbId.isDefined).getOrElse(records.head)
    records.filterNot(_ eq canonical).foldLeft(canonical)(union)
  }

  private def mergeData(
    canonical: Map[Source, SourceData],
    victim:    Map[Source, SourceData]
  ): Map[Source, SourceData] =
    (canonical.keySet ++ victim.keySet).iterator.map { src =>
      val mergedSlot = (canonical.get(src), victim.get(src)) match {
        case (Some(a), Some(b)) =>
          a.copy(showtimes = dedupShowtimes(a.showtimes ++ b.showtimes))
        case (Some(a), None)    => a
        case (None,    Some(b)) => b
        case (None,    None)    => throw new MatchError(src)   // unreachable: src ∈ keys union
      }
      src -> mergedSlot
    }.toMap

  /** Collapse `showtimes` to one entry per *physical* screening, time-sorted.
   *
   *  A screening's identity is `(dateTime, room, format)` — NOT the whole
   *  `Showtime`. `bookingUrl` is a per-source ticket link, not part of what
   *  makes two sessions the same; a plain `.distinct` keyed on the full case
   *  class kept the same screening twice whenever two sources (e.g. Kino Nowe
   *  Horyzonty surfacing one film under two `op.s?id=` event pages) reported
   *  it with different booking links. That produced a phantom duplicate whose
   *  surviving order flipped with scrape/merge order — the "screenings error".
   *
   *  Among entries sharing an identity, the representative is chosen by a pure
   *  function of the data (the one carrying a `bookingUrl`, then the lowest URL
   *  string) so the kept link — and therefore the rendered slot — is identical
   *  whatever order the sources arrived in. */
  def dedupShowtimes(showtimes: Seq[Showtime]): Seq[Showtime] = {
    def identity(s: Showtime): (java.time.LocalDateTime, Option[String], List[String]) =
      (s.dateTime, s.room, s.format)
    def rank(s: Showtime): (Boolean, String) =
      (s.bookingUrl.isEmpty, s.bookingUrl.getOrElse(""))
    showtimes
      .groupBy(identity)
      .values
      .map(_.minBy(rank))
      .toSeq
      .sortBy(_.dateTime)
  }
}
