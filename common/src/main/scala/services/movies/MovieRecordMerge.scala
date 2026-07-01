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
 *     MC/RT/Filmweb URLs) prefer the canonical, falling back to the victim
 *     when the canonical lacks them. Both rows necessarily have the same
 *     tmdbId-derived data once they're identified as the same film, so
 *     preferring the canonical avoids churn — but when only the victim was
 *     enriched (a freshly-resolved cross-language duplicate became the
 *     canonical base), the fallback keeps its ratings instead of dropping
 *     them.
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
    canonical.copy(
      // Enrichment-side single-source fields prefer the canonical, but fall back
      // to the victim when the canonical lacks them. The two rows are the SAME
      // film (same tmdbId), so their ratings describe one film and only ever
      // converge — taking the victim's when the canonical's is empty can't be
      // wrong, and it stops the merge from DROPPING ratings. This matters once a
      // cluster can hold two RESOLVED rows (a cross-language duplicate folded by
      // shared tmdbId, FilmCanonicalizer.groupByFilm): the union base is the
      // lowest-`canonicalRank` row, which may be a freshly-resolved translation
      // that has a tmdbId but no ratings yet. A canonical-only copy then nulled
      // the rated sibling's scores until the next rating refresh re-fetched them —
      // the "ratings keep disappearing and coming back" flap. With the fallback,
      // `canonical` is order-independent for enrichment, as its callers assume.
      imdbId            = canonical.imdbId.orElse(victim.imdbId),
      imdbRating        = canonical.imdbRating.orElse(victim.imdbRating),
      metascore         = canonical.metascore.orElse(victim.metascore),
      filmwebUrl        = canonical.filmwebUrl.orElse(victim.filmwebUrl),
      filmwebRating     = canonical.filmwebRating.orElse(victim.filmwebRating),
      rottenTomatoes    = canonical.rottenTomatoes.orElse(victim.rottenTomatoes),
      tmdbId            = canonical.tmdbId.orElse(victim.tmdbId),
      metacriticUrl     = canonical.metacriticUrl.orElse(victim.metacriticUrl),
      rottenTomatoesUrl = canonical.rottenTomatoesUrl.orElse(victim.rottenTomatoesUrl),
      searchTitle       = canonical.searchTitle.orElse(victim.searchTitle),
      data              = mergeData(canonical.data, victim.data),
      retainedSynopses  = mergeRetainedSynopses(canonical.retainedSynopses, victim.retainedSynopses)
    )

  /** Combine two retained-synopsis maps, keeping the LONGEST synopsis seen per
   *  source (the canonical's on an exact-length tie). Shared by `union` and by
   *  `MovieCache`'s slot-prune capture so both agree on "best-seen per source"
   *  — the rule that keeps the displayed (longest-wins) synopsis sticky once a
   *  cinema stops listing a film. Commutative on content, so it's
   *  scrape/merge-order independent. */
  def mergeRetainedSynopses(
    a: Map[Source, String],
    b: Map[Source, String]
  ): Map[Source, String] =
    (a.keySet ++ b.keySet).iterator.map { s =>
      s -> (a.get(s).iterator ++ b.get(s).iterator).maxBy(_.length)
    }.toMap

  /** Fold a set of rows of the SAME film into one. The enriched row (the first
   *  carrying a `tmdbId`) is the canonical, so its single-source enrichment
   *  fields survive; every other row's per-source `data` is unioned onto it.
   *  Used wherever several stored rows resolve to one merge key at once, e.g. the
   *  cache's `rehydrate` (a late merge-key rule makes two documents collide).
   *  `records` must be non-empty. */
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
    // Total order (not just dateTime): `groupBy(...).values` iteration order is
    // non-deterministic, so same-time showings could otherwise land in a
    // scrape/merge-order-dependent sequence — a re-fold would then re-write the
    // same content in a different order (the churn `sortShowtimes` exists to kill).
    sortShowtimes(showtimes.groupBy(identity).values.map(_.minBy(rank)).toSeq)
  }

  /** Canonical TOTAL order for a cinema slot's showtimes. Sorting at the
   *  ingestion boundary means a re-scrape that returns the same showings in a
   *  different order stores a byte-identical slot, so `MovieCache`'s write-through
   *  equality guard (`updated == before`) skips the write — and with it the
   *  change-stream event and reprojection that a reorder-only "change" would
   *  otherwise trigger. Total across every field (unlike [[dedupShowtimes]]'s
   *  `dateTime`-only sort): `dateTime` first (its ISO string sorts
   *  chronologically), then room/format/bookingUrl, so equal multisets of
   *  showings always collapse to the same sequence. */
  def sortShowtimes(showtimes: Seq[Showtime]): Seq[Showtime] =
    showtimes.sortBy(s => (s.dateTime.toString, s.room.getOrElse(""), s.format.mkString(","), s.bookingUrl.getOrElse("")))

  /** Merge a fresh cinema scrape's showtimes over the prior stored ones, NEVER
   *  deleting a screening that is already in the PAST. A re-scrape is authoritative
   *  for the future (adds/removes upcoming showings) but a past showing it no longer
   *  lists is kept, not dropped: dropping it is pure churn — it no longer displays
   *  (the web filters past showtimes at render) and nothing reaps it, so re-writing
   *  the slot just to remove an aged-out time would fire a change event + reprojection
   *  for no visible effect. With the past retained, an aging-only re-scrape yields a
   *  slot equal to what's stored, so `MovieCache`'s `updated == before` guard skips
   *  the write. "Past" is STRICT (`dateTime <= now`, no grace); result is canonical
   *  order. A genuine future change still replaces the slot (and drops stale past
   *  the fresh scrape didn't re-list, since it rebuilds from `fresh`). */
  def retainPastShowtimes(prior: Seq[Showtime], fresh: Seq[Showtime], now: java.time.LocalDateTime): Seq[Showtime] = {
    val freshSet = fresh.toSet
    val keptPast = prior.filter(s => !s.dateTime.isAfter(now) && !freshSet.contains(s))
    sortShowtimes(fresh ++ keptPast)
  }
}
