package services.movies

import models.{Cinema, CinemaShowings, MovieRecord}

/**
 * Pure merge primitive — combines a `victim` row's cinema-side data onto a
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
 *   - Enrichment fields (tmdbId, imdbId, ratings, originalTitle, MC/RT/Filmweb
 *     URLs) come from the canonical. Both rows necessarily have the same
 *     tmdbId-derived data once they're identified as the same film; preferring
 *     the canonical avoids churn.
 *   - `cinemaScrapes` is unioned — every (cinema, raw title, raw year)
 *     provenance entry from both rows survives.
 *   - `cinemaShowings` is unioned per-cinema: when only one row has a slot
 *     for cinema C, that slot survives unchanged; when BOTH rows have a slot
 *     for the same cinema (the regression case — a cinema reports the film
 *     twice in the same tick under variant titles that resolve to the same
 *     tmdbId, e.g. "Diabeł ubiera się u Prady 2" + "Diabeł ubiera się u Prady
 *     2 ukraiński dubbing" from CinemaCity Poznań Plaza), the two slots'
 *     **showtimes are merged** (deduplicated, time-sorted) and the canonical
 *     slot's metadata fields (filmUrl, posterUrl, …) are kept.
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
      cinemaScrapes  = canonical.cinemaScrapes ++ victim.cinemaScrapes,
      cinemaShowings = mergeShowings(canonical.cinemaShowings, victim.cinemaShowings)
    )

  private def mergeShowings(
    canonical: Map[Cinema, CinemaShowings],
    victim:    Map[Cinema, CinemaShowings]
  ): Map[Cinema, CinemaShowings] =
    (canonical.keySet ++ victim.keySet).iterator.map { c =>
      val mergedShowings = (canonical.get(c), victim.get(c)) match {
        case (Some(a), Some(b)) =>
          a.copy(showtimes = (a.showtimes ++ b.showtimes).distinct.sortBy(_.dateTime))
        case (Some(a), None) => a
        case (None,    Some(b)) => b
        case (None,    None)    => throw new MatchError(c)   // unreachable: c ∈ keys union
      }
      c -> mergedShowings
    }.toMap
}
