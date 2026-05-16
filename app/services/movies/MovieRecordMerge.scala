package services.movies

import models.MovieRecord

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
 *   - `cinemaShowings` is unioned with **right-bias** (victim wins on key
 *     collisions). The victim is typically the fresher write (the row that
 *     triggered the gate); its slot data represents the latest scrape tick.
 */
object MovieRecordMerge {

  def union(canonical: MovieRecord, victim: MovieRecord): MovieRecord =
    canonical.copy(
      cinemaScrapes  = canonical.cinemaScrapes  ++ victim.cinemaScrapes,
      cinemaShowings = canonical.cinemaShowings ++ victim.cinemaShowings
    )
}
