package services.tasks

import models.MovieRecord
import services.freshness.FreshnessKind

/**
 * The four rating sources and the single thing that decides whether a row can be
 * enriched by each: its eligibility predicate (IMDb needs an `imdbId`; the others
 * a resolved `tmdbId`). Owned in one place so every consumer agrees on it:
 *
 *   - [[RatingEnqueuer]] enqueues a `(row, source)` task only when the source is
 *     eligible; and
 *   - the metrics census ([[services.metrics.RatingRunCensus]]) counts a resolved
 *     row as "this rating never ran" only for sources the row is eligible for.
 *
 * If the two disagreed, the census would flag a backlog the enqueuer never acts
 * on (or miss one it does) — so the eligibility rule lives here, not copied into
 * each.
 */
object RatingSources {
  case class RatingSource(taskType: TaskType, kind: FreshnessKind, eligible: MovieRecord => Boolean)

  // Order IS the enqueue priority: [[RatingEnqueuer]] walks this sequence and, under
  // the reaper's per-tick cap, the earlier sources win the slots. Filmweb sits
  // RIGHT AFTER IMDb — a peer, not a trailing rating — because it's now a full
  // enrichment source (originalTitle / director / year / synopsis / genres) whose
  // fields DRIVE re-resolution of the others, so it should land early rather than
  // after RT/MC. Filmweb is tmdbId-gated (like RT/MC), so it still can't fire before
  // TMDB resolves.
  val all: Seq[RatingSource] = Seq(
    RatingSource(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined),
    RatingSource(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, _.tmdbId.isDefined),
    RatingSource(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined),
    RatingSource(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined)
  )
}
