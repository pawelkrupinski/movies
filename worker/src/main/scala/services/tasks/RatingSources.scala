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

  val all: Seq[RatingSource] = Seq(
    RatingSource(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined),
    RatingSource(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined),
    RatingSource(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined),
    RatingSource(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, _.tmdbId.isDefined)
  )
}
