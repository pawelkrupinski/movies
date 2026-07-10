package services.tasks

import models.MovieRecord
import services.freshness.FreshnessKind

/**
 * The four rating sources and the single thing that decides whether a row can be
 * enriched by each: its eligibility predicate (IMDb needs an `imdbId`; RT/Metacritic
 * a resolved `tmdbId`; Filmweb a `tmdbId` OR a Filmweb URL). Owned in one place so
 * every consumer agrees on it:
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
  // after RT/MC. Unlike RT/MC, Filmweb is NOT tmdbId-gated: a tmdbId-less row that
  // carries a Filmweb URL is eligible too, because it can RESOLVE its tmdbId via
  // Filmweb→Wikidata (P5032 → P4947; see `MovieService`) and its rating refresh
  // reads that KNOWN URL directly — no fuzzy match, so no bogus rating. A tmdbId-less
  // row with no Filmweb URL stays ineligible (nothing to refresh from, nothing to
  // resolve through), so the event/opera/NT-Live long tail isn't enqueued.
  val all: Seq[RatingSource] = Seq(
    RatingSource(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    _.imdbId.isDefined),
    RatingSource(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, r => r.tmdbId.isDefined || r.filmwebUrl.isDefined),
    RatingSource(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined),
    RatingSource(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined)
  )
}
