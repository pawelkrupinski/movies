package services.tasks

import models.{Country, MovieRecord}
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
 *
 * A source also carries WHICH COUNTRIES it applies in ([[RatingSource.appliesIn]]).
 * Filmweb is a Poland-only source: its handler is wired only when
 * `country.filmwebEnabled` (see `WorkerWiring`), so a non-Filmweb country that
 * enqueued a `FilmwebRating` task would have no handler to work it — the task
 * re-releases to `waiting` forever (never `Done`), piling up a handler-less backlog
 * whose "oldest waiting age" climbs without bound. So both the enqueuer and the
 * census consult [[forCountry]], which drops Filmweb outside Filmweb-enabled
 * countries — keeping the enqueue side symmetric with the handler side.
 */
object RatingSources {
  case class RatingSource(
    taskType: TaskType,
    kind:     FreshnessKind,
    eligible: MovieRecord => Boolean,
    // Which countries this source applies in at all — an axis ABOVE per-row
    // `eligible`. Global sources (IMDb/RT/MC) apply everywhere; Filmweb only where
    // `country.filmwebEnabled` (its handler is wired only there).
    appliesIn: Country => Boolean = _ => true
  )

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
    RatingSource(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, r => r.tmdbId.isDefined || r.filmwebUrl.isDefined, _.filmwebEnabled),
    RatingSource(TaskType.RtRating,      FreshnessKind.RtRating,      _.tmdbId.isDefined),
    RatingSource(TaskType.McRating,      FreshnessKind.McRating,      _.tmdbId.isDefined)
  )

  /** The rating sources that apply in `country` — the global ones plus the
   *  country-gated ones this country enables (Filmweb only where
   *  `filmwebEnabled`). Both [[RatingEnqueuer]] and the census walk THIS, never
   *  [[all]], so neither queues nor counts a source whose handler this country
   *  never wires. */
  def forCountry(country: Country): Seq[RatingSource] = all.filter(_.appliesIn(country))
}
