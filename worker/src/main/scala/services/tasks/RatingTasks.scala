package services.tasks

import services.cadence.RatingCadenceStore
import services.freshness.FreshnessKind
import services.movies.CacheKey

/** Dedup/freshness key + payload for a rating-refresh task. The key encodes the
 *  rating source and the film, so "refresh the IMDb rating of film F" can't be
 *  queued twice while one is active (the queue's unique index), and is shared
 *  across servers. */
object RatingTasks {
  val TitleKey = "title"
  val YearKey  = "year"

  /** Legacy title-based key. Kept only as the transition fallback in
   *  [[EnrichmentReaper]] (so deploying tmdbId-keyed freshness doesn't re-queue the
   *  whole resolved corpus once); new stamps use the tmdbId-keyed form below. */
  def dedupKey(kind: FreshnessKind, key: CacheKey): String =
    s"${kind.label}|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  /** Stable dedup/freshness key for a row, keyed on the IMMUTABLE tmdbId when the
   *  row is resolved. A film re-keyed under a new title (cross-language fold, a
   *  title-rule merge) keeps the same tmdbId, so its rating freshness survives the
   *  re-key instead of being orphaned and re-queued across all four sources — the
   *  corpus-wide rating surge that exhausted the worker's CPU credit after a merge
   *  wave. Falls back to the title key for a rare eligible row without a tmdbId. */
  def dedupKey(kind: FreshnessKind, key: CacheKey, tmdbId: Option[Int]): String =
    tmdbId.fold(dedupKey(kind, key))(services.attempts.RatingKeys.tmdbKey(kind, _))

  /** Record in `attempts`, under the rating task's `key`, what one refresh attempt `attempted` at `at`, taking
   *  `durationMs`, came to: the value it (re)set, none, or its failure. */
  def recordAttempt(attempts: services.attempts.EnrichmentAttemptStore, key: String, at: java.time.Instant, durationMs: Long,
                    attempted: scala.util.Try[Option[String]]): Unit =
    attempts.record(key, models.EnrichmentAttempt(at = at, durationMs = durationMs, outcome = attempted match {
      case scala.util.Success(Some(value)) => models.AttemptOutcome.Changed(value)
      case scala.util.Success(None)        => models.AttemptOutcome.Unchanged
      case scala.util.Failure(exception)   => models.AttemptOutcome.failed(exception)
    }))

  def payload(key: CacheKey): Map[String, String] =
    Map(TitleKey -> key.cleanTitle, YearKey -> key.year.map(_.toString).getOrElse(""))

  /** Freshness key under which a resolved row's TMDB-resolution TIME is stamped
   *  (see `MovieService.publishTmdbOutcome`). tmdbId-keyed so the rating handler
   *  can correlate it with its own tmdbId-keyed rating dedup key without touching
   *  the cache — both sides derive the same `tmdb|tmdb:<id>` string. */
  def tmdbResolvedAtKey(tmdbId: Int): String = s"${FreshnessKind.TmdbResolve.label}|tmdb:$tmdbId"

  /** The `tmdbResolvedAtKey` matching a rating task's dedup key — `Some` only when
   *  the rating task is tmdbId-keyed (`<site>|tmdb:<id>`). A legacy title-keyed
   *  rating row has no tmdbId to correlate on, so its delay isn't measured. */
  def tmdbResolvedAtKeyFor(ratingDedupKey: String): Option[String] =
    ratingDedupKey.split('|') match {
      case Array(_, tmdbPart) if tmdbPart.startsWith("tmdb:") => Some(s"${FreshnessKind.TmdbResolve.label}|$tmdbPart")
      case _                                                  => None
    }
}

/** Builds the cadence recorder the full-corpus `*Ratings.refreshAll` walks use to
 *  feed an operator bulk refresh's displayed-value changes into the SAME
 *  tmdbId-keyed adaptive cadence the per-row [[RatingHandler]] feeds. Lives here
 *  (not in the `modules` composition root) because the root can't name the
 *  `private[services]` [[services.movies.CacheKey]] — it wires the recorder by
 *  value instead. */
object BulkCadenceRecorder {
  def apply(cadence: RatingCadenceStore, kind: FreshnessKind, clock: java.time.Clock): (CacheKey, Option[Int], Option[String]) => Unit =
    (key, tmdbId, value) => cadence.record(RatingTasks.dedupKey(kind, key, tmdbId), value, clock.instant())
}

/** Records how long after a film's TMDB resolution each rating site FIRST tried
 *  to fetch its rating — the latency the [[EnrichmentReaper]]'s first pass now
 *  owns, since ratings are no longer enqueued the instant a film resolves. A
 *  narrow seam (like `ReadModelProjectionMetrics`) so [[RatingHandler]] doesn't depend on the
 *  Prometheus sink; `WorkerTaskMetrics` implements it, tests pass a spy or NoOp. */
trait RatingLatencyMetrics {
  /** `site` is a [[FreshnessKind]] label (imdb/fw/rt/mc); `seconds` is clamped ≥ 0. */
  def recordFirstRatingDelay(site: String, seconds: Double): Unit
}

object RatingLatencyMetrics {
  val NoOp: RatingLatencyMetrics = (_, _) => ()
}
