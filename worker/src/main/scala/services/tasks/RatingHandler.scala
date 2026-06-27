package services.tasks

import services.cadence.RatingCadenceStore
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.CacheKey

import java.time.Clock

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
    tmdbId.fold(dedupKey(kind, key))(id => s"${kind.label}|tmdb:$id")

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

/** Records how long after a film's TMDB resolution each rating site FIRST tried
 *  to fetch its rating — the latency the [[EnrichmentReaper]]'s first pass now
 *  owns, since ratings are no longer enqueued the instant a film resolves. A
 *  narrow seam (like `MergeMetrics`) so [[RatingHandler]] doesn't depend on the
 *  Prometheus sink; `WorkerTaskMetrics` implements it, tests pass a spy or NoOp. */
trait RatingLatencyMetrics {
  /** `site` is a [[FreshnessKind]] label (imdb/fw/rt/mc); `seconds` is clamped ≥ 0. */
  def recordFirstRatingDelay(site: String, seconds: Double): Unit
}

object RatingLatencyMetrics {
  val NoOp: RatingLatencyMetrics = (_, _) => ()
}

/**
 * Handles one rating-refresh task by delegating to the existing per-row refresh
 * of a `*Ratings` class, re-gated at pickup by the SAME [[DueWindow]] the
 * [[EnrichmentReaper]] enqueues on. One generic handler covers all four sources —
 * they differ only in their `taskType`, `kind`, and which `refreshOneSync` they
 * call.
 *
 * The re-gate must use the reaper's due definition, not a separate rolling TTL:
 * when it didn't, the reaper enqueued a row on its phase boundary while the
 * handler skipped it as still-fresh, so the same row churned the queue every tick
 * without ever refreshing (see [[DueWindow]]). A row is marked fresh after
 * the refresh attempt (success or no-op), so it isn't due again until its next
 * window.
 */
class RatingHandler(
  override val taskType: TaskType,
  kind:                  FreshnessKind,
  freshness:             FreshnessStore,
  dueWindow:             DueWindow,
  cadence:               RatingCadenceStore,
  refresh:               (String, Option[Int]) => Option[String],
  clock:                 Clock = Clock.systemUTC(),
  metrics:               RatingLatencyMetrics = RatingLatencyMetrics.NoOp
) extends TaskHandler {
  import HandlerOutcome._

  override def handle(task: Task): HandlerOutcome = {
    val key      = task.dedupKey
    val lastRated = freshness.lastFetchedAt(key)
    if (!dueWindow.isDue(key, lastRated, clock.instant())) Skipped
    else {
      // No prior rating stamp ⇒ this is the FIRST attempt for this (row, site):
      // record how long after TMDB resolution it took, so the EnrichmentReaper's
      // first-pass latency is observable per site (the smoothing trade-off).
      if (lastRated.isEmpty) recordFirstAttemptDelay(key)
      val title = task.payload.getOrElse(RatingTasks.TitleKey, "")
      val year  = task.payload.get(RatingTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
      val now    = clock.instant()
      val reported = refresh(title, year)   // Some(displayValue) when the row's badge was (re)set this refresh
      freshness.markFresh(key, kind, now)
      // Feed the adaptive cadence: a refresh that moved the DISPLAYED value to a
      // new badge snaps the interval back to the base; a no-change refresh — or one
      // that re-reported the same value after a re-key — lets it back off (up to 4
      // days). The cadence dedups same-value reports, see RatingCadence.record.
      cadence.record(key, reported, now)
      Done
    }
  }

  private def recordFirstAttemptDelay(ratingKey: String): Unit =
    for {
      resolvedKey <- RatingTasks.tmdbResolvedAtKeyFor(ratingKey)
      resolvedAt  <- freshness.lastFetchedAt(resolvedKey)
    } {
      val seconds = (clock.instant().toEpochMilli - resolvedAt.toEpochMilli).toDouble / 1000.0
      metrics.recordFirstRatingDelay(kind.label, math.max(0.0, seconds))
    }
}
