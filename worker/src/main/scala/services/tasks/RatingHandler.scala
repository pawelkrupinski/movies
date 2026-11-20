package services.tasks

import java.time.Clock

import scala.util.Try

import services.attempts.EnrichmentAttemptStore
import services.cadence.RatingCadenceStore
import services.freshness.{FreshnessKind, FreshnessStore}
import services.tasktypes.TaskType

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
  // The WIRING's clock, never a default: the enqueuer judges a film due on the wiring's clock, so a
  // handler judging (and stamping) freshness on another re-asked every film each round once the two
  // drifted — Identity model convergence, run 36717160191.
  clock:                 Clock,
  metrics:               RatingLatencyMetrics = RatingLatencyMetrics.NoOp,
  // Where each attempt's outcome is recorded for the /debug per-film expand
  // section. Defaults to NoOp so scripts/tests opt in rather than out.
  attempts:              EnrichmentAttemptStore = EnrichmentAttemptStore.NoOp
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
      // Record the attempt whatever happens — a thrown fetch is the case the
      // /debug page most needs to show, and it never reached the lines below.
      // `Try` + rethrow, NOT a catch: the queue's retry/backoff must still see the
      // failure exactly as before; this only observes it on the way past.
      val attempted = Try(refresh(title, year))   // Some(displayValue) when the row's badge was (re)set this refresh
      RatingTasks.recordAttempt(attempts, key, now, clock.instant().toEpochMilli - now.toEpochMilli, attempted)
      val reported = attempted.get   // rethrows a failed fetch, as before
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
