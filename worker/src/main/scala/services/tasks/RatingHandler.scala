package services.tasks

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
  refresh:               (String, Option[Int]) => Unit,
  clock:                 Clock = Clock.systemUTC()
) extends TaskHandler {
  import HandlerOutcome._

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!dueWindow.isDue(key, freshness.lastFetchedAt(key), clock.instant())) Skipped
    else {
      val title = task.payload.getOrElse(RatingTasks.TitleKey, "")
      val year  = task.payload.get(RatingTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
      refresh(title, year)
      freshness.markFresh(key, kind)
      Done
    }
  }
}
