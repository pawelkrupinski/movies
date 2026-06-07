package services.tasks

import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.CacheKey

/** Dedup/freshness key + payload for a rating-refresh task. The key encodes the
 *  rating source and the film, so "refresh the IMDb rating of film F" can't be
 *  queued twice while one is active (the queue's unique index), and is shared
 *  across servers. */
object RatingTasks {
  val TitleKey = "title"
  val YearKey  = "year"

  def dedupKey(kind: FreshnessKind, key: CacheKey): String =
    s"${kind.label}|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  def payload(key: CacheKey): Map[String, String] =
    Map(TitleKey -> key.cleanTitle, YearKey -> key.year.map(_.toString).getOrElse(""))
}

/**
 * Handles one rating-refresh task by delegating to the existing per-row refresh
 * of a `*Ratings` class, freshness-gated to that source's TTL (4h). One generic
 * handler covers all four sources — they differ only in their `taskType`,
 * `kind`, and which `refreshOneSync` they call.
 *
 * A row is marked fresh after the refresh attempt (success or no-op), so the
 * staggered reaper won't re-enqueue it inside the 4h window — matching the old
 * 4h walk cadence, now deduped and shared across servers.
 */
class RatingHandler(
  override val taskType: TaskType,
  kind:                  FreshnessKind,
  freshness:             FreshnessStore,
  refresh:               (String, Option[Int]) => Unit
) extends TaskHandler {
  import HandlerOutcome._

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (freshness.isFresh(key, kind)) Skipped
    else {
      val title = task.payload.getOrElse(RatingTasks.TitleKey, "")
      val year  = task.payload.get(RatingTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
      refresh(title, year)
      freshness.markFresh(key, kind)
      Done
    }
  }
}
