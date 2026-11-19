package services.tasks

import services.freshness.FreshnessKind
import services.movies.RetriggerKind
import services.tasktypes.TaskType

/** What a retrigger kind enqueues: the IMDb-id resolve, or a rating task with the freshness stamp its handler writes on
 *  each ask. Its own two cases, so a new kind cannot fall into the resolve branch by answering "no rating". */
sealed trait RetriggerTask {
  /** Whether this task has a handler where `sources` are the rating sources wired: a rating task only when its own
   *  source is one of them, the IMDb-id resolve everywhere. */
  def appliesWith(sources: Seq[RatingSources.RatingSource]): Boolean = this match {
    case RetriggerTask.Rating(taskType, _) => sources.exists(_.taskType == taskType)
    case RetriggerTask.ResolveImdbId       => true
  }
}

object RetriggerTask {
  case object ResolveImdbId extends RetriggerTask
  final case class Rating(taskType: TaskType, freshness: FreshnessKind) extends RetriggerTask

  /** Which task a retrigger kind enqueues, as a match the compiler checks whole: a new kind must name its task. */
  def of(kind: RetriggerKind): RetriggerTask = kind match {
    case RetriggerKind.ResolveImdbId => ResolveImdbId
    case RetriggerKind.ImdbRating    => Rating(TaskType.ImdbRating,    FreshnessKind.ImdbRating)
    case RetriggerKind.FilmwebRating => Rating(TaskType.FilmwebRating, FreshnessKind.FilmwebRating)
    case RetriggerKind.RtRating      => Rating(TaskType.RtRating,      FreshnessKind.RtRating)
    case RetriggerKind.McRating      => Rating(TaskType.McRating,      FreshnessKind.McRating)
  }
}
