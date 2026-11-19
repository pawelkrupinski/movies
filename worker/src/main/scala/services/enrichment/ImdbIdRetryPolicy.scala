package services.enrichment

import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

/** When a failed IMDb id lookup ([[ImdbIdResolver]]) is asked again, and when its failure is itself the answer. */
object ImdbIdRetryPolicy {
  /** The first retry's wait; each failed retry doubles it, up to [[MaxRetryDelay]]. */
  val FirstRetryDelay: FiniteDuration = 1.minute
  /** The longest a failed lookup waits: an outage of a day is asked about a few times an hour, not a few hundred. */
  val MaxRetryDelay: FiniteDuration = 6.hours

  /** The wait before retry `attempt + 1`: the doubling backoff, with "equal jitter" (half fixed, half `random`) so
   *  films an outage failed together do not come back together, and never shorter than the source's own
   *  `Retry-After` (capped at [[MaxRetryDelay]]). */
  def retryDelay(attempt: Int, failure: Throwable, random: Double): FiniteDuration = {
    val backoff  = (FirstRetryDelay.toMillis << math.min(attempt, 20)).min(MaxRetryDelay.toMillis)
    val jittered = backoff / 2 + (backoff / 2 * random).toLong
    val failures = failure +: failure.getSuppressed.toSeq
    val asked    = failures.collect { case e: tools.HttpStatusException => e.retryAfter }.flatten
    val waited   = (jittered +: asked.map(_.toMillis)).max.min(MaxRetryDelay.toMillis)
    // A spent OMDb budget (OmdbBudget) names when it has permits again, and asking before then asks nothing: wait for
    // it, past the cap, plus the jitter so the films it held do not all come back in the same second.
    val budget = failures.collect { case e: OMDbClient.BudgetSpentException => e.retryAfter.toMillis }.maxOption
    budget.fold(waited)(b => b.max(waited) + (backoff / 2 * random).toLong).millis
  }

  /** Whether a failed ladder is an ANSWER: every failure in it (the first and those suppressed beside it) one. */
  def definitive(failure: Throwable): Boolean =
    (failure +: failure.getSuppressed.toSeq).forall(services.tasks.TaskWorker.failedDefinitively)
}
