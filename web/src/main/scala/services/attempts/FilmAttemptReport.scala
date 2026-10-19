package services.attempts

import services.cadence.{RatingCadence, RatingCadenceReader, RatingChangeStats}
import services.freshness.FreshnessKind

import java.time.Instant
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * One rating source's enrichment state for ONE film, as the /debug row's expand
 * section shows it: what the last attempt did, and what the adaptive cadence
 * intends to do next.
 *
 * The two halves answer different questions and are only useful together —
 * "the last fetch failed" (attempt) versus "and it won't be retried for another
 * 3 days" (cadence). Reading either alone is what made a stuck film hard to
 * explain from the outside.
 */
case class SourceAttemptStatus(
  source:  FreshnessKind,
  key:     String,
  attempt: Option[EnrichmentAttempt],
  stats:   Option[RatingChangeStats]
) {
  /** When this source is next eligible to run — `lastCheckedAt` plus the interval
   *  the cadence's current backoff level implies. None when it has never run (it
   *  is due immediately, which is exactly what an absent freshness stamp means to
   *  `DueWindow`). */
  def nextDueAt: Option[Instant] =
    stats.map(s => s.lastCheckedAt.plusNanos(RatingCadence.intervalFor(stats).toNanos))

  /** True when the last attempt errored — the case worth colouring on the page. */
  def failing: Boolean = attempt.exists(_.failed)

  /** Never attempted at all. Distinct from "attempted and failed": an eligible
   *  source with no attempt means it was never enqueued (no id, or the row was
   *  ineligible when the enqueuer last looked), not that fetching went wrong. */
  def neverRan: Boolean = attempt.isEmpty
}

object FilmAttemptReport {
  /** The four rating sources, in the order [[services.tasks.RatingSources]]
   *  enqueues them, so the page reads in the same order the pipeline works. */
  val Sources: Seq[FreshnessKind] =
    Seq(FreshnessKind.ImdbRating, FreshnessKind.FilmwebRating, FreshnessKind.RtRating, FreshnessKind.McRating)

  /** Every rating key for a resolved film — what the readers' `forKeys` take. */
  def keysFor(tmdbId: Int): Seq[String] = Sources.map(RatingKeys.tmdbKey(_, tmdbId))

  /**
   * Join the attempt log and the cadence history into one per-source row.
   *
   * A film with no `tmdbId` yields an EMPTY report rather than title-keyed rows:
   * both stores are tmdbId-keyed for anything resolved, and a title-keyed
   * fallback here would silently show a different film's history after a
   * cross-language fold re-keyed the row (the same hazard the tmdbId-keyed
   * freshness migration exists to avoid).
   */
  def build(
    tmdbId:   Option[Int],
    attempts: Map[String, EnrichmentAttempt],
    cadence:  Map[String, RatingChangeStats]
  ): Seq[SourceAttemptStatus] =
    tmdbId.toSeq.flatMap { id =>
      Sources.map { source =>
        val key = RatingKeys.tmdbKey(source, id)
        SourceAttemptStatus(source, key, attempts.get(key), cadence.get(key))
      }
    }

  /**
   * [[build]], but reading the two stores CONCURRENTLY.
   *
   * The attempt log and the cadence history are independent — each is keyed only
   * on the film's rating keys, neither feeds the other — yet each is its own
   * Mongo round-trip. Awaiting them in turn pays two RTTs, and against a remote
   * Mongo (the dev `flyctl` tunnel: ~110ms each) those round-trips ARE the /debug
   * row-expand latency, not the queries, which are bounded `_id in [...]` reads
   * that execute in single-digit ms. Issuing both before awaiting either pays one.
   *
   * An unresolved film (no `tmdbId`) reads nothing at all — [[keysFor]] yields no
   * keys, both stores would short-circuit to empty, and [[build]] drops the row.
   */
  def buildFrom(
    tmdbId:        Option[Int],
    attemptReader: EnrichmentAttemptReader,
    cadenceReader: RatingCadenceReader,
    timeout:       FiniteDuration = 30.seconds
  )(using ExecutionContext): Seq[SourceAttemptStatus] = {
    val keys = tmdbId.toSeq.flatMap(keysFor)
    if (keys.isEmpty) Seq.empty
    else {
      val attempts = Future(attemptReader.forKeys(keys))
      val cadence  = Future(cadenceReader.forKeys(keys))
      build(tmdbId, Await.result(attempts, timeout), Await.result(cadence, timeout))
    }
  }
}
