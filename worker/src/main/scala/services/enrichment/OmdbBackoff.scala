package services.enrichment

import java.time.Clock

import scala.concurrent.duration.*

/**
 * The durable per-film backoff of the OMDb id backfill ([[OmdbBackfill]]): a film OMDb could not identify is not asked
 * again until its (doubling) window elapses, so the daily sweep does not re-probe it every day and burn the free
 * 1000/day quota. Stamps live in `attempts` (no backoff over `OmdbAttemptStore.noop`: tests, Mongo-less wiring).
 *
 * A sweep ([[during]]) reads the whole store ONCE and answers from that snapshot: per-row reads, run corpus-wide every
 * sweep, drained the worker's shared-CPU credit to the floor (see OmdbAttemptStore.all). Outside a sweep each ask reads
 * the store directly.
 */
final class OmdbBackoff(attempts: OmdbAttemptStore, clock: Clock) {

  @volatile private var sweep: Option[Map[String, OmdbAttempt]] = None

  /** Whether a sweep is running — its asks are bulk ones ([[OmdbAsk.Bulk]]). */
  def sweeping: Boolean = sweep.isDefined

  /** `run` with every backoff read answered from one read of the store. */
  def during[A](run: => A): A = {
    sweep = Some(attempts.all())
    try run finally sweep = None
  }

  /** Whether the film of `title` and `year` (a row's key) missed recently and is still backing off. */
  def holds(title: String, year: Option[Int]): Boolean =
    attemptFor(title, year).exists(a => clock.instant().isBefore(a.at.plusMillis(OmdbBackoff.window(a.level).toMillis)))

  /** The film of `title` and `year` missed: its window doubles, from now. */
  def missed(title: String, year: Option[Int]): Unit = {
    val nextLevel = math.min(attemptFor(title, year).map(_.level).getOrElse(0) + 1, OmdbBackoff.MaxLevel)
    attempts.record(filmKey(title, year), nextLevel, clock.instant())
  }

  private def attemptFor(title: String, year: Option[Int]): Option[OmdbAttempt] = sweep match {
    case Some(snapshot) => snapshot.get(filmKey(title, year))
    case None           => attempts.get(filmKey(title, year))
  }

  private def filmKey(title: String, year: Option[Int]): String =
    s"$title|${year.map(_.toString).getOrElse("")}"
}

object OmdbBackoff {
  /** Cap on consecutive-miss backoff levels — level 5 already reaches the 30-day ceiling. */
  val MaxLevel = 5
  private val Base = 2.days
  private val Max  = 30.days

  /** Exponential backoff after `level` consecutive misses: 2d, 4d, 8d, 16d, then the 30-day cap. Level 0 (never
   *  missed) is immediately eligible. */
  def window(level: Int): FiniteDuration =
    if (level <= 0) Duration.Zero
    else {
      val scaled = Base * math.pow(2, (level - 1).toDouble).toLong
      if (scaled > Max) Max else scaled
    }
}
