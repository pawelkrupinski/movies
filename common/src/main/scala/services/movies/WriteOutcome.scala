package services.movies

import play.api.Logger

import scala.util.control.NonFatal

/**
 * What a repository WRITE did — the write-side twin of the checked reads
 * (`findByIdChecked`, `findForFilmChecked`): a write that FAILED must not look like one
 * that landed.
 *
 * WHY (2026-09-24): a codec bug made `MovieRepository.upsert`, `SlotsRepository.replaceFilm`
 * and `upsertSlot` throw for ~6h (34 failures, 29 films). Every one of them was caught,
 * logged at WARN and returned `Unit`. `MovieCache.persist` had already put the row in the
 * cache, so the next identical scrape diffed as a no-op and never retried the write: two
 * new films never reached the site until a restart, and no metric moved.
 *
 *  - [[WriteOutcome.Written]] — the write landed, or the store already held exactly this.
 *  - [[WriteOutcome.Declined]] — the store deliberately did not write, and repeating the
 *    identical write would be declined again: another document holds the key, the client
 *    is closing, no store is configured. Not a failure; nothing is counted or rolled back.
 *  - [[WriteOutcome.Failed]] — the write threw. Counted by
 *    `kinowo_worker_repository_write_failed_total`, and a caller that has already updated
 *    an in-memory copy must roll it back so the next identical write is retried.
 */
sealed trait WriteOutcome {
  def failed: Boolean = false

  /** Run `next` only when this did not fail — a composite write reports its first failure. */
  def andThen(next: => WriteOutcome): WriteOutcome = if (failed) this else next
}

object WriteOutcome {
  case object Written extends WriteOutcome

  final case class Declined(reason: String) extends WriteOutcome

  final case class Failed(collection: String, op: String, cause: Throwable) extends WriteOutcome {
    override def failed: Boolean = true
  }

  /** The first failure among `outcomes` (every one of which has already run), else `Written`. */
  def all(outcomes: Iterable[WriteOutcome]): WriteOutcome =
    outcomes.find(_.failed).getOrElse(Written)
}

/** Where a failed repository write is counted. The worker wires the Prometheus-backed
 *  [[services.metrics.WorkerTaskMetrics]]; the web, scripts and most tests use `noop`. */
trait RepositoryWriteMetrics {
  def recordWriteFailed(collection: String, op: String, exception: String): Unit
}

object RepositoryWriteMetrics {
  val noop: RepositoryWriteMetrics = (_: String, _: String, _: String) => ()
}

/**
 * THE ONE PLACE a repository write's exception becomes a [[WriteOutcome]] — logged,
 * counted and returned, never swallowed into `Unit`. Every Mongo repository wraps its
 * writes in [[attempt]], and a test double that throws goes through the same call, so
 * "what counts as a failure and what is recorded for it" is decided once.
 */
object RepositoryWrite {
  def attempt(collection: String, op: String, what: => String, metrics: RepositoryWriteMetrics, logger: Logger)
             (body: => WriteOutcome): WriteOutcome =
    try body
    catch {
      // Shutdown race: the lifecycle closed the MongoClient while a worker was mid-write. The
      // driver throws IllegalStateException("state should be: open"). Nothing to retry into.
      case NonFatal(exception) if isClientClosed(exception) =>
        logger.debug(s"$what skipped — Mongo client closing.")
        WriteOutcome.Declined("client-closing")
      case NonFatal(exception) =>
        logger.warn(s"$what failed: ${exception.getMessage}")
        metrics.recordWriteFailed(collection, op, exception.getClass.getSimpleName)
        WriteOutcome.Failed(collection, op, exception)
    }

  /** [[attempt]] for a write whose body only returns once it has landed. */
  def unit(collection: String, op: String, what: => String, metrics: RepositoryWriteMetrics, logger: Logger)
          (body: => Any): WriteOutcome =
    attempt(collection, op, what, metrics, logger) { body; WriteOutcome.Written }

  def isClientClosed(exception: Throwable): Boolean =
    Option(exception.getMessage).exists(_.contains("state should be: open"))
}
