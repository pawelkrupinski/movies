package services.enrichment

import java.util.concurrent.{ConcurrentHashMap, ScheduledExecutorService, TimeUnit}

import scala.concurrent.ExecutionContextExecutorService
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

import play.api.Logging
import services.Drainable
import services.events.{DomainEvent, ImdbIdMissing}

/**
 * Recovers missing IMDb ids off the bus: each `ImdbIdMissing` is resolved ([[ImdbIdRecovery]]) on the resolver's own
 * pool, so the publisher (the TMDB stage worker) isn't blocked on IMDb, and a lookup that FAILED is asked again later
 * ([[ImdbIdRetryPolicy]]). Lifecycle owned by the composition root.
 *
 * Split out of `ImdbRatings` so rating maintenance doesn't entangle with
 * id discovery — `ImdbRatings` now only deals with the rating lifecycle and
 * can rely on `imdbId` being already set on the rows it touches.
 */
class ImdbIdResolver(
  recovery: ImdbIdRecovery,
  // IMDb's suggestion endpoint is fast and the event-driven path is sparse (the
  // TMDB stage emits many resolutions at startup but very few carry an
  // `ImdbIdMissing`). Virtual threads keep per-task overhead trivial; the rate
  // cap, if any, sits at the HTTP layer. `Wiring` injects a shared-budget EC. See
  // `SharedExecutionBudget`.
  executionContext:    ExecutionContextExecutorService,
  // Where a lookup that FAILED (IMDb or a backstop down, rate-limited, timing out) waits for its
  // next attempt ([[ImdbIdRetryPolicy.retryDelay]]). Nothing else re-asks a missing id outside Poland
  // (Filmweb's refresh does there), so without it one blip cost the film its IMDb rating for good.
  retries: ScheduledExecutorService,
  // Whether a failed lookup waits on [[retries]] at all. False only over a hermetic replay (`ArchiveReplayWiring`),
  // whose wire answers a retry exactly as it answered the first ask: there a retry fired a minute or more later on the
  // WALL clock, so whether its answer landed before a pass was snapshotted decided the film's imdbId, and two passes
  // over one recording disagreed (Verify scrape recording #2245: 'Royal Ballet & Opera 2026/27: Swan Lake').
  val retriesFailures: Boolean = true,
  random: () => Double = () => scala.util.Random.nextDouble(),
  // The wiring's monotonic clock: how long a `drain` has waited on the resolver's pool.
  stopwatch: tools.Stopwatch,
  sleep: scala.concurrent.duration.FiniteDuration => Unit
) extends Drainable with Logging {

  private val pool = new tools.DrainablePool(executionContext, stopwatch, sleep)

  /** Bus listener: when the TMDB stage resolved a film but TMDB has no IMDb
   *  cross-reference for it, recover the id ([[ImdbIdRecovery]]) and write it back to the cached row — the
   *  `EnrichmentReaper` then enqueues its IMDb rating on the next pass. */
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, searchTitle) => pool.submit(resolveOrRetry(title, year, searchTitle, attempt = 0))
  }

  /** The films whose failed lookup waits on [[retries]] — one wait per film, however many events
   *  name it meanwhile, so an outage cannot multiply them. */
  private val waiting = ConcurrentHashMap.newKeySet[(String, Option[Int])]()

  /** The event path has no caller to hand a failure to, so it retries here: a failed lookup is not
   *  a "no match", and the film is asked again on a growing backoff until a source answers or the
   *  failure is itself the answer (`TaskWorker.failedDefinitively`: a 404, a deterministic error). */
  private def resolveOrRetry(title: String, year: Option[Int], searchTitle: String, attempt: Int): Unit =
    try recovery.resolve(title, year, searchTitle)
    catch {
      case NonFatal(failure) if ImdbIdRetryPolicy.definitive(failure) =>
        logger.info(s"IMDb-id: lookup for '$title' (${year.getOrElse("?")}) [search='$searchTitle'] failed for good: ${failure.getMessage}")
      case NonFatal(failure) if !retriesFailures =>
        logger.info(s"IMDb-id: lookup for '$title' (${year.getOrElse("?")}) [search='$searchTitle'] failed, not concluded; " +
          s"not retried (a hermetic replay answers a retry as it answered this): ${failure.getMessage}")
      case NonFatal(failure) =>
        val delay = ImdbIdRetryPolicy.retryDelay(attempt, failure, random())
        // None: a retry for the film already waits. A failed schedule is the scheduler stopping with the worker:
        // the film is asked again on its next trigger.
        val retry = Option.when(waiting.add(title -> year))(Try(retries.schedule(
          (() => { waiting.remove(title -> year); pool.submit(resolveOrRetry(title, year, searchTitle, attempt + 1)) }): Runnable,
          delay.toMillis, TimeUnit.MILLISECONDS)))
        retry.foreach(_.failed.foreach(_ => waiting.remove(title -> year)))
        val next = retry match {
          case Some(Success(_))       => s"retrying in ${delay.toSeconds}s (attempt ${attempt + 1})"
          case Some(Failure(stopped)) => s"not retried (${stopped.getClass.getSimpleName}: the resolver is stopping)"
          case None                   => "a retry already waits"
        }
        logger.warn(s"IMDb-id: lookup for '$title' (${year.getOrElse("?")}) [search='$searchTitle'] failed, not concluded; " +
          s"$next: ${failure.getMessage}")
    }

  /** Wait for in-flight id write-backs, leaving the pool able to take more. Waits for
   *  the queue to drain, not a fixed window — the bounded cap was returning before
   *  real-network suggestion lookups finished and `cascadeDrainOrder`'s next entry was
   *  shutting down a pool that still had inbound work coming. */
  def drain(): Unit = pool.drain()

  /** Drain within `budget`, then end the pool — shutdown only. A caller that merely wants the
   *  in-flight work finished wants [[drain]]: this one rejects everything after it.
   *
   *  Lookups still queued at the budget, and failed ones waiting to be retried, are dropped: the row
   *  keeps no imdbId, so it is asked again — on its next Filmweb rating refresh, the daily OMDb
   *  backfill sweep, or its next TMDB (re-)identification. */
  override def stopWithin(budget: scala.concurrent.duration.FiniteDuration): Unit = {
    retries.shutdownNow()
    val dropped = pool.stop(budget)
    if (dropped > 0) logger.info(s"ImdbIdResolver: stopped with $dropped id lookup(s) unfinished — dropped; each row is asked again on its next trigger.")
  }

  def stop(): Unit = stopWithin(tools.ManagedResources.Grace)
}
