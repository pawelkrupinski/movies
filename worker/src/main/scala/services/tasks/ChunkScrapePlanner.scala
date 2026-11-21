package services.tasks

import java.time.Clock

import scala.concurrent.duration.*

import play.api.Logging
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, ListingReads}

/**
 * The PLAN ("split") step of a chunked scrape, called from `ScrapeCinemaHandler`
 * for a chunked cinema: enumerate the chunk keys (a nav/index fetch), start a
 * run (the per-cinema mutex in [[ChunkScrapeStore]]), and enqueue one
 * `ScrapeChunk` task per key. A day the plan's walk could not probe may hold films
 * no chunk will read: such a run is marked ([[ChunkScrapeStore.markPlanIncomplete]]).
 *
 * Failure handling matches a normal scrape: a nav-fetch throw — or a genuinely
 * empty repertoire — is published immediately as the scrape's outcome
 * ([[ChunkedListingPublisher.publishEmpty]], [[FailedChunkPlan]]) rather than starting an empty run. If a run is already
 * active for the cinema, planning is a no-op (one run at a time) — duplicate
 * `ScrapeCinema` tasks are harmless. The fan-out itself is [[ChunkFanOut]]'s.
 */
class ChunkScrapePlanner(
  chunkScrapers: Map[String, ChunkedCinemaScraper],
  store:         ChunkScrapeStore,
  fanOut:        ChunkFanOut,
  // A plan that never reaches the reduce (empty or failed) publishes and records its
  // outcome itself, so such a cinema still advances its due schedule.
  emptyPlan:     ChunkedListingPublisher,
  failedPlan:    FailedChunkPlan,
  staleAfter:    ChunkScrapePlanner.RunTimeout = ChunkScrapePlanner.RunTimeout(ChunkScrapePlanner.DefaultRunTimeout),
  clock:         Clock,
  // Told of each run this planner starts, so its abandonment deadline is armed then
  // ([[ChunkRunDeadlines.arm]]) rather than found by polling every run.
  runStarted:    ChunkRun => Unit = _ => ()
) extends Logging {

  /** The chunked scraper for `cinema`, when it is scraped in chunks. */
  def scraper(cinema: String): Option[CinemaScraper] = chunkScrapers.get(cinema)

  /** [[plan]], for the `ScrapeCinema` handler: a plan records a failed fetch itself; what still escapes (a
   *  store write that failed) is logged and leaves the venue due for the next tick, so the queue never
   *  reschedules. A shutdown's interrupt is no fault: kept set for the task loop to stop on. */
  def planLogged(cinema: String): Unit =
    try { val _ = plan(cinema) }
    catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        logger.info(s"chunked plan for $cinema interrupted — the worker is stopping")
      case e: Exception => logger.error(s"chunked plan for $cinema threw", e)
    }

  /** Returns the number of `ScrapeChunk` tasks enqueued (0 when a run was already
   *  active, or when the plan was published directly as empty/failed). */
  def plan(cinema: String): Int = chunkScrapers.get(cinema) match {
    case None => 0
    // Bail BEFORE planChunks() when a run is already active. The store's `startRun` is
    // the real mutex, but reaching it costs a nav fetch of the venue's programme
    // page purely to discover keys we are about to throw away — and a duplicate
    // ScrapeCinema is the COMMON case, not a rare race, because a chunked venue
    // stays due for its whole run (see [[ScrapeInFlight]]). Prod 2026-07-29: ~90-110
    // such no-ops an hour on the UK worker, each paying that fetch.
    case Some(_) if store.activeRun(cinema).exists(!_.isStale(clock.instant(), staleAfter.value)) => 0
    case Some(scraper) =>
      val planned =
        try Right(ListingReads.during(scraper.planSchedule()))
        catch { case e: Exception => Left(e) }
      planned match {
        case Left(e)                                   => failedPlan.publish(scraper, e); 0
        case Right((plan, reads)) if plan.keys.isEmpty => emptyPlan.publishEmpty(scraper, plan, reads.complete); 0
        case Right((plan, reads))                      => startRun(scraper, cinema, plan.keys, planComplete = reads.complete)
      }
  }

  /** Start a run over the plan's `keys` and enqueue a chunk task for each — 0 when a run is already active. */
  private def startRun(scraper: ChunkedCinemaScraper, cinema: String, keys: Seq[String], planComplete: Boolean): Int = {
    val now = clock.instant()
    store.startRun(cinema, keys, now, staleAfter.value) match {
      case None => 0
      case Some(runId) =>
        if (!planComplete) store.markPlanIncomplete(cinema, runId, now)
        // Armed before the fan-out: a fan-out that throws part way leaves its run to the deadline's partial reduce, as
        // a chunk task the queue could not take does, not started with no timer until the hourly sweep.
        runStarted(ChunkRun(cinema, runId, keys, now))
        fanOut.enqueue(scraper, cinema, runId, keys, now)
    }
  }
}

object ChunkScrapePlanner {

  /** How long a chunked run may go without finishing before it is abandoned and re-planned. */
  final case class RunTimeout(value: FiniteDuration) extends AnyVal

  /** A run with no fresh completion this long is considered abandoned: a new
   *  `ScrapeCinema` supersedes it, and the backstop reduces its partial data. */
  val DefaultRunTimeout: FiniteDuration = 15.minutes
}
