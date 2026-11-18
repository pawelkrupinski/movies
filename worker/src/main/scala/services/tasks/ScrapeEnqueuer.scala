package services.tasks

import services.tasktypes.TaskType

import play.api.Logging

import java.time.Clock
import scala.util.Try

/** The queue side of a [[ScrapeReaper]] tick: how much scrape work is already waiting, and the
 *  enqueue of the venues admitted. */
final class ScrapeEnqueuer(queue: TaskProducer, admission: ScrapeAdmission, clock: Clock) extends Logging {

  /** Outstanding work in TASKS. A waiting ScrapeCinema is counted as the fan-out it is
   *  about to become, not the one task it currently is: on a chunked country the
   *  planner is an INTENTION to enqueue ~36 more, and a budget that ignores that keeps
   *  admitting against room it has already committed. Prod showed exactly that shape —
   *  ScrapeCinema=13 waiting with ScrapeChunk=0, ~470 tasks of pending fan-out read as 13.
   *  A backlog that cannot be READ is not an empty backlog: `waitingCount` throws on a
   *  failed read (it used to answer 0 — the reading of an empty queue, so a Mongo blip
   *  was the one moment the whole budget got admitted on top of an unknown pile), and so
   *  this fails.
   *
   *  Every task type a cinema scrape can be sitting in is counted: `ScrapeCinema` alone is NOT the
   *  scrape's cost. Counting only the planner makes the bound blind to two orders of magnitude of the
   *  work it has just created: the reaper sees a near-empty ScrapeCinema queue, judges itself under
   *  budget, and keeps enqueueing — so the pool never reaches the idle gap the bound exists to buy.
   *  Measured on kinowo-worker-uk 2026-07-28, when a credit-throttle back-off used the same count:
   *  381-1142 tasks waiting against a cap of 26, with the pool pinned busy all day. PL, whose venues
   *  mostly don't chunk, held 5-55 and recovered between ticks exactly as designed. */
  def outstanding(): Try[Int] = Try(
    queue.waitingCount(TaskType.ScrapeChunk) +
    queue.waitingCount(TaskType.ScrapeChunkReduce) +
    queue.waitingCount(TaskType.ScrapeCinema) * admission.typicalVenueCost
  )

  /** Enqueue due cinemas in order until `cap` NEW tasks have been added or the next
   *  doesn't fit `room`; already-waiting/working cinemas dedup for free (they count
   *  against neither). Returns how many tasks were actually added, and how many enqueues failed. */
  def enqueueUpTo(due: Vector[(String, String)], cap: Int, room: ScrapeAdmission.Room): Enqueued = {
    var enqueued = Enqueued.Zero
    var spent    = 0.0
    due.iterator.takeWhile { case (key, _) => enqueued.added < cap && room.fits(spent, key, first = enqueued.added == 0) }
      .foreach { case (key, displayName) =>
        val result = Enqueued.of(queue.enqueue(TaskType.ScrapeCinema, key,
          Map(ScrapeCinemaHandler.CinemaKey -> displayName), submittedAt = clock.instant()))
        if (result.added > 0) spent += admission.costOf(key)
        enqueued += result
      }
    if (enqueued.failed > 0) logger.warn(s"ScrapeReaper: ${enqueued.failed} scrape enqueue(s) failed; they stay due for the next tick.")
    enqueued
  }
}
