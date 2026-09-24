package services.sharecards

import play.api.Logging
import services.readmodel.ReadModelReader
import services.tasks.{EnqueueResult, TaskQueue, TaskType}

import java.time.{Clock, Instant}
import scala.concurrent.duration.*
import scala.util.Try

/**
 * Renders the cards no input change asked for: every film on screen at boot (the first rollout, a
 * restart after the directory was lost) and, daily, whatever slipped through. The work of the
 * `ShareCardBackfill` task, which a claimed recurring tick enqueues once a minute.
 *
 * RATE-LIMITED BY THE QUEUE ITSELF. A sweep reads the corpus once and remembers the films whose
 * card is missing; each tick then enqueues at most `batch` renders, and none while `maxBacklog`
 * renders are already waiting — so a cold start drips into the queue behind the scrapes instead
 * of flooding it (a 2,000-film country backfills in well under two hours).
 *
 * Each tick also publishes `kinowo_worker_share_cards_coverage_ratio` — the share of films on
 * screen whose card for their CURRENT inputs exists — from the sweep's list of films and their card inputs.
 */
class ShareCardBackfill(
  service:    ShareCardService,
  reader:     ReadModelReader,
  queue:      TaskQueue,
  metrics:    ShareCardMetrics,
  clock:      Clock,
  batch:      Int            = ShareCardBackfill.Batch,
  maxBacklog: Int            = ShareCardBackfill.MaxBacklog,
  sweepEvery: FiniteDuration = 24.hours
) extends Logging {

  private var pending   = List.empty[ShareCardInputs]
  // Films whose card was drawn without a poster because every poster failed: re-tried once per
  // sweep (a day), which is the backoff — a cinema origin that was down usually comes back.
  private var posterless = List.empty[ShareCardInputs]
  private var expected  = Seq.empty[ShareCardInputs]
  private var lastSweep = Option.empty[Instant]

  /** One tick: sweep when due, publish coverage, enqueue the next batch. Returns how many renders
   *  it enqueued. */
  def tick(): Int = synchronized {
    if (lastSweep.forall(at => !clock.instant().isBefore(at.plusMillis(sweepEvery.toMillis)))) sweep()
    if (expected.nonEmpty) metrics.coverage(expected.count(service.existing(_).isDefined).toDouble / expected.size)
    Try(queue.waitingCount(TaskType.RenderShareCard)).toOption.fold(0) { backlog =>
      val room          = math.min(batch, maxBacklog - backlog)
      val (next, later) = pending.filter(service.existing(_).isEmpty).splitAt(math.max(room, 0))
      pending = later
      val (retry, rest) = posterless.filter(service.lacksPoster).splitAt(math.max(room - next.size, 0))
      posterless = rest
      next.count(in => service.request(in).contains(EnqueueResult.Added)) +
        retry.count(in => service.retryPoster(in) == EnqueueResult.Added)
    }
  }

  private def sweep(): Unit = {
    val (screenings, complete) = reader.findAllScreeningRefsChecked()
    if (complete) {
      val screened = screenings.iterator.map(_.filmId).toSet
      expected  = reader.findAllMovies().filter(movie => screened(movie._id)).map(service.inputs)
      pending   = expected.filter(service.existing(_).isEmpty).toList
      posterless = expected.filter(service.lacksPoster).toList
      lastSweep = Some(clock.instant())
      logger.info(s"share cards backfill: ${pending.size} of ${expected.size} cards missing, ${posterless.size} drawn without their poster.")
    } else logger.warn("share cards backfill: web_screenings read incomplete — sweep skipped, retried next tick.")
  }
}

object ShareCardBackfill {
  /** Renders enqueued per tick at most. */
  val Batch: Int      = tools.Env.positiveInt("KINOWO_SHARE_CARD_BACKFILL_BATCH", 20)
  /** No backfill enqueue while this many renders already wait. */
  val MaxBacklog: Int = tools.Env.positiveInt("KINOWO_SHARE_CARD_BACKFILL_MAX_BACKLOG", 40)
}
