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
 * THE SWEEP'S INPUTS AGE. A film's inputs are read once a day, and its ratings move meanwhile; the
 * projection re-renders its card for the new ones as they change. So a film whose card on disk is no
 * longer the one the sweep saw is the projection's, not the backfill's: rendering the sweep's
 * inputs would overwrite a newer card with an older picture — under the URL `web_movies` names for
 * the newer one, which a preview cache then keeps for a year.
 *
 * Each tick also publishes `kinowo_worker_share_cards_coverage_ratio` — the share of films on
 * screen whose card for their CURRENT inputs exists: the sweep's inputs, or a card rendered since
 * the sweep (from inputs newer than the sweep's).
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
  import ShareCardBackfill.Swept

  // Each film's card inputs as the sweep read them, with its card's version on disk then.
  private var pending   = List.empty[Swept]
  // Films whose card was drawn without a poster because every poster failed: re-tried once per
  // sweep (a day), which is the backoff — a cinema origin that was down usually comes back.
  private var posterless = List.empty[ShareCardInputs]
  private var expected  = Seq.empty[Swept]
  private var lastSweep = Option.empty[Instant]

  /** One tick: sweep when due, publish coverage, enqueue the next batch. Returns how many renders
   *  it enqueued. */
  def tick(): Int = synchronized {
    if (lastSweep.forall(at => !clock.instant().isBefore(at.plusMillis(sweepEvery.toMillis)))) sweep()
    if (expected.nonEmpty) metrics.coverage(expected.count(covered).toDouble / expected.size)
    Try(queue.waitingCount(TaskType.RenderShareCard)).toOption.fold(0) { backlog =>
      val room          = math.min(batch, maxBacklog - backlog)
      val (next, later) = pending.filterNot(covered).splitAt(math.max(room, 0))
      pending = later
      // `lacksPoster` holds only while the card on disk is still the sweep's inputs' own.
      val (retry, rest) = posterless.filter(service.lacksPoster).splitAt(math.max(room - next.size, 0))
      posterless = rest
      next.count(film => service.request(film.inputs, askedAt = film.sweptAt).contains(EnqueueResult.Added)) +
        retry.count(in => service.retryPoster(in) == EnqueueResult.Added)
    }
  }

  /** The film's card is current for the sweep's inputs, or was rendered since from newer ones. */
  private def covered(film: Swept): Boolean = {
    val now = service.onDisk(film.inputs.filmId)
    now.exists(film.inputs.acceptableVersions.contains) || (now.nonEmpty && now != film.cardAtSweep)
  }

  private def sweep(): Unit = {
    val sweptAt                = clock.instant()
    val (screenings, screeningsRead) = reader.findAllScreeningRefsChecked()
    // Both reads must be whole. An unread `web_movies` used to come back empty and pass as
    // "no film expects a card": the sweep then ENDED — coverage cleared, nothing pending,
    // and no retry until the next sweep a day later.
    val (movies, moviesRead) = if (screeningsRead) reader.findAllMoviesChecked() else (Seq.empty, false)
    if (screeningsRead && moviesRead) {
      val screened = screenings.iterator.map(_.filmId).toSet
      expected  = movies.filter(movie => screened(movie._id))
        .map(movie => Swept(service.inputs(movie), service.onDisk(movie._id), sweptAt))
      pending   = expected.filterNot(covered).toList
      posterless = expected.map(_.inputs).filter(service.lacksPoster).toList
      lastSweep = Some(sweptAt)
      logger.info(s"share cards backfill: ${pending.size} of ${expected.size} cards missing, ${posterless.size} drawn without their poster.")
    } else logger.warn("share cards backfill: read-model read incomplete — sweep skipped, retried next tick.")
  }
}

object ShareCardBackfill {
  private final case class Swept(inputs: ShareCardInputs, cardAtSweep: Option[String], sweptAt: Instant)

  /** Renders enqueued per tick at most. */
  val Batch: Int      = tools.Env.positiveInt("KINOWO_SHARE_CARD_BACKFILL_BATCH", 20)
  /** No backfill enqueue while this many renders already wait. */
  val MaxBacklog: Int = tools.Env.positiveInt("KINOWO_SHARE_CARD_BACKFILL_MAX_BACKLOG", 40)
}
