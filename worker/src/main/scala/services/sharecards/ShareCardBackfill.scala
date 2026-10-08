package services.sharecards

import models.ResolvedMovie
import play.api.Logging
import services.events.{DomainEvent, TaskFinished}
import services.readmodel.ReadModelReader
import services.tasks.{EnqueueResult, TaskQueue, TaskType}

import java.time.{Clock, Instant}
import scala.util.Try

/**
 * Renders the cards no input change asked for — every film on screen at boot (the first rollout, a
 * template bump, a restart after the directory was lost) and, daily, whatever slipped through — and
 * publishes `kinowo_worker_share_cards_coverage_ratio`. Driven by events alone, with no task or
 * timer of its own:
 *
 *  - A PRUNE PASS SWEEPS. The first `PruneShareCards` pass to finish after boot (the budget pass,
 *    minutes in) and every daily prune read the corpus once and remember the films whose card is
 *    missing, and those drawn without their poster. The daily sweep follows its prune, so the films
 *    that left the screens are gone from the directory before it looks. A sweep whose read fails is
 *    due again at the next pass.
 *  - A FINISHED RENDER MAKES ROOM. What a sweep found missing goes onto the queue only while fewer
 *    than `maxBacklog` renders wait, each finished render letting the next one in ([[ShareCardBackfill.onTaskFinished]]) — so a cold start
 *    trails the scrapes instead of flooding the queue. A render dropped after its attempts finishes
 *    nothing, so every prune pass tops up too: a stalled drip waits ten minutes at most.
 *  - THE PROJECTION KEEPS THE FILM LIST CURRENT. Each film it projects replaces the sweep's inputs
 *    for it, and a film that leaves the screens is forgotten ([[BackfilledShareCardLedger]]). So
 *    the backfill never renders a sweep's stale inputs over a newer card — under the URL `web_movies`
 *    names for the newer one, which a preview cache then keeps for a year — and the gauge never
 *    counts a film whose run ended as one missing its card.
 *
 * THE GAUGE is the share of films on screen whose card for their latest inputs is on disk. Each
 * event re-checks only the film it is about, so publishing costs one file read, not one per film.
 */
class ShareCardBackfill(
  service:    ShareCardService,
  reader:     ReadModelReader,
  queue:      TaskQueue,
  metrics:    ShareCardMetrics,
  clock:      Clock,
  maxBacklog: settings.ShareCardBackfillMaxBacklog = settings.ShareCardBackfillMaxBacklog(ShareCardBackfill.DefaultMaxBacklog)
) extends Logging {
  import ShareCardBackfill.Tracked

  // Each film on screen: its card inputs as last read, by a sweep or a projection.
  private var tracked    = Map.empty[String, Tracked]
  // The tracked films whose card for those inputs is not on disk.
  private var uncovered  = Set.empty[String]
  // Films that left the screens since the last sweep, and when: a sweep whose read began before
  // they left must not bring them back.
  private var departed   = Map.empty[String, Instant]
  // The films the last sweep found without their card, in its order, not yet asked for.
  private var pending    = Vector.empty[String]
  // Films whose card was drawn without a poster because every poster failed: re-tried once per
  // sweep — a day, and at every boot — which is the backoff: a cinema origin that was down usually
  // comes back.
  private var posterless = List.empty[ShareCardInputs]
  private var sweepDue   = true
  private var swept      = false

  /** A prune pass finished: sweep when one is due — the first since boot, a daily one, or one
   *  after a sweep whose read failed — and top the queue up. */
  def afterPrune(daily: Boolean): Unit = {
    if (synchronized { sweepDue ||= daily; sweepDue }) sweep()
    topUp(); ()
  }

  /** A render finished: re-check its film's card, and let the next missing one in. */
  def afterRender(filmId: String): Unit = {
    synchronized { recheck(filmId); publish() }
    topUp(); ()
  }

  /** The projection wrote `movie`'s document: its inputs are now the latest for its card. */
  def onProjected(movie: ResolvedMovie, screened: Boolean): Unit =
    if (!screened) onRetired(movie._id)
    else synchronized {
      tracked += movie._id -> Tracked(service.inputs(movie), clock.instant())
      recheck(movie._id)
      publish()
    }

  /** The film left the screens: no card is expected of it any more. */
  def onRetired(filmId: String): Unit = synchronized {
    tracked   -= filmId
    uncovered -= filmId
    departed  += filmId -> clock.instant()
    publish()
  }

  /** Enqueue what the last sweep found missing while the render backlog has room. Returns how many
   *  renders it enqueued. */
  private def topUp(): Int = synchronized {
    if (pending.isEmpty && posterless.isEmpty) 0 else enqueueWithinBacklog()
  }

  // Called holding the lock.
  private def enqueueWithinBacklog(): Int =
    Try(queue.waitingCount(TaskType.RenderShareCard)).toOption.fold(0) { backlog =>
      var room     = maxBacklog.value - backlog
      var enqueued = 0
      while (room > 0 && pending.nonEmpty) {
        val filmId = pending.head
        pending = pending.tail
        // The film's latest inputs: a projection since the sweep may have rendered their card already.
        tracked.get(filmId).foreach { film =>
          service.request(film.inputs, askedAt = film.readAt) match {
            case None                      => recheck(filmId)
            case Some(EnqueueResult.Added) => enqueued += 1; room -= 1
            case Some(_)                   => ()
          }
        }
      }
      publish()
      // `lacksPoster` holds only while the card on disk is still the sweep's inputs' own.
      val (retry, rest) = posterless.filter(service.lacksPoster).splitAt(math.max(room, 0))
      posterless = rest
      enqueued + retry.count(in => service.retryPoster(in) == EnqueueResult.Added)
    }

  private def sweep(): Unit = {
    val sweptAt = clock.instant()
    // Both reads must be whole. An unread `web_movies` used to come back empty and pass as
    // "no film expects a card": the sweep then ENDED — coverage cleared, nothing pending,
    // and no retry until the next sweep a day later.
    val read = for {
      screenings <- reader.findAllScreeningRefsChecked()
      movies     <- reader.findAllMoviesChecked()
    } yield (screenings, movies)
    read.answered.fold(logger.warn(s"share cards backfill: read-model read ${read.explain} — sweep skipped, retried at the next prune pass.")) { (screenings, movies) =>
      val screened = screenings.iterator.map(_.filmId).toSet
      val onScreen = movies.filter(movie => screened(movie._id))
      synchronized {
        // What the projection reported after the read began is fresher than the read.
        val left = departed.collect { case (filmId, at) if !at.isBefore(sweptAt) => filmId }.toSet
        tracked = onScreen.iterator.filterNot(movie => left(movie._id)).map(movie => movie._id -> Tracked(service.inputs(movie), sweptAt)).toMap ++
          tracked.filter { case (_, film) => film.readAt.isAfter(sweptAt) }
        departed   = Map.empty
        uncovered  = Set.empty
        tracked.keysIterator.foreach(recheck)
        pending    = onScreen.iterator.map(_._id).filter(uncovered).toVector
        posterless = tracked.valuesIterator.map(_.inputs).filter(service.lacksPoster).toList
        sweepDue   = false
        swept      = true
        publish()
        logger.info(s"share cards backfill: ${pending.size} of ${tracked.size} cards missing, ${posterless.size} drawn without their poster.")
      }
    }
  }

  /** Whether the tracked film's card for its latest inputs is on disk. Called holding the lock. */
  private def recheck(filmId: String): Unit =
    tracked.get(filmId).foreach { film =>
      if (service.existing(film.inputs).isDefined) uncovered -= filmId else uncovered += filmId
    }

  // Not before the first sweep: until then the projection's films are only those it happened to touch.
  // With none on screen the sample is withdrawn, not published: left as it was, the gauge froze at whatever
  // the last film to leave had left it at; read as full coverage, a wiped or failed web_screenings read
  // (which tracks nothing either) silenced ShareCardCoverageLow and ShareCardCoverageAbsent alike.
  private def publish(): Unit =
    if (swept) {
      if (tracked.isEmpty) metrics.coverageUnknown()
      else metrics.coverage((tracked.size - uncovered.size).toDouble / tracked.size)
    }
}

object ShareCardBackfill {

  /** What drives the backfill, on the task framework's completion event. By name: the backfill is
   *  built at the first event, not when the wiring subscribes. */
  def onTaskFinished(backfill: => ShareCardBackfill): PartialFunction[DomainEvent, Unit] = {
    case TaskFinished(TaskType.PruneShareCards, _, payload) =>
      backfill.afterPrune(daily = !payload.get(PruneShareCardsHandler.ModeKey).contains(PruneShareCardsHandler.Budget))
    case TaskFinished(TaskType.RenderShareCard, _, payload) =>
      ShareCardInputs.fromPayload(payload).foreach(inputs => backfill.afterRender(inputs.filmId))
  }

  /** A film's card inputs, and when they were read. */
  private final case class Tracked(inputs: ShareCardInputs, readAt: Instant)

  /** No backfill enqueue while this many renders already wait (`KINOWO_SHARE_CARD_BACKFILL_MAX_BACKLOG`). */
  val DefaultMaxBacklog: Int = 40
}

/** The ledger the projection talks to: the service's, with the backfill told which films are on
 *  screen with which inputs — the events that keep its film list current between sweeps. */
final class BackfilledShareCardLedger(service: ShareCardService, backfill: ShareCardBackfill) extends services.readmodel.ShareCardLedger {
  def current(movie: ResolvedMovie): Option[String]                    = service.current(movie)
  def readyToPublish(movie: ResolvedMovie): Boolean                    = service.readyToPublish(movie)
  def requestFirstCard(movie: ResolvedMovie, until: Instant): Unit     = service.requestFirstCard(movie, until)
  def onPendingCardLanded(filmId: String): Unit                        = service.onPendingCardLanded(filmId)
  def onProjected(movie: ResolvedMovie, screened: Boolean): Unit       = { service.onProjected(movie, screened); backfill.onProjected(movie, screened) }
  def onRetired(filmId: String): Unit                                  = { service.onRetired(filmId); backfill.onRetired(filmId) }
}
