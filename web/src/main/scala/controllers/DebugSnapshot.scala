package controllers

import play.api.Logging
import services.MirrorFreshness

import java.time.{Clock, Duration => JDuration, Instant}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/** One `/debug` read, and the newest `updatedAt` the mirror held when it was taken —
 *  what the navbar's age badge renders, so the badge states the age of what is ON
 *  SCREEN rather than of the mirror right now. */
final case class DebugSnapshot[A](value: A, mirrorNewest: Option[Instant])

object DebugSnapshot {
  /** Read it now, on the caller's thread — a stack whose read is cheap enough to do per
   *  request (an in-memory model, every spec's in-memory repository). */
  def readNow[A](freshness: MirrorFreshness)(read: => A): () => DebugSnapshot[A] =
    () => DebugSnapshot(read, freshness.newestUpdate())
}

/**
 * A whole-collection `/debug` read served from memory instead of re-read per request.
 *
 * Why: the corpus listing stitches every film's `movie_slots` back in, and that read
 * scales with the corpus, not the page. Measured against the LOCAL mirror
 * (2026-10-03): US is 105k slots (~1.3 s for the scan alone) and 100k read-model
 * screenings (~0.8 s just to count them server-side), so every country switch cost
 * 4–10 s even with no network hop. A snapshot turns a switch into a map lookup.
 *
 * `get()` answers from the last snapshot at once; when that is older than
 * `refreshAfter` it also starts ONE background re-read (never two at once), so the
 * next load is current. Only the very first `get()` (nothing read yet) waits.
 * [[refreshIfOlderThan]] is the periodic warm-up's entry point, which keeps every
 * country's snapshot within a minute or so of the mirror whether or not anyone is
 * looking at it.
 *
 * A failed background re-read keeps the previous snapshot and logs. A failed FIRST
 * read throws, so the page shows the error instead of an empty table that would
 * read as an empty corpus.
 */
final class RefreshingSnapshot[A](
  label:        String,
  read:         () => A,
  freshness:    MirrorFreshness,
  refreshAfter: FiniteDuration,
  clock:        Clock,
)(using ec: ExecutionContext) extends Logging {

  private final case class Taken(snapshot: DebugSnapshot[A], at: Instant)

  private val current  = new AtomicReference[Option[Taken]](None)
  private val inFlight = new AtomicReference[Option[Future[Taken]]](None)

  def get(): DebugSnapshot[A] = current.get() match {
    case Some(taken) =>
      if (olderThan(taken, refreshAfter)) refresh()
      taken.snapshot
    // The 70 s sits above the reads' own 60 s timeouts, so an inner timeout fires (and logs) first.
    case None => Await.result(refresh(), 70.seconds).snapshot
  }

  /** Start a re-read unless the snapshot is younger than `threshold` (or one is already running). */
  def refreshIfOlderThan(threshold: FiniteDuration): Unit =
    if (current.get().forall(olderThan(_, threshold))) { refresh(); () }

  private def olderThan(taken: Taken, threshold: FiniteDuration): Boolean =
    JDuration.between(taken.at, clock.instant()).toMillis >= threshold.toMillis

  private def refresh(): Future[Taken] = {
    val promise = Promise[Taken]()
    if (inFlight.compareAndSet(None, Some(promise.future))) {
      promise.completeWith(Future {
        // Read the mirror's newest stamp BEFORE the data: the badge may then overstate
        // the snapshot's age by the read's duration, but never understate it.
        val newest = freshness.newestUpdate()
        val taken  = Taken(DebugSnapshot(read(), newest), clock.instant())
        // Published before the future completes, so a caller woken by it already sees it.
        current.set(Some(taken))
        taken
      })
      promise.future.onComplete { result =>
        result match {
          case Success(_)         => ()
          case Failure(exception) =>
            logger.warn(s"$label: re-read failed, keeping the previous snapshot: " +
              s"${exception.getClass.getSimpleName}: ${exception.getMessage}")
        }
        inFlight.set(None)
      }
      promise.future
    } else inFlight.get().getOrElse(refresh())
  }
}
