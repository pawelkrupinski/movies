package services.identity

import services.movies.MovieChangeStream

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.concurrent.duration._

/**
 * Runs `run` — a projection of what moved ([[IdentityProjection.tickChanged]]) — a short while after it is asked to, as
 * the identity model takes this worker's scrapes in: each request pushes the run to `debounce.quiet` after it, never past
 * `debounce.cap` after the first request it gathers (the read model's change debounce, `MovieChangeStream.Debounce`). A
 * scrape's changes reach the films within the cap rather than at the next five-minute projection, and a projection does
 * a burst's work in one pass rather than in a five-minute batch with the whole corpus's fixed costs at once.
 *
 * A run that did not settle — refused, failed, a write declined, a film's TMDB details still missing — runs again after
 * a backoff (`retryAfter`, doubling to `retryAtMost`), which a settled one resets: there is no period to come back on.
 * [[retry]] asks the same of a run made elsewhere (the hourly reconcile).
 *
 * Requests arrive from the model's thread; `run` runs on `scheduler`'s.
 */
final class ProjectionTrigger(run: () => Boolean, debounce: MovieChangeStream.Debounce, scheduler: ScheduledExecutorService,
                              clock: Clock, retryAfter: FiniteDuration = ProjectionTrigger.RetryAfter,
                              retryAtMost: FiniteDuration = ProjectionTrigger.RetryAtMost) {
  private var pending: Option[ScheduledFuture[?]] = None
  private var since   = 0L
  private var backoff = retryAfter

  /** Run within `debounce` of now, with whatever else asks in the meantime. */
  def request(): Unit = synchronized {
    val now = clock.millis()
    if (pending.isEmpty) since = now
    schedule(math.min(now + debounce.quiet.toMillis, since + debounce.cap.toMillis) - now)
  }

  /** Run after the current backoff, unless a run is already due sooner; the backoff doubles. */
  def retry(): Unit = synchronized {
    val in = backoff.toMillis
    backoff = (backoff * 2).min(retryAtMost)
    if (pending.forall(_.getDelay(TimeUnit.MILLISECONDS) > in)) { since = clock.millis(); schedule(in) }
  }

  private def schedule(inMillis: Long): Unit = {
    pending.foreach(_.cancel(false))
    pending = Some(scheduler.schedule((() => fire()): Runnable, math.max(0L, inMillis), TimeUnit.MILLISECONDS))
  }

  private def fire(): Unit = {
    synchronized { pending = None }
    if (run()) synchronized { backoff = retryAfter } else retry()
  }
}

object ProjectionTrigger {
  /** How long an unsettled projection waits before it is tried again, and the most that wait doubles to. */
  val RetryAfter: FiniteDuration  = 1.minute
  val RetryAtMost: FiniteDuration = 15.minutes
}
