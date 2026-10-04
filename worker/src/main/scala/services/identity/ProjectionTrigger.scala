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
  /** Each window's burst since the last run: its first request, and when it is due. */
  private val bursts  = scala.collection.mutable.Map.empty[MovieChangeStream.Debounce, (Long, Long)]
  private var backoff = retryAfter

  /** Run within `debounce` of now, with whatever else asks in the meantime. */
  def request(): Unit = request(debounce)

  /** Run within `window` of now: each window gathers its own burst — pushed `window.quiet` past its last request, never
   *  past `window.cap` after its first — and the run is due when the soonest is, so a long window never postpones a
   *  short one (an agreement answer's, [[ProjectionTrigger.Answer]], beside a scrape's). */
  def request(window: MovieChangeStream.Debounce): Unit = synchronized {
    val now   = clock.millis()
    val since = bursts.get(window).fold(now)(_._1)
    bursts(window) = (since, math.min(now + window.quiet.toMillis, since + window.cap.toMillis))
    schedule(bursts.valuesIterator.map(_._2).min - now)
  }

  /** Run after the current backoff, unless a run is already due sooner; the backoff doubles. */
  def retry(): Unit = synchronized {
    val in = backoff.toMillis
    backoff = (backoff * 2).min(retryAtMost)
    if (pending.forall(_.getDelay(TimeUnit.MILLISECONDS) > in)) schedule(in)
  }

  /** Run `body` once, `after` from now, on this trigger's scheduler — no claim, no window: a worker's first projection,
   *  which no other projection may wait behind ([[IdentityProjection.tickChanged]] waits for it). */
  def once(after: FiniteDuration)(body: => Unit): Unit = {
    scheduler.schedule((() => body): Runnable, after.toMillis, TimeUnit.MILLISECONDS); ()
  }

  private def schedule(inMillis: Long): Unit = {
    pending.foreach(_.cancel(false))
    pending = Some(scheduler.schedule((() => fire()): Runnable, math.max(0L, inMillis), TimeUnit.MILLISECONDS))
  }

  private def fire(): Unit = {
    synchronized { pending = None; bursts.clear() }
    if (run()) synchronized { backoff = retryAfter } else retry()
  }
}

object ProjectionTrigger {
  /** An answer the identity can be updated by — the agreement's family answers: projected within seconds of it, a
   *  burst of them every few seconds at most. */
  val Answer: MovieChangeStream.Debounce = MovieChangeStream.Debounce(1.second, 5.seconds)
  /** How long an unsettled projection waits before it is tried again, and the most that wait doubles to. */
  val RetryAfter: FiniteDuration  = 1.minute
  val RetryAtMost: FiniteDuration = 15.minutes
}
