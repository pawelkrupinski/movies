package services.identity

import services.movies.MovieChangeStream

import java.time.Clock
import java.util.concurrent.{RejectedExecutionException, ScheduledExecutorService, ScheduledFuture, TimeUnit}
import scala.concurrent.duration._

/**
 * Runs `run` a short while after it is asked to, as events arrive — the identity projection of what moved
 * ([[IdentityProjection.tickChanged]]) and the fill of the gaps the model found ([[ShadowLookupFill]]): each request pushes
 * the run to `debounce.quiet` after it, never past `debounce.cap` after the first request it gathers (the read model's
 * change debounce, `MovieChangeStream.Debounce`). What an event moved is acted on within the cap, a burst in one run,
 * with no period between.
 *
 * A run that did not settle (a projection refused, failed, a write declined, a film's TMDB details still missing) runs again after
 * a backoff (`retryAfter`, doubling to `retryAtMost`), which a settled one resets: there is no period to come back on.
 * [[retry]] asks the same of a run made elsewhere (the hourly reconcile).
 *
 * Requests arrive from the model's thread; `run` runs on `scheduler`'s.
 */
final class EventTrigger(run: () => Boolean, debounce: MovieChangeStream.Debounce, scheduler: ScheduledExecutorService,
                              clock: Clock, retryAfter: FiniteDuration = EventTrigger.RetryAfter,
                              retryAtMost: FiniteDuration = EventTrigger.RetryAtMost) {
  private var pending: Option[ScheduledFuture[?]] = None
  /** Each window's burst since the last run: its first request, and when it is due. */
  private val bursts  = scala.collection.mutable.Map.empty[MovieChangeStream.Debounce, (Long, Long)]
  private var backoff = retryAfter

  /** Run within `debounce` of now, with whatever else asks in the meantime. */
  def request(): Unit = request(debounce)

  /** Run within `window` of now: each window gathers its own burst — pushed `window.quiet` past its last request, never
   *  past `window.cap` after its first — and the run is due when the soonest is, so a long window never postpones a
   *  short one (an agreement answer's, [[EventTrigger.Answer]], beside a scrape's). */
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
  def once(after: FiniteDuration)(body: => Unit): Unit = { submit(after.toMillis)(body); () }

  private def schedule(inMillis: Long): Unit = {
    pending.foreach(_.cancel(false))
    pending = submit(math.max(0L, inMillis))(fire())
  }

  /** Nothing, once `scheduler` has shut down: the worker is stopping, and a drain's last batch asking for a run
   *  must not throw into the model's `safely`, which would rebuild the whole model from its store on the way out. */
  private def submit(inMillis: Long)(body: => Unit): Option[ScheduledFuture[?]] =
    try Some(scheduler.schedule((() => body): Runnable, inMillis, TimeUnit.MILLISECONDS))
    catch { case _: RejectedExecutionException if scheduler.isShutdown => None }

  private def fire(): Unit = {
    synchronized { pending = None; bursts.clear() }
    if (run()) synchronized { backoff = retryAfter } else retry()
  }
}

object EventTrigger {
  /** An answer the identity can be updated by — the agreement's family answers: projected within seconds of it, a
   *  burst of them every few seconds at most. */
  val Answer: MovieChangeStream.Debounce = MovieChangeStream.Debounce(1.second, 5.seconds)
  /** How long an unsettled projection waits before it is tried again, and the most that wait doubles to. */
  val RetryAfter: FiniteDuration  = 1.minute
  val RetryAtMost: FiniteDuration = 15.minutes
}
