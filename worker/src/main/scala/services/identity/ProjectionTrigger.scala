package services.identity

import services.movies.MovieChangeStream

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, ScheduledFuture, TimeUnit}

/**
 * Runs `run` — a projection of what moved ([[IdentityProjection.tickChanged]]) — a short while after it is asked to, as
 * the identity model takes this worker's scrapes in: each request pushes the run to `debounce.quiet` after it, never past
 * `debounce.cap` after the first request it gathers (the read model's change debounce, `MovieChangeStream.Debounce`). A
 * scrape's changes reach the films within the cap rather than at the next five-minute projection, and a projection does
 * a burst's work in one pass rather than in a five-minute batch with the whole corpus's fixed costs at once.
 *
 * Requests arrive from the model's thread; `run` runs on `scheduler`'s.
 */
final class ProjectionTrigger(run: () => Unit, debounce: MovieChangeStream.Debounce, scheduler: ScheduledExecutorService,
                              clock: Clock) {
  private var pending: Option[ScheduledFuture[?]] = None
  private var since   = 0L

  /** Run within `debounce` of now, with whatever else asks in the meantime. */
  def request(): Unit = synchronized {
    val now = clock.millis()
    if (pending.isEmpty) since = now
    val due = math.min(now + debounce.quiet.toMillis, since + debounce.cap.toMillis)
    pending.foreach(_.cancel(false))
    pending = Some(scheduler.schedule((() => fire()): Runnable, math.max(0L, due - now), TimeUnit.MILLISECONDS))
  }

  private def fire(): Unit = {
    synchronized { pending = None }
    run()
  }
}
