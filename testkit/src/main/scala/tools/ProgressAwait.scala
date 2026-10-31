package tools

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.Try

/**
 * A wait bounded by PROGRESS, not by the wall clock: `f` is waited for window after window for as long as `progress`
 * moves between them, and fails only once a whole window passes with no progress at all.
 *
 * For work whose length is the machine's, not the code's: HardClusterPasses' boot of every pass, which took past
 * [[SpecTimeouts.Run]] on a runner under load (it-e2e-measure #2748, #2776, #2800: `Future timed out after [10
 * minutes]` with every pass still projecting) and fails nothing there. A hang still fails, after one window of nothing.
 */
object ProgressAwait {

  /** `f`'s result; a `TimeoutException` naming `what` once `window` passes with `progress` unmoved. `ready` waits for
   *  `f` at most a window and says whether it completed (a spec passes its own, to hold the waits without a clock). */
  def result[T](what: String, f: Future[T], window: FiniteDuration, progress: () => Long)(
      ready: (Future[T], FiniteDuration) => Boolean = defaultReady[T]): T = {
    var seen = progress()
    while (!ready(f, window)) {
      val now = progress()
      if (now == seen) throw new TimeoutException(s"$what made no progress in $window (progress $now)")
      seen = now
    }
    f.value.get.get
  }

  private def defaultReady[T](f: Future[T], window: FiniteDuration): Boolean =
    Try(Await.ready(f, window)).isSuccess
}
