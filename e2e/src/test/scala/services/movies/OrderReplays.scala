package services.movies

/** When a convergence suite's order-independence replays start. */
object OrderReplays {

  /** Beside the boot only in a HERMETIC run, which sends nothing: there the replays and the boot share only the
   *  runner's cores. A RECORDING run fetches live, and four pipelines asking TMDB at once drew 429s until its breaker
   *  opened and the replays resolved no film at all (Germany, recording run 37719613743) — the replays wait for their
   *  test there, after the boot, as they did before they were overlapped. Never when the run does not include the
   *  order test. */
  def besideTheBoot(hermetic: Boolean, included: Boolean): Boolean = hermetic && included

  /** Wait, at most `within`, for replays that were started — before a suite's teardown writes what they name (the
   *  refetch list) and closes the databases they write. Their failure is their test's to report, never the teardown's. */
  def settle(started: Option[tools.Alongside.Started[?]], within: scala.concurrent.duration.FiniteDuration): Unit =
    started.foreach(replays => scala.util.Try(replays.join(within)))
}
