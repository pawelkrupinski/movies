package services.movies

/** When a convergence suite's order-independence replays start. */
object OrderReplays {

  /** Beside the boot only in a HERMETIC run, which sends nothing: there the replays and the boot share only the
   *  runner's cores. A RECORDING run fetches live, and four pipelines asking TMDB at once drew 429s until its breaker
   *  opened and the replays resolved no film at all (Germany, recording run 37719613743) — the replays wait for their
   *  test there, after the boot, as they did before they were overlapped. Never when the run does not include the
   *  order test. */
  def besideTheBoot(hermetic: Boolean, included: Boolean): Boolean = hermetic && included
}
