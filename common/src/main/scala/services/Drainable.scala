package services

/** A [[Stoppable]] whose worker pool can be QUIESCED without being ended.
 *
 *  `stop()` is a one-way door — it shuts the executor down, and every later
 *  submission is rejected. That is right at shutdown and wrong for a harness that
 *  drains between phases: the replay boot drains the enrichment pools, then folds
 *  staging, and the fold is what publishes `ImdbIdMissing`. Drained with `stop()`,
 *  every one of those events reached a dead pool and the whole id-recovery ladder
 *  — the route prod takes for bare-title films TMDB cannot identify — silently did
 *  nothing.
 *
 *  So the two operations are named apart: `drain()` waits for in-flight work and
 *  leaves the pool usable; `stop()` drains and then ends it. */
trait Drainable extends Stoppable {

  /** Wait for in-flight work to finish, leaving the pool able to accept more. */
  def drain(): Unit
}
