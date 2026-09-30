package modules

import tools.Stopwatch

import scala.concurrent.duration.FiniteDuration

/**
 * What `/ready` reports: whether the process's heavy boot work has SETTLED — every country's
 * cache hydrate, read-model projector prepare and identity take-up done or failed — which
 * `/health` deliberately never waits for. The rollout reads it: the workers roll out a few at a
 * time, the next only once these are ready, because five booting at once saturated the node
 * (7.9 of 8 cores, 2026-09-30) and each boot then ran several times slower.
 *
 * Ready anyway once `cap` has passed since the process started, so a boot step that never
 * settles delays the next rollout by the cap, not forever. Readiness never restarts a pod.
 */
private[modules] final class BootReadiness(cap: FiniteDuration, stopwatch: Stopwatch = Stopwatch.System) {
  private val started = stopwatch.start()
  @volatile private var settled: () => Boolean = () => false

  def isReady: Boolean = settled() || started.elapsed >= cap

  /** From now on, ready once `real` says the boot work has settled. */
  def becomes(real: () => Boolean): Unit = settled = real
}
