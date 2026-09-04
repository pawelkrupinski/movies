package modules

import services.tasks.LivenessWatchdog

/**
 * The countries one worker PROCESS runs, addressed as a single machine.
 *
 * A worker JVM builds one [[WorkerWiring]] per country in `KINOWO_COUNTRIES`,
 * each with its own heartbeat and watchdog. `/health` is a property of the
 * MACHINE, not of any one country, so it has to fold across every wiring rather
 * than read the first one.
 *
 * It previously took `wirings.head`, which was invisible on a single-country
 * deploy and wrong the moment a second country joined: `/health` answered 200
 * while a non-primary country's heartbeat was stale, so the one backstop that
 * survives a dead watchdog scheduler never fired.
 *
 * A `setThrottled` fold sat beside `isAlive` for the same reason, applying an
 * externally-pushed CPU-credit back-off to every country at once. Shared-CPU
 * credit was a Fly billing concept and the whole throttle path went with the
 * platform; this class is down to the one fold.
 *
 * Deliberately built over [[LivenessWatchdog]] rather than [[WorkerWiring]]:
 * that is the capability the endpoint actually needs, and it is cheap to
 * construct, so the fold is unit-testable without standing up a Mongo-backed
 * wiring per country.
 */
private[modules] class WorkerFleet(watchdogs: Seq[LivenessWatchdog]) {

  /** True only while EVERY country's heartbeat is fresh — one wedged country
   *  wedges the process, because they share a JVM. */
  def isAlive: Boolean = watchdogs.forall(_.isAlive)
}
