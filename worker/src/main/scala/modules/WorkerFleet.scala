package modules

import services.tasks.{ExternalThrottleGate, LivenessWatchdog}

/**
 * The countries one worker PROCESS runs, addressed as a single machine.
 *
 * A worker JVM builds one [[WorkerWiring]] per country in `KINOWO_COUNTRIES`,
 * each with its own heartbeat, watchdog and throttle gate. The two process-wide
 * control surfaces — Fly's `/health` check and the external `/throttle` push —
 * are properties of the MACHINE, not of any one country, so they have to fold
 * across every wiring rather than read the first one.
 *
 * Both endpoints previously took `wirings.head`, which was invisible on a
 * single-country deploy and wrong the moment a second country joined:
 *
 *  - `/health` answered 200 while a non-primary country's heartbeat was stale,
 *    so the one backstop that survives a dead watchdog scheduler never fired.
 *  - `/throttle` backed off only the primary country. The Grafana alert driving
 *    it watches `fly_instance_cpu_balance` — a per-MACHINE metric — so credit
 *    exhaustion is shared, but the other countries kept enqueueing straight
 *    through the throttle that was supposed to let credit rebuild.
 *
 * Deliberately built over [[LivenessWatchdog]] / [[ExternalThrottleGate]] rather
 * than [[WorkerWiring]]: those are the two capabilities the endpoints actually
 * need, and both are cheap to construct, so the fold is unit-testable without
 * standing up a Mongo-backed wiring per country.
 */
private[modules] class WorkerFleet(
  watchdogs: Seq[LivenessWatchdog],
  gates:     Seq[ExternalThrottleGate]
) {

  /** True only while EVERY country's heartbeat is fresh — one wedged country
   *  wedges the process, because they share a JVM. */
  def isAlive: Boolean = watchdogs.forall(_.isAlive)

  /** Apply an external throttle decision to EVERY country: the CPU credit it
   *  protects is a property of the machine, so a partial back-off doesn't let
   *  the balance recover. */
  def setThrottled(on: Boolean): Unit = gates.foreach(_.setThrottled(on))
}
