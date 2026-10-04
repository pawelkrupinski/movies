package services.tasks

import settings.SettleInterval

import services.schedule.{AlwaysClaimScheduledRunStore, ScheduledRunStore}

import java.time.Clock
import scala.concurrent.duration._

/**
 * Runs the worker's one periodic projection — the identity projection's reconciliation of the whole corpus
 * (`IdentityProjection.ReconcileEvery`), which also reads every archive's stamps and records the slot fingerprints; its
 * first run is the boot's projection. Every other projection runs as the identity model takes the worker's scrapes in
 * (`ProjectionTrigger`).
 *
 * Cluster-safe: a multi-machine worker gates each tick on a window occurrence claim ([[ScheduledRunStore]]) so one
 * machine projects per window.
 */
class SettleReaper(
  settle: () => Unit,
  // The period. BY-NAME so an `/admin/config` flip applies on the next cycle without a restart.
  interval:     => SettleInterval,
  // How long after `start()` the first tick runs: a worker's first projection is of the whole corpus, kept off its boot.
  initialDelay: SettleReaper.InitialDelay,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock
) extends ClaimedPeriodicTask("settle", settle, interval.value, initialDelay.value, runStore, clock)

object SettleReaper {

  /** How long after `start()` the first tick runs. */
  final case class InitialDelay(value: FiniteDuration) extends AnyVal
}
