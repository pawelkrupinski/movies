package services.tasks

import settings.SettleInterval

import services.schedule.{AlwaysClaimScheduledRunStore, ScheduledRunStore}

import java.time.Clock
import scala.concurrent.duration._

/**
 * Runs the worker's periodic tick — the identity projection (`IdentityCutoverWiring`) — once per `interval`: the
 * projection that reads every archive's stamps, records the slot fingerprints and, every
 * `IdentityProjection.ScopedBetweenWhole` of them, reconciles the whole corpus. Between two, the projection also runs as the
 * identity model takes this worker's scrapes in (`ProjectionTrigger`).
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
