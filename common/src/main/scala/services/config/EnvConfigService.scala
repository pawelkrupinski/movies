package services.config

import settings.ConfigRefreshInterval

import play.api.Logging
import tools.Stoppable
import tools.{ThreadPools, Env, ScheduledTick, TickRecorder, TickRun}

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._

/**
 * Keeps a process's config knobs in sync with the admin overrides and publishes
 * what it discovered so the `/admin/config` page can list every knob across both
 * apps. Run by BOTH web and worker:
 *
 *  - installs the override cache as the process [[Env]]'s override source, so
 *    every read through that instance sees a flip live (override wins over the
 *    env var);
 *  - on a periodic tick: refreshes the override cache (picks up flips made on the
 *    other process) and republishes this process's non-secret knobs + their
 *    current values to the shared registry.
 *
 * The web process additionally serves the admin views and the operator actions
 * over the same stores ([[EnvConfigAdmin]]). Secrets are excluded everywhere — at
 * publish, at merge, and at write — so a credential can neither be shown nor
 * flipped (see [[EnvKnobClassifier]]).
 */
class EnvConfigService(
  app:            String,
  overrides:      EnvOverrideStore,
  registry:       EnvRegistryStore,
  // The process's config: overrides are installed into it, and its registry of
  // read knobs is what gets published.
  env:            Env,
  tickInterval:   ConfigRefreshInterval = ConfigRefreshInterval(30.seconds),
  // Where each completed publish is reported (`ScheduledTickStale`).
  ticks:          TickRecorder = TickRecorder.Unrecorded,
  threadPools:    ThreadPools
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = threadPools.scheduler("env-config")

  /** Wire the override cache into Env and start the refresh/publish loop. */
  def start(): Unit = {
    env.installOverrides(overrides.lookup)
    ticks.started("env-config-publish", tickInterval.value, scala.concurrent.duration.Duration.Zero)
    scheduler.scheduleWithFixedDelay(
      () => ScheduledTick.recorded("env-config-publish", tickInterval.value, logger, ticks)(publishTick()), 0L, tickInterval.value.toSeconds, TimeUnit.SECONDS)
    logger.info(s"EnvConfigService[$app] started: overrides installed, publishing every ${tickInterval.value.toSeconds}s.")
  }

  /** Refresh overrides from the store, then republish this process's knobs: [[TickRun.Incomplete]] when either store
   *  failed (each logs it) — flips stop arriving, or the admin page stops seeing this process's knobs. */
  private[config] def publishTick(): TickRun = {
    val refreshed = overrides.refresh()
    val knobs = env.knobs.filterNot(k => EnvKnobClassifier.isSecret(k.key)).map { k =>
      RegisteredKnob(app, k.key, k.kind, k.default, env.currentValue(k.key))
    }
    TickRun.completedWhen(registry.publish(app, knobs) && refreshed)
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
