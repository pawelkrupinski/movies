package services.config

import play.api.Logging
import services.Stoppable
import tools.{DaemonExecutors, Env}

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Keeps a process's config knobs in sync with the admin overrides and publishes
 * what it discovered so the `/admin/config` page can list every knob across both
 * apps. Run by BOTH web and worker:
 *
 *  - installs the override cache as [[Env]]'s override source, so every Env read
 *    sees a flip live (override wins over the env var);
 *  - on a periodic tick: refreshes the override cache (picks up flips made on the
 *    other process) and republishes this process's non-secret knobs + their
 *    current values to the shared registry.
 *
 * The web process additionally serves the admin views ([[rows]]) and the
 * operator actions ([[set]] / [[reset]]). Secrets are excluded everywhere — at
 * publish, at merge, and at write — so a credential can neither be shown nor
 * flipped (see [[EnvKnobClassifier]]).
 */
class EnvConfigService(
  app:            String,
  overrides:      EnvOverrideStore,
  registry:       EnvRegistryStore,
  tickInterval:   FiniteDuration = 30.seconds,
  isSecret:       String => Boolean = EnvKnobClassifier.isSecret,
  // Injectable so the publish logic is unit-testable without Env's global,
  // process-wide registry; production uses the real Env reads.
  knobSource:     () => Seq[Env.Knob] = () => Env.knobs,
  currentValueOf: String => Option[String] = Env.currentValue
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("env-config")

  /** Wire the override cache into Env and start the refresh/publish loop. */
  def start(): Unit = {
    Env.installOverrides(overrides.lookup)
    scheduler.scheduleWithFixedDelay(
      () => Try(publishTick()), 0L, tickInterval.toSeconds, TimeUnit.SECONDS)
    logger.info(s"EnvConfigService[$app] started: overrides installed, publishing every ${tickInterval.toSeconds}s.")
  }

  /** Refresh overrides from the store, then republish this process's knobs. */
  private[config] def publishTick(): Unit = {
    overrides.refresh()
    val knobs = knobSource().filterNot(k => isSecret(k.key)).map { k =>
      RegisteredKnob(app, k.key, k.kind, k.default, currentValueOf(k.key))
    }
    registry.publish(app, knobs)
  }

  // ── admin (web) ──────────────────────────────────────────────────────────────
  /** Every non-secret knob, merged across apps, with its active override —
   *  sorted by key for a stable page. */
  def rows(): Seq[EnvConfigRow] = {
    val overrideMap = overrides.all()
    registry.all()
      .filterNot(k => isSecret(k.key))
      .groupBy(_.key)
      .toVector
      .sortBy(_._1)
      .map { case (key, knobs) =>
        val sorted = knobs.sortBy(_.app)
        EnvConfigRow(
          key           = key,
          kind          = sorted.head.kind,
          default       = sorted.flatMap(_.default).headOption,
          apps          = sorted.map(k => AppValue(k.app, k.current)),
          overrideValue = overrideMap.get(key)
        )
      }
  }

  /** Apply an override. Rejected (returns false) for an unknown or secret key, or
   *  for a value that doesn't parse for the knob's numeric type — so the page
   *  can't poke a non-numeric value into an Int/Long knob or flip a credential. */
  def set(key: String, value: String): Boolean =
    knobKind(key) match {
      case None => false
      case Some(kind) =>
        if (!validValue(kind, value)) false
        else { overrides.set(key, value.trim); true }
    }

  /** Remove an override ("reset to default / env"). Returns false for a secret /
   *  unknown key. Idempotent for a key with no override. */
  def reset(key: String): Boolean =
    if (knobKind(key).isEmpty) false else { overrides.clear(key); true }

  /** The registered kind for a known, non-secret key. */
  private def knobKind(key: String): Option[Env.Kind] =
    if (isSecret(key)) None
    else registry.all().find(_.key == key).map(_.kind)

  private def validValue(kind: Env.Kind, value: String): Boolean = kind match {
    case Env.Kind.Int  => value.trim.toIntOption.isDefined
    case Env.Kind.Long => value.trim.toLongOption.isDefined
    case Env.Kind.Str  => value.nonEmpty
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
