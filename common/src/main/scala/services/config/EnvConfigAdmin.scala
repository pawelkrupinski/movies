package services.config

import tools.Env

/** The `/admin/config` page's side of the live config (web only): every non-secret knob every process published
 *  ([[EnvRegistryStore]]) with its active override, and the operator's flips into the override store
 *  ([[EnvOverrideStore]]) that [[EnvConfigService]] carries to every process. Secrets are never listed nor flipped
 *  ([[EnvKnobClassifier]]). */
class EnvConfigAdmin(overrides: EnvOverrideStore, registry: EnvRegistryStore) {
  private def isSecret(key: String): Boolean = EnvKnobClassifier.isSecret(key)

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

}
