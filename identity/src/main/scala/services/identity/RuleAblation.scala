package services.identity

/** The identity rules a RULE ABLATION run switches off: each named as the identity trace names it (`accept:<rule>`,
 *  `veto:<id of its reason>`, `join:<kind>`, `apart:<id of its reason>`), or a family of them by a trailing `*`
 *  (`veto:vetoed-not-released-in-gb-as-namesake-*`). A rule switched off never accepts, never denies a candidate, and
 *  never makes a must-link or a cannot-link, so the next rule in line decides instead.
 *
 *  Only the ablation harness sets it (`integration.IdentityRuleAblation`, `scripts/identity-rule-ablation`): production
 *  never does, and with nothing switched off every check is one empty-set test. It holds per THREAD, as the resolver
 *  decides on the thread that calls it: specs resolving side by side never see each other's switches. The title rules
 *  need no switch here: the harness hands the resolver a normalizer built without them. */
object RuleAblation {
  private val off: ThreadLocal[Set[String]] = ThreadLocal.withInitial(() => Set.empty[String])

  /** `run` with `rules` switched off on this thread, and every rule back on after it, however it ends. */
  def disabling[A](rules: Set[String])(run: => A): A = {
    off.set(rules)
    try run finally off.remove()
  }

  /** Is the rule the trace names `ruleId` switched off? */
  def disables(ruleId: String): Boolean = {
    val names = off.get
    names.nonEmpty && names.exists(name => if (name.endsWith("*")) ruleId.startsWith(name.dropRight(1)) else name == ruleId)
  }

  /** `denial`, unless the veto it is (`veto:<id of its why>`) is switched off. */
  private[identity] def live(denial: Option[Denial]): Option[Denial] =
    if (denial.isEmpty || off.get.isEmpty) denial else denial.filterNot(d => disables("veto:" + DecisionTrace.id(d.why)))
}
