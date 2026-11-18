package services.identity

import models.ListingKey

/**
 * Which stage decided a listing's take, and by which rule — each as ONE short id, what a take-change line
 * ([[TakeChangeLines]]) and the rule-firing count (`services.identityadapters.RuleFiringCount`) are filtered and grouped by:
 *
 *  - [[stage]]: `pin`; `model` (the resolver's acceptance rules); `relay` (its stage-relay rules, [[RelayStage]]); and the
 *    agreement stage's takes after the model, `agreement`, `poster`, `broadcast`, `fill`, `catalogue`, `correction`,
 *    `event`;
 *  - [[of]]: the rule within it — `accept:<rule>` (the listing's own node's, else the cluster's), `pooled:<rule>`,
 *    `veto:<reason id>`, the blocker of a listing no rule took (`search:found-nothing`, `rule:<why>`, …), `agreed:<the
 *    families>`, `fill:<rule>`, `correction:<evidence>`, `withdrawal:<evidence>`, `poster:vote`, `broadcast`,
 *    `catalogue`, `event`, `pin`.
 */
object DecidedBy {
  import ResolverDecision.Basis

  /** The acceptance rules of a stage relay ([[AcceptanceStageRules]]): a take by one is the relay stage's. */
  val RelayRules: Set[String] = Set("house-production", "season-record", "relay-record")

  /** The stage of each basis the agreement stage (or a pin) decides; every other is the model's or its relay rules'. */
  private val StageOf: Map[Basis, String] = Map(
    Basis.Pinned -> "pin", Basis.Agreed -> "agreement", Basis.Poster -> "poster", Basis.Broadcast -> "broadcast",
    Basis.Filled -> "fill", Basis.Catalogue -> "catalogue", Basis.Corrected -> "correction", Basis.Withdrawn -> "correction",
    Basis.Event -> "event")

  def stage(decision: ResolverDecision, key: ListingKey): String = StageOf.getOrElse(decision.basis,
    if (accepted(decision, Some(key)).exists(RelayRules) || decision.trace.pooled.exists(RelayRules)) "relay" else "model")

  /** The rule of each basis that names one alone. */
  private val RuleOf: Map[Basis, String] = Map(
    Basis.Pinned -> "pin", Basis.Poster -> "poster:vote", Basis.Broadcast -> "broadcast", Basis.Catalogue -> "catalogue",
    Basis.Event -> "event")
  /** The rule of each basis the stage that took it names in the trace, and what it reads when the stage names none. */
  private val UnnamedOf: Map[Basis, String] = Map(
    Basis.Filled -> "fill:unnamed", Basis.Corrected -> "correction:unnamed", Basis.Withdrawn -> "withdrawal:unnamed")

  /** The rule that decided `key`'s take under `decision`. */
  def of(decision: ResolverDecision, key: ListingKey): String =
    RuleOf.get(decision.basis)
      .orElse(UnnamedOf.get(decision.basis).map(unnamed => decision.trace.stage.getOrElse(unnamed)))
      .getOrElse(weighedRule(decision, key))

  /** The rule of a take the model weighed, or the agreement's families agreed on. */
  private def weighedRule(decision: ResolverDecision, key: ListingKey): String = {
    val trace = decision.trace
    decision.basis match {
      case Basis.OwnMatch    => accepted(decision, Some(key)).fold("accept:joined")("accept:" + _)
      case Basis.PooledMatch => "pooled:" + trace.pooled.getOrElse("unnamed")
      case Basis.Vetoed      => "veto:" + trace.vetoed.fold("denied")(v => DecisionTrace.id(v.reason))
      case Basis.Agreed      => "agreed:" + (if (decision.agreed.isEmpty) "families" else decision.agreed.keys.toSeq.sorted.mkString("+"))
      case _                 => trace.nodes.get(key).flatMap(_.blocker).getOrElse("pooled:no-film")
    }
  }

  /** The rule a node accepted the film by alone: `key`'s own node's, else the first member's that did. */
  private def accepted(decision: ResolverDecision, key: Option[ListingKey]): Option[String] =
    key.flatMap(decision.trace.nodes.get).flatMap(_.accepted).orElse(decision.firstAccepted)
}
