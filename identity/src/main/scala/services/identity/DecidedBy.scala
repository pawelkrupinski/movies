package services.identity

import services.movies.ListingKey

/**
 * Which stage decided a listing's take, and by which rule — each as ONE short id, what a take-change line
 * ([[TakeChangeLines]]) and the rule-firing count (`adapters.RuleFiringCount`) are filtered and grouped by:
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

  def stage(decision: ResolverDecision, key: ListingKey): String = decision.basis match {
    case Basis.Pinned                                                                   => "pin"
    case Basis.Agreed                                                                   => "agreement"
    case Basis.Poster                                                                   => "poster"
    case Basis.Broadcast                                                                => "broadcast"
    case Basis.Filled                                                                   => "fill"
    case Basis.Catalogue                                                                => "catalogue"
    case Basis.Corrected | Basis.Withdrawn                                              => "correction"
    case Basis.Event                                                                    => "event"
    case Basis.OwnMatch | Basis.PooledMatch | Basis.NoCandidate | Basis.NoEvidence | Basis.Vetoed | Basis.BelowThreshold =>
      if (accepted(decision, Some(key)).exists(RelayRules) || decision.trace.pooled.exists(RelayRules)) "relay"
      else "model"
  }

  /** The rule that decided `key`'s take under `decision`. */
  def of(decision: ResolverDecision, key: ListingKey): String = {
    val trace = decision.trace
    decision.basis match {
      case Basis.Pinned      => "pin"
      case Basis.OwnMatch    => accepted(decision, Some(key)).fold("accept:joined")("accept:" + _)
      case Basis.PooledMatch => "pooled:" + trace.pooled.getOrElse("unnamed")
      case Basis.Vetoed      => "veto:" + trace.vetoed.fold("denied")(v => DecisionTrace.id(v.reason))
      case Basis.NoCandidate | Basis.NoEvidence | Basis.BelowThreshold =>
        trace.nodes.get(key).flatMap(_.blocker).getOrElse("pooled:no-film")
      case Basis.Agreed      => "agreed:" + (if (decision.agreed.isEmpty) "families" else decision.agreed.keys.toSeq.sorted.mkString("+"))
      case Basis.Poster      => "poster:vote"
      case Basis.Broadcast   => "broadcast"
      case Basis.Catalogue   => "catalogue"
      case Basis.Event       => "event"
      case Basis.Filled      => trace.stage.getOrElse("fill:unnamed")
      case Basis.Corrected   => trace.stage.getOrElse("correction:unnamed")
      case Basis.Withdrawn   => trace.stage.getOrElse("withdrawal:unnamed")
    }
  }

  /** The rule a node accepted the film by alone: `key`'s own node's, else the first member's that did. */
  private def accepted(decision: ResolverDecision, key: Option[ListingKey]): Option[String] = {
    val nodes = decision.trace.nodes
    key.flatMap(nodes.get).flatMap(_.accepted).orElse(decision.members.iterator.flatMap(nodes.get).flatMap(_.accepted).nextOption())
  }
}
