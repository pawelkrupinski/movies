package services.identity

/** The rules behind a cluster's decision ([[DecisionTrace]]): each node's own accepting rule, joins and cannot-links,
 *  the pooled rule when the pooled scoring decided, and which member's own evidence denied a vetoed film. */
private[identity] final class DecisionTraces(generation: CandidateGeneration, families: Families, acceptance: Acceptance) {
  import generation.{answers, nodeById, queriesOf}
  import families.bestOf

  def of(cluster: Seq[EvidenceNode], scope: FamilyScope, scored: Seq[Scored], film: Option[Int],
         basis: ResolverDecision.Basis, edges: Seq[ResolverEdge], ids: Set[String]): DecisionTrace = {
    val nodes = cluster.flatMap { node =>
      val traced = nodeTrace(node, scope, film, edges, ids)
      node.listings.map(_.key -> traced)
    }.toMap
    DecisionTrace(pooledRule(scored, film, basis), vetoOf(cluster, scope, scored, film), nodes)
  }

  /** One node's part of the trace. Computed here, not when the trace is written: a thunk would keep `own` — every scored
   *  candidate of the node — alive until the trace writer got to it, and a restore hands the whole corpus over at once. */
  private def nodeTrace(node: EvidenceNode, scope: FamilyScope, film: Option[Int], edges: Seq[ResolverEdge],
                        ids: Set[String]): DecisionTrace.Node = {
    val accepted = bestOf.get(node.id).flatMap { case (best, _) => scope.acceptedBy(node, best.candidate.tmdbId) }
    val touching = edges.filter(edge => edge.a == node.id || edge.b == node.id)
    val joins    = touching.filter(edge => edge.must && ids(edge.a) && ids(edge.b)).map(_.reason).distinct.sorted
    val apart    = touching.filter(edge => !edge.must && (ids(edge.a) ^ ids(edge.b))).map(_.reason).distinct.sorted
    val own      = scope.of(node)
    val weighed  = film.flatMap(id => own.find(_.candidate.tmdbId == id)).orElse(own.headOption)
    val withdrawn = if (accepted.isDefined) None else withdrawnOf(node, scope)
    val refusals = if (accepted.isDefined) Nil else withdrawn.toSeq ++ acceptance.refusals(own)
    val searched = if (accepted.isDefined) Nil else searchedBy(node)
    // What it weighed: for a node no rule took, its five best candidates; for one a rule took, the runner-up it beat.
    val shown    = if (accepted.isDefined) own.filterNot(scored => weighed.exists(_ eq scored)).take(1) else own.take(5)
    val blocker  = Option.when(accepted.isEmpty)(withdrawn.map(_ => "withdrawn:sibling-denies-film")
      .getOrElse(DecisionTrace.blockerOf(own, searched.exists(_.endsWith(": unanswered")), refusals)))
    // the best other film it weighed, by probability: what the take beat (or, taking none, its next best)
    val rival    = own.iterator.filterNot(scored => weighed.exists(_ eq scored)).maxByOption(_.probability)
    DecisionTrace.Node(accepted, joins, apart, weighed.fold(Map.empty[String, IdentityMeasures.Measure])(_.measures),
      weighed.map(_.candidate.tmdbId), refusals, searched, shown.map(DecisionTrace.renderCandidate), blocker,
      weighed.map(_.probability), rival.map(scored => DecisionTrace.Rival(scored.candidate.tmdbId, scored.probability)))
  }

  /** A match the node's own rules took, withdrawn because a title-linked sibling's evidence denies that film. */
  private def withdrawnOf(node: EvidenceNode, scope: FamilyScope): Option[DecisionTrace.Refusal] =
    scope.takenAlone(node).flatMap { case ((best, _), _) =>
      families.deniedBySibling(node, scope.members, scope, best, id => scope.takenAlone(nodeById(id)).map(_._1._1))
        .map { case (sibling, denial) => DecisionTrace.Refusal("withdrawn", "a title-linked sibling's own evidence denies its film",
          Some(best.candidate.tmdbId), s"${sibling.label}: $denial") }
    }

  /** What a node no rule took searched: each query with what it found — enough to tell a search that found nothing from
   *  a veto from a rule that would not take it. Strings, like the refusals, so nothing scored is kept. */
  private def searchedBy(node: EvidenceNode): Seq[String] =
    queriesOf(node.id).map(query => s"${DecisionTrace.renderQuery(query)}: " +
      answers.get(query).flatMap(_.answered).fold("unanswered")(hits => s"${hits.size} film(s)"))

  /** The pooled rule, when the pooled scoring decided. */
  private def pooledRule(scored: Seq[Scored], film: Option[Int], basis: ResolverDecision.Basis): Option[String] =
    Option.when(basis == ResolverDecision.Basis.PooledMatch)(acceptance.pooledNamed(scored).collect {
      case ((accepted, _), rule) if film.contains(accepted.candidate.tmdbId) => rule }).flatten

  /** Which member's own evidence denied the vetoed best film, and why. */
  private def vetoOf(cluster: Seq[EvidenceNode], scope: FamilyScope, scored: Seq[Scored], film: Option[Int]): Option[DecisionTrace.Veto] =
    scored.headOption.filter(best => best.denied && !film.contains(best.candidate.tmdbId)).map { best =>
      val by = cluster.find(node => scope.of(node).exists(own => own.candidate.tmdbId == best.candidate.tmdbId && own.denied))
      DecisionTrace.Veto(by.flatMap(node => scope.of(node).find(_.candidate.tmdbId == best.candidate.tmdbId)).flatMap(_.denial)
        .orElse(best.denial).fold("denied")(_.why), by.map(_.name))
    }
}
