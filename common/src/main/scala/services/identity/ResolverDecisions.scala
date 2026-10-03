package services.identity

import services.identity.IdentityMeasures.ListingFilm

/** Each final cluster as a [[ResolverDecision]]: its film, confidence, basis and explanation. */
private[identity] final class ResolverDecisions(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import scoring.generation.{answers, nodeById, queriesOf}
  import families.{bestOf, pinnedFilm}

  def of(cluster: Seq[EvidenceNode], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
         voted: Map[String, (Int, Double)], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int],
         familyTaken: Map[String, (Int, Double, String)]): ResolverDecision = {
    val films = cluster.flatMap(node => filmOf(node.id)).distinct
    require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
    val film    = films.headOption
    val pinned  = film.isDefined && cluster.exists(node => pinnedFilm.contains(node.id))
    val scored  = scope.pooled(cluster)
    val eligible = scored.filterNot(_.denied)
    // A cluster the title family alone decided is filed at the family's bound when its own
    // scoring rates the film lower.
    val familyBound = film.filter(_ => cluster.forall(node => !accepted.contains(node.id))).flatMap(filmId =>
      cluster.flatMap(node => familyTaken.get(node.id)).filter(_._1 == filmId).map(_._2).minOption)
    val confidence =
      if (pinned) 1.0
      else film.fold(eligible.map(1 - _.probability).product)(filmId => math.max(acceptance.confidenceOf(scored, filmId), familyBound.getOrElse(0.0)))
    val unknown = cluster.flatMap(node => queriesOf(node.id)).distinct.count(query => !answers(query).isKnown)
    val basis =
      if (pinned) ResolverDecision.Basis.Pinned
      else if (film.isDefined && cluster.exists(node => accepted.contains(node.id))) ResolverDecision.Basis.OwnMatch
      else if (film.isDefined) ResolverDecision.Basis.PooledMatch
      else if (scored.isEmpty) (if (unknown > 0) ResolverDecision.Basis.NoEvidence else ResolverDecision.Basis.NoCandidate)
      else if (scored.head.denied) ResolverDecision.Basis.Vetoed
      else ResolverDecision.Basis.BelowThreshold
    val ids   = cluster.map(_.id).toSet
    val own   = cluster.flatMap(node => bestOf.get(node.id).map { case (scored, confidence) =>
      s"${node.label}: own match ${scored.candidate.tmdbId} at ${ResolverDecision.percent(confidence)}${acceptance.liftedBy(scope.of(node), scored, confidence)}" +
        scope.acceptedBy(node, scored.candidate.tmdbId).fold("")(rule => s" by $rule") + " " +
        s"(${calibration.explain(ListingFilm, scored.measures)})" })
    val joins = edges.filter(edge => edge.must && ids(edge.a) && ids(edge.b)).groupBy(_.reason).toSeq.sortBy(_._1)
      .map { case (reason, reasonEdges) => s"joined by ${reason} ×${reasonEdges.size}" }
    val apart = edges.filter(edge => !edge.must && (ids(edge.a) ^ ids(edge.b))).map { edge =>
      val other = if (ids(edge.a)) edge.b else edge.a
      s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${edge.reason}"
    }.distinct.sorted
    val vote  = cluster.flatMap(node => familyTaken.get(node.id).map(_._3)).headOption.orElse(
      cluster.flatMap(node => voted.get(node.id)).headOption.map { case (id, probability) => s"pooled evidence of ${cluster.size} node(s) → $id at ${ResolverDecision.percent(probability)}" })
    val best  = scored.headOption.filter(scored => !film.contains(scored.candidate.tmdbId)).map(scored =>
      s"best ${if (scored.denied) "vetoed" else "rejected"} candidate ${scored.candidate.tmdbId} at ${ResolverDecision.percent(scored.probability)}${scored.denial.fold("")(why => s", denied: $why,")} (${calibration.explain(ListingFilm, scored.measures)})")
    val gaps  = Option.when(unknown > 0)(s"$unknown lookup(s) unanswerable")
    ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, film, confidence, basis,
      (own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ vote ++ joins ++
        apart.take(4) ++ best ++ gaps) :+ s"node ${cluster.head.listings.head.key}", contradictions = apart)(
      traceOf(cluster, scope, scored, film, basis, edges, ids))
  }

  /** The rules behind the decision ([[DecisionTrace]]): each node's own accepting rule, joins and cannot-links,
   *  the pooled rule when the pooled scoring decided, and which member's own evidence denied a vetoed film. */
  private def traceOf(cluster: Seq[EvidenceNode], scope: FamilyScope, scored: Seq[Scored], film: Option[Int],
                      basis: ResolverDecision.Basis, edges: Seq[ResolverEdge], ids: Set[String]): DecisionTrace = {
    val nodes = cluster.flatMap { node =>
      val accepted = bestOf.get(node.id).flatMap { case (best, _) => scope.acceptedBy(node, best.candidate.tmdbId) }
      val joins    = edges.filter(edge => edge.must && (edge.a == node.id || edge.b == node.id) && ids(edge.a) && ids(edge.b)).map(_.reason).distinct.sorted
      val apart    = edges.filter(edge => !edge.must && (edge.a == node.id || edge.b == node.id) && (ids(edge.a) ^ ids(edge.b))).map(_.reason).distinct.sorted
      val own      = scope.of(node)
      val weighed  = film.flatMap(id => own.find(_.candidate.tmdbId == id)).orElse(own.headOption)
      // Computed here, not when the trace is written: a thunk would keep `own` — every scored candidate of the
      // node — alive until the trace writer got to it, and a restore hands the whole corpus over at once.
      // a match the node's own rules took, withdrawn because a title-linked sibling's evidence denies that film
      val withdrawn = if (accepted.isDefined) None else scope.takenAlone(node).flatMap { case ((best, _), _) =>
        families.deniedBySibling(node, scope.members, scope, best.candidate.tmdbId, id => scope.takenAlone(nodeById(id)).isDefined)
          .map { case (sibling, denial) => DecisionTrace.Refusal("withdrawn", "a title-linked sibling's own evidence denies its film",
            Some(best.candidate.tmdbId), s"${sibling.label}: $denial") }
      }
      val refusals = if (accepted.isDefined) Nil else withdrawn.toSeq ++ acceptance.refusals(own)
      // What it searched and what it weighed: for a node no rule took, each query with what it found and its five
      // best candidates — enough to tell a search that found nothing from a veto from a rule that would not take it;
      // for one a rule took, the runner-up it beat. Strings, like the refusals, so nothing scored is kept.
      val searched = if (accepted.isDefined) Nil else queriesOf(node.id).map(query => s"${DecisionTrace.renderQuery(query)}: " +
        answers.get(query).flatMap(_.toOption).fold("unanswered")(hits => s"${hits.size} film(s)"))
      val shown    = if (accepted.isDefined) own.filterNot(scored => weighed.exists(_ eq scored)).take(1) else own.take(5)
      // a listing a model judged no film — a package of shorts, a live event — is blocked by that, not by a rule
      val notAFilm = node.evidence.proposal.filter(_.notAFilm).map(proposal => s"not-a-film:${proposal.category}")
      val blocker  = Option.when(accepted.isEmpty)(notAFilm.orElse(withdrawn.map(_ => "withdrawn:sibling-denies-film"))
        .getOrElse(DecisionTrace.blockerOf(own, searched.exists(_.endsWith(": unanswered")), refusals)))
      val traced   = DecisionTrace.Node(accepted, joins, apart, weighed.fold(Map.empty[String, IdentityMeasures.Measure])(_.measures),
        weighed.map(_.candidate.tmdbId), refusals, searched, shown.map(DecisionTrace.renderCandidate), blocker)
      node.listings.map(_.key -> traced)
    }.toMap
    val pooled = Option.when(basis == ResolverDecision.Basis.PooledMatch)(acceptance.pooledNamed(scored).collect {
      case ((accepted, _), rule) if film.contains(accepted.candidate.tmdbId) => rule }).flatten
    val vetoed = scored.headOption.filter(best => best.denied && !film.contains(best.candidate.tmdbId)).map { best =>
      val by = cluster.find(node => scope.of(node).exists(own => own.candidate.tmdbId == best.candidate.tmdbId && own.denied))
      DecisionTrace.Veto(by.flatMap(node => scope.of(node).find(_.candidate.tmdbId == best.candidate.tmdbId)).flatMap(_.denial)
        .orElse(best.denial).getOrElse("denied"), by.map(_.label))
    }
    DecisionTrace(pooled, vetoed, nodes)
  }
}
