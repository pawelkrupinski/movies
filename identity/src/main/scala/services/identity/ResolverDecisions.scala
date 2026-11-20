package services.identity

import services.identity.measures.Measure
import services.identity.measures.IdentityMeasures.ListingFilm
import services.identity.vocabulary.ResolverEdge

/** Each final cluster as a [[ResolverDecision]]: its film, confidence, basis and explanation. */
private[identity] final class ResolverDecisions(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import scoring.generation.{filmsFound, nodeById, queriesOf}
  import families.bestOf

  def of(cluster: Seq[EvidenceNode], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
         votes: Map[String, ResolverDecisions.Vote], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int]): ResolverDecision = {
    val films = cluster.flatMap(node => filmOf(node.id)).distinct
    require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
    val read  = new ClusterDecisionFacts(scoring, families, acceptance, cluster, scope, films.headOption, accepted, votes)
    val ids   = cluster.map(_.id).toSet
    val apart = keptApart(edges, ids, clusterIndex)
    ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, read.film, read.confidence, read.basis,
      explanation(read, edges, ids, apart), contradictions = apart, fallback = read.fallback, leaning = read.leaning,
      unanswered = read.unknown, candidate = read.candidate, awaits = read.awaits,
      filmTitled = cluster.flatMap(node => node.listings.filter(NonFilmEvents.filmTitled(_, scoring.generation.hitsOf(node.id))).map(_.key)).toSet)(
      traceOf(cluster, scope, read.scored, read.film, read.basis, edges, ids))
  }

  /** The decision's explanation, line by line. */
  private def explanation(read: ClusterDecisionFacts, edges: Seq[ResolverEdge], ids: Set[String], apart: Seq[String]): Seq[String] = {
    import read.cluster
    val own = ownMatches(cluster, read.scope)
    (overruledLines(read) ++ own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ voteLine(read) ++
      joinLines(edges, ids) ++ apart.take(4) ++ bestRejected(read) ++ Option.when(read.unknown > 0)(s"${read.unknown} lookup(s) unanswerable") ++
      Option.when(read.awaited.nonEmpty)(s"${read.awaited.size} record(s) awaited: ${read.awaited.take(5).mkString(", ")}") ++
      read.fallback.map(taken => s"falls back to ${taken.source} ${taken.id} at ${ResolverDecision.percent(taken.probability)}") ++
      read.leaning.map(lean => s"leans to ${lean.film} (tt${lean.imdbNumber})")) :+
      s"node ${cluster.head.listings.head.key}"
  }

  private def overruledLines(read: ClusterDecisionFacts): Seq[String] =
    read.cluster.flatMap(node => read.scope.overruled(node).map(link => s"${node.label}: ${link.explanation}"))

  private def ownMatches(cluster: Seq[EvidenceNode], scope: FamilyScope): Seq[String] =
    cluster.flatMap(node => bestOf.get(node.id).map { case (scored, confidence) =>
      s"${node.label}: own match ${scored.candidate.tmdbId} at ${ResolverDecision.percent(confidence)}${acceptance.liftedBy(scope.of(node), scored, confidence)}" +
        scope.acceptedBy(node, scored.candidate.tmdbId).fold("")(rule => s" by $rule") + " " +
        s"(${calibration.explain(ListingFilm, scored.measures)})" })

  private def joinLines(edges: Seq[ResolverEdge], ids: Set[String]): Seq[String] =
    edges.filter(edge => edge.must && ids(edge.a) && ids(edge.b)).groupBy(_.reason).toSeq.sortBy(_._1)
      .map { case (reason, reasonEdges) => s"joined by ${reason} ×${reasonEdges.size}" }

  private def keptApart(edges: Seq[ResolverEdge], ids: Set[String], clusterIndex: Map[String, Int]): Seq[String] =
    edges.filter(edge => !edge.must && (ids(edge.a) ^ ids(edge.b))).map { edge =>
      val other = if (ids(edge.a)) edge.b else edge.a
      s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${edge.reason}"
    }.distinct.sorted

  private def voteLine(read: ClusterDecisionFacts): Option[String] = {
    val clusterVotes = read.cluster.flatMap(node => read.votes.get(node.id))
    clusterVotes.flatMap(_.majority).headOption.orElse(clusterVotes.headOption.map(v =>
      s"pooled evidence of ${read.cluster.size} node(s) → ${v.film} at ${ResolverDecision.percent(v.probability)}"))
  }

  private def bestRejected(read: ClusterDecisionFacts): Option[String] =
    read.scored.headOption.filter(scored => !read.film.contains(scored.candidate.tmdbId)).map { scored =>
      val why = scored.denial.orElse(scored.untaken).fold("")(reason => s", ${if (scored.denied) "denied" else "untaken"}: $reason,")
      s"best ${if (scored.denied) "vetoed" else "rejected"} candidate ${scored.candidate.tmdbId} at ${ResolverDecision.percent(scored.probability)}$why (${calibration.explain(ListingFilm, scored.measures)})"
    }

  /** The rules behind a cluster's decision ([[DecisionTrace]]): each node's own accepting rule, joins and cannot-links,
   *  the pooled rule when the pooled scoring decided, and which member's own evidence denied a vetoed film. */
  private def traceOf(cluster: Seq[EvidenceNode], scope: FamilyScope, scored: Seq[Scored], film: Option[Int],
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
    // A node a rule took is traced by its take; one none took, by what withheld a take: a sibling's withdrawal, the
    // refusals, the searches.
    val withheld: (Option[DecisionTrace.Refusal], Seq[DecisionTrace.Refusal], Seq[String]) =
      if (accepted.isDefined) (None, Nil, Nil)
      else {
        val withdrawn = withdrawnOf(node, scope)
        (withdrawn, withdrawn.toSeq ++ acceptance.refusals(own), searchedBy(node))
      }
    val (withdrawn, refusals, searched) = withheld
    // What it weighed: for a node no rule took, its five best candidates; for one a rule took, the runner-up it beat.
    val shown    = if (accepted.isDefined) own.filterNot(scored => weighed.exists(_ eq scored)).take(1) else own.take(5)
    val blocker  = Option.when(accepted.isEmpty)(withdrawn.map(_ => "withdrawn:sibling-denies-film")
      .getOrElse(DecisionTrace.blockerOf(own, searched.exists(_.endsWith(": unanswered")), refusals)))
    // the best other film it weighed, by probability: what the take beat (or, taking none, its next best)
    val rival    = own.iterator.filterNot(scored => weighed.exists(_ eq scored)).maxByOption(_.probability)
    DecisionTrace.Node(accepted, joins, apart, weighed.fold(Map.empty[String, Measure])(_.measures),
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
      filmsFound(query).fold("unanswered")(films => s"$films film(s)"))

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

private[identity] object ResolverDecisions {
  /** The film a cluster no member matched alone voted for, at `probability` — `majority` saying why when its title
   *  family's clear majority decided it (`ClusterVoting.familyMajority`), its confidence then the family's bound. */
  final case class Vote(film: Int, probability: Double, majority: Option[String])
}
