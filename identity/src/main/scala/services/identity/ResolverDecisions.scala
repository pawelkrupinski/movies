package services.identity

import services.identity.IdentityMeasures.ListingFilm

/** Each final cluster as a [[ResolverDecision]]: its film, confidence, basis and explanation. */
private[identity] final class ResolverDecisions(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import scoring.generation.{answers, nodeById, queriesOf}
  import families.{bestOf, pinnedFilm}

  def of(cluster: Seq[EvidenceNode], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
         votes: Map[String, ResolverDecisions.Vote], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int]): ResolverDecision = {
    val films = cluster.flatMap(node => filmOf(node.id)).distinct
    require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
    val film    = films.headOption
    val pinned  = film.isDefined && cluster.exists(node => pinnedFilm.contains(node.id))
    val scored  = scope.pooled(cluster)
    val eligible = scored.filterNot(_.denied)
    // A cluster the title family alone decided is filed at the family's bound when its own
    // scoring rates the film lower.
    val familyBound = film.filter(_ => cluster.forall(node => !accepted.contains(node.id))).flatMap(filmId =>
      cluster.flatMap(node => votes.get(node.id)).filter(vote => vote.majority.isDefined && vote.film == filmId).map(_.probability).minOption)
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
    val overruled = cluster.flatMap(node => scope.overruled(node).map(link => s"${node.label}: ${link.explanation}"))
    val joins = edges.filter(edge => edge.must && ids(edge.a) && ids(edge.b)).groupBy(_.reason).toSeq.sortBy(_._1)
      .map { case (reason, reasonEdges) => s"joined by ${reason} ×${reasonEdges.size}" }
    val apart = edges.filter(edge => !edge.must && (ids(edge.a) ^ ids(edge.b))).map { edge =>
      val other = if (ids(edge.a)) edge.b else edge.a
      s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${edge.reason}"
    }.distinct.sorted
    val clusterVotes = cluster.flatMap(node => votes.get(node.id))
    val vote  = clusterVotes.flatMap(_.majority).headOption.orElse(clusterVotes.headOption.map(v =>
      s"pooled evidence of ${cluster.size} node(s) → ${v.film} at ${ResolverDecision.percent(v.probability)}"))
    val best  = scored.headOption.filter(scored => !film.contains(scored.candidate.tmdbId)).map(scored =>
      s"best ${if (scored.denied) "vetoed" else "rejected"} candidate ${scored.candidate.tmdbId} at ${ResolverDecision.percent(scored.probability)}${scored.denial.orElse(scored.untaken).fold("")(why => s", ${if (scored.denied) "denied" else "untaken"}: $why,")} (${calibration.explain(ListingFilm, scored.measures)})")
    val gaps  = Option.when(unknown > 0)(s"$unknown lookup(s) unanswerable")
    // a cluster no TMDB film was taken for, every question answered: the one fallback film its members take
    val fallback = if (film.isDefined || unknown > 0) None else fallbackOf(cluster, scope)
    // a film a reused page's copied original title and director alone name ([[Acceptance.copiedBeyondItsBilling]]): facts
    // that agree with it by construction, so no vote of TMDB's for it (DE Scheune Dresden's "Regio Tales", an event on
    // kinoheld's "Heads and Tales" page)
    def copiedOnly(tmdbId: Int) = eligible.exists(s => s.candidate.tmdbId == tmdbId && acceptance.copiedBeyondItsBilling(s))
    // the one TMDB film the members lean to, none of them denying it
    val leaned  = if (film.isDefined) Nil else cluster.flatMap(node => acceptance.leaning(scope.of(node))).map(_.candidate).distinctBy(_.tmdbId)
    val leaning = leaned match {
      case Seq(lean) if lean.film.imdbNumber > 0 && !cluster.exists(node => families.denies(node, lean.tmdbId)) && !copiedOnly(lean.tmdbId) =>
        Some(ResolverDecision.Leaning(lean.tmdbId, lean.film.imdbNumber))
      case _ => None
    }
    // the best-ranked TMDB film the pooled evidence weighed that no member denies: the agreement stage's TMDB voter — none
    // where the release veto took out a film ranked above it, as the veto adds no evidence for its namesake
    // ([[ReleaseVeto]]: PL "Stan wyjątkowy", Brešan's 2018 film best only once Hřebejk's 2024 one was vetoed), nor a
    // reused page's copied record
    val candidate = if (film.isDefined || scored.takeWhile(_.denied).exists(_.denial.exists(ReleaseVeto.vetoes))) None
      else eligible.headOption.filter(best => best.untaken.isEmpty && !copiedOnly(best.candidate.tmdbId)).map(_.candidate)
      .filterNot(best => cluster.exists(node => families.denies(node, best.tmdbId)))
      .map(best => ResolverDecision.Leaning(best.tmdbId, best.film.imdbNumber))
    ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, film, confidence, basis,
      (overruled ++ own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ vote ++ joins ++
        apart.take(4) ++ best ++ gaps ++ fallback.map(taken => s"falls back to ${taken.source} ${taken.id} at ${ResolverDecision.percent(taken.probability)}") ++
        leaning.map(lean => s"leans to ${lean.film} (tt${lean.imdbNumber})")) :+
        s"node ${cluster.head.listings.head.key}", contradictions = apart, fallback = fallback, leaning = leaning, unanswered = unknown,
      candidate = candidate)(
      traceOf(cluster, scope, scored, film, basis, edges, ids))
  }

  /** The fallback film a cluster no TMDB film was taken for falls back to: the one every member that takes one takes
   *  ([[Acceptance.fallback]], over the family's fallback pool), denied by no member's evidence — and none while a TMDB
   *  film the title names, or crediting the listing's director, stood undenied: TMDB then knows a film the listing may
   *  be, and its rules not taking it is no licence to take another source's in its place. UK Cineworld's "Royal Ballet
   *  and Opera: Tosca" ×123 fell back to IMDb's record of the 2025/26 production, which TMDB holds too, unlinked. */
  private def fallbackOf(cluster: Seq[EvidenceNode], scope: FamilyScope): Option[ResolverDecision.Fallback] = {
    val fallbacks = new FamilyScope(scope.members, scoring, acceptance, () => (), fallback = true)
    lazy val tmdbNamesake = cluster.exists(node => scope.of(node).exists(scored =>
      !scored.denied && (scored.titleNamesIt || IdentityMeasures.sameDirector(scored.measures))))
    if (fallbacks.pool.isEmpty) None
    else cluster.flatMap(node => acceptance.fallback(fallbacks.of(node))).distinctBy(_.candidate.tmdbId) match {
      case Seq(taken) if !tmdbNamesake && !cluster.exists(node => families.denies(node, taken.candidate.tmdbId)) =>
        FallbackIds.unapply(taken.candidate.tmdbId).flatMap { case (source, _) =>
          FallbackIds.imdbId(taken.candidate.tmdbId).map(ResolverDecision.Fallback(source.label, _, taken.probability)) }
      case _ => None
    }
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
        families.deniedBySibling(node, scope.members, scope, best, id => scope.takenAlone(nodeById(id)).isDefined)
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
      val blocker  = Option.when(accepted.isEmpty)(withdrawn.map(_ => "withdrawn:sibling-denies-film")
        .getOrElse(DecisionTrace.blockerOf(own, searched.exists(_.endsWith(": unanswered")), refusals)))
      // the best other film it weighed, by probability: what the take beat (or, taking none, its next best)
      val rival    = own.iterator.filterNot(scored => weighed.exists(_ eq scored)).maxByOption(_.probability)
      val traced   = DecisionTrace.Node(accepted, joins, apart, weighed.fold(Map.empty[String, IdentityMeasures.Measure])(_.measures),
        weighed.map(_.candidate.tmdbId), refusals, searched, shown.map(DecisionTrace.renderCandidate), blocker,
        weighed.map(_.probability), rival.map(scored => DecisionTrace.Rival(scored.candidate.tmdbId, scored.probability)))
      node.listings.map(_.key -> traced)
    }.toMap
    val pooled = Option.when(basis == ResolverDecision.Basis.PooledMatch)(acceptance.pooledNamed(scored).collect {
      case ((accepted, _), rule) if film.contains(accepted.candidate.tmdbId) => rule }).flatten
    val vetoed = scored.headOption.filter(best => best.denied && !film.contains(best.candidate.tmdbId)).map { best =>
      val by = cluster.find(node => scope.of(node).exists(own => own.candidate.tmdbId == best.candidate.tmdbId && own.denied))
      DecisionTrace.Veto(by.flatMap(node => scope.of(node).find(_.candidate.tmdbId == best.candidate.tmdbId)).flatMap(_.denial)
        .orElse(best.denial).fold("denied")(_.why), by.map(_.name))
    }
    DecisionTrace(pooled, vetoed, nodes)
  }
}

private[identity] object ResolverDecisions {
  /** The film a cluster no member matched alone voted for, at `probability` — `majority` saying why when its title
   *  family's clear majority decided it (`ClusterVoting.familyMajority`), its confidence then the family's bound. */
  final case class Vote(film: Int, probability: Double, majority: Option[String])
}
