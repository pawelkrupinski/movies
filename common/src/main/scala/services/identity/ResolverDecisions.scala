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
    val films = cluster.flatMap(n => filmOf(n.id)).distinct
    require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
    val film    = films.headOption
    val pinned  = film.isDefined && cluster.exists(n => pinnedFilm.contains(n.id))
    val scored  = scope.pooled(cluster)
    val eligible = scored.filterNot(_.denied)
    // A cluster the title family alone decided is filed at the family's bound when its own
    // scoring rates the film lower.
    val familyBound = film.filter(_ => cluster.forall(n => !accepted.contains(n.id))).flatMap(f =>
      cluster.flatMap(n => familyTaken.get(n.id)).filter(_._1 == f).map(_._2).minOption)
    val confidence =
      if (pinned) 1.0
      else film.fold(eligible.map(1 - _.p).product)(f => math.max(acceptance.confidenceOf(scored, f), familyBound.getOrElse(0.0)))
    val unknown = cluster.flatMap(n => queriesOf(n.id)).distinct.count(q => !answers(q).isKnown)
    val basis =
      if (pinned) ResolverDecision.Basis.Pinned
      else if (film.isDefined && cluster.exists(n => accepted.contains(n.id))) ResolverDecision.Basis.OwnMatch
      else if (film.isDefined) ResolverDecision.Basis.PooledMatch
      else if (scored.isEmpty) (if (unknown > 0) ResolverDecision.Basis.NoEvidence else ResolverDecision.Basis.NoCandidate)
      else if (scored.head.denied) ResolverDecision.Basis.Vetoed
      else ResolverDecision.Basis.BelowThreshold
    val ids   = cluster.map(_.id).toSet
    val own   = cluster.flatMap(n => bestOf.get(n.id).map { case (s, c) =>
      s"${n.label}: own match ${s.c.tmdbId} at ${ResolverDecision.percent(c)}${acceptance.liftedBy(scope.of(n), s, c)} " +
        s"(${calibration.explain(ListingFilm, s.measures)})" })
    val joins = edges.filter(e => e.must && ids(e.a) && ids(e.b)).groupBy(_.reason).toSeq.sortBy(_._1)
      .map { case (r, es) => s"joined by $r ×${es.size}" }
    val apart = edges.filter(e => !e.must && (ids(e.a) ^ ids(e.b))).map { e =>
      val other = if (ids(e.a)) e.b else e.a
      s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${e.reason}"
    }.distinct.sorted
    val vote  = cluster.flatMap(n => familyTaken.get(n.id).map(_._3)).headOption.orElse(
      cluster.flatMap(n => voted.get(n.id)).headOption.map { case (id, p) => s"pooled evidence of ${cluster.size} node(s) → $id at ${ResolverDecision.percent(p)}" })
    val best  = scored.headOption.filter(s => !film.contains(s.c.tmdbId)).map(s =>
      s"best ${if (s.denied) "vetoed" else "rejected"} candidate ${s.c.tmdbId} at ${ResolverDecision.percent(s.p)} (${calibration.explain(ListingFilm, s.measures)})")
    val gaps  = Option.when(unknown > 0)(s"$unknown lookup(s) unanswerable")
    ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, film, confidence, basis,
      (own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ vote ++ joins ++
        apart.take(4) ++ best ++ gaps) :+ s"node ${cluster.head.listings.head.key}", contradictions = apart)
  }
}
