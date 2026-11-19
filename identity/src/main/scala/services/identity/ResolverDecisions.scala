package services.identity

import services.identity.IdentityMeasures.ListingFilm

/** Each final cluster as a [[ResolverDecision]]: its film, confidence, basis and explanation. */
private[identity] final class ResolverDecisions(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import scoring.generation.nodeById
  import families.bestOf

  private val traces = new DecisionTraces(scoring.generation, families, acceptance)

  def of(cluster: Seq[EvidenceNode], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
         votes: Map[String, ResolverDecisions.Vote], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int]): ResolverDecision = {
    val films = cluster.flatMap(node => filmOf(node.id)).distinct
    require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
    val read  = new ClusterRead(scoring, families, acceptance, cluster, scope, films.headOption, accepted, votes)
    val ids   = cluster.map(_.id).toSet
    val apart = keptApart(edges, ids, clusterIndex)
    ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, read.film, read.confidence, read.basis,
      explanation(read, edges, ids, apart), contradictions = apart, fallback = read.fallback, leaning = read.leaning,
      unanswered = read.unknown, candidate = read.candidate, awaits = read.awaits,
      filmTitled = cluster.flatMap(node => node.listings.filter(agreement.NonFilmEvents.filmTitled(_, scoring.generation.hitsOf(node.id))).map(_.key)).toSet)(
      traces.of(cluster, scope, read.scored, read.film, read.basis, edges, ids))
  }

  /** The decision's explanation, line by line. */
  private def explanation(read: ClusterRead, edges: Seq[ResolverEdge], ids: Set[String], apart: Seq[String]): Seq[String] = {
    import read.cluster
    val own = ownMatches(cluster, read.scope)
    (overruledLines(read) ++ own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ voteLine(read) ++
      joinLines(edges, ids) ++ apart.take(4) ++ bestRejected(read) ++ Option.when(read.unknown > 0)(s"${read.unknown} lookup(s) unanswerable") ++
      Option.when(read.awaited.nonEmpty)(s"${read.awaited.size} record(s) awaited: ${read.awaited.take(5).mkString(", ")}") ++
      read.fallback.map(taken => s"falls back to ${taken.source} ${taken.id} at ${ResolverDecision.percent(taken.probability)}") ++
      read.leaning.map(lean => s"leans to ${lean.film} (tt${lean.imdbNumber})")) :+
      s"node ${cluster.head.listings.head.key}"
  }

  private def overruledLines(read: ClusterRead): Seq[String] =
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

  private def voteLine(read: ClusterRead): Option[String] = {
    val clusterVotes = read.cluster.flatMap(node => read.votes.get(node.id))
    clusterVotes.flatMap(_.majority).headOption.orElse(clusterVotes.headOption.map(v =>
      s"pooled evidence of ${read.cluster.size} node(s) → ${v.film} at ${ResolverDecision.percent(v.probability)}"))
  }

  private def bestRejected(read: ClusterRead): Option[String] =
    read.scored.headOption.filter(scored => !read.film.contains(scored.candidate.tmdbId)).map { scored =>
      val why = scored.denial.orElse(scored.untaken).fold("")(reason => s", ${if (scored.denied) "denied" else "untaken"}: $reason,")
      s"best ${if (scored.denied) "vetoed" else "rejected"} candidate ${scored.candidate.tmdbId} at ${ResolverDecision.percent(scored.probability)}$why (${calibration.explain(ListingFilm, scored.measures)})"
    }

}

private[identity] object ResolverDecisions {
  /** The film a cluster no member matched alone voted for, at `probability` — `majority` saying why when its title
   *  family's clear majority decided it (`ClusterVoting.familyMajority`), its confidence then the family's bound. */
  final case class Vote(film: Int, probability: Double, majority: Option[String])
}
