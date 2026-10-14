package services.identity

import services.movies.ListingKey

/** The shadow diff written the plain way — every decision's members looked up, every film's listings grouped — the
 *  reference [[IdentityProjectionPlan.canary]]'s index-read relations are held to. */
object ShadowDiffOracle {
  /** Each decision's relation to the pipeline's films (None when the pipeline placed none of its listings), and the
   *  pipeline films its listings are on. */
  def relationsOf(decisions: Seq[ResolverDecision], pipelineOf: Map[ListingKey, PipelineFilmRef]): Seq[(ResolverDecision, Option[ShadowRelation], Seq[PipelineFilmRef])] = {
    val membersOf = pipelineOf.toSeq.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
    decisions.map { d =>
      val placed = d.members.filter(pipelineOf.contains)
      val films  = placed.map(pipelineOf).distinct.sortBy(_.id)
      val relation = films match {
        case Seq()  => None
        case Seq(p) =>
          if (membersOf(p) != placed.toSet) Some(ShadowRelation.Split)
          else if (p.tmdbId == d.film) Some(ShadowRelation.Identical)
          else Some(ShadowRelation.Moved)
        case _ => Some(ShadowRelation.Merged)
      }
      (d, relation, films)
    }
  }

  /** Decisions per relation, every relation present (zero when none). */
  def counts(decisions: Seq[ResolverDecision], pipelineOf: Map[ListingKey, PipelineFilmRef]): Map[ShadowRelation, Int] = {
    val relations = relationsOf(decisions, pipelineOf).flatMap(_._2)
    ShadowRelation.values.map(r => r -> relations.count(_ == r)).toMap
  }
}
