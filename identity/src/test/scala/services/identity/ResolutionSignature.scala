package services.identity

import services.movies.ListingKey

/** What two models must agree on to decide alike: every decision (its listings, film, confidence
 *  and basis) and the partition into families. */
object ResolutionSignature {
  type Signature = (Set[(Set[ListingKey], Option[Int], Long, ResolverDecision.Basis)], Set[Set[ListingKey]])

  def of(decisions: Seq[ResolverDecision], familyOf: Map[ListingKey, Int]): Signature =
    (decisions.map(d => (d.listings, d.film, math.round(d.confidence * 1e9), d.basis)).toSet,
     familyOf.groupMap(_._2)(_._1).values.map(_.toSet).toSet)

  def of(resolution: Resolution): Signature = of(resolution.decisions, resolution.familyOf)
  def of(model: IncrementalResolver): Signature = of(model.decisions, model.familyOf)
}
