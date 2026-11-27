package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.{Listing, ResolverDecision}
import services.identity.vocabulary.Question

/** The CATALOGUE take of a cluster the families agreed on no film for ([[AgreementTakes.CatalogueTake]]): the film its
 *  listings' own catalogue ids name ([[Catalogue]]), read from `catalogue` through `evidence` ([[AgreementEvidence]])
 *  and the cluster's `candidates`' title picks. */
private[agreement] final class CatalogueTake(evidence: AgreementEvidence, candidates: AgreementCandidates,
                                             catalogue: AgreementSources.CatalogueSource) {
  /** The decision with the film its listings' catalogue ids name taken ([[Catalogue.take]]) — none while a question it
   *  needs is open, each noted to ask. */
  def catalogued(decision: ResolverDecision, listings: Seq[Listing], asked: mutable.Set[Question]): Option[ResolverDecision] =
    Catalogue.take(listings, listings.map(evidence.asStated), catalogue, evidence.families, evidence.tmdbOf, () => candidates.titlePicks(listings),
      asked, evidence.attempted).answered.flatten.map(taken(decision, _))

  /** `decision` with the film the catalogue ids name `taken` — a fallback one where TMDB holds none. */
  private def taken(decision: ResolverDecision, taken: Catalogue.Taken): ResolverDecision =
    taken.film.fold(AgreementTakes.taking(decision)(ResolverDecision.Basis.Catalogue, taken.line,
      fallback = taken.fallback.map { case (source, id) => AgreementTakes.fallbackOn(source, id, taken.title, taken.year) }))(film =>
      AgreementTakes.taking(decision)(ResolverDecision.Basis.Catalogue, taken.line, film = Some(film)))
}
