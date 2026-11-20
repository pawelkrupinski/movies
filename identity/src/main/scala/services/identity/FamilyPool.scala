package services.identity

import scala.collection.mutable

/** A family's pool ([[FamilyScope]]) as its listings' titles relate to it: the candidates, the qualifiers their records
 *  bill, and each title's relation to each candidate, read once. */
private[identity] final class FamilyPool(val candidates: Seq[Candidate], houses: BannerHouses, related: () => Unit) {

  /** Which pieces of the members' titles are qualifiers — an edition, a banner — rather than
   *  works, learned from how the pool's records bill them (`IdentityMeasures.Qualifiers`): a
   *  record titled only a listing's qualifier does not name it. The family's own pool, so a
   *  family resolves alone as it does among the others. */
  val qualifiers: IdentityMeasures.Qualifiers = IdentityMeasures.Qualifiers.learn(candidates.map(_.film))

  // A title relation reads only the listing's titles — its title, raw, original and search titles and its
  // decorations — never its facts, beside the film and this pool's houses and qualifiers. Every node and
  // cluster of the family is related to the whole pool, so the nodes billing one title, and a cluster read
  // under its lead's titles, share the pool's relations instead of reading them again.
  private val relations = mutable.HashMap.empty[(MeasuredListing.TitleInputs, Int), Measure.Category]

  /** How `listing`'s titles relate to `candidate`, once per title and candidate for this pool's life. */
  def titleRelation(listing: MeasuredListing, candidate: Candidate): Measure.Category =
    relations.getOrElseUpdate((listing.titleInputs, candidate.tmdbId), { related(); relate(listing, candidate.film) })

  /** How `listing`'s titles relate to `film` under this pool's houses and qualifiers, read afresh: a listing's titles
   *  alone, or a film's record otherwise, that the shared relations do not hold. */
  def relate(listing: MeasuredListing, film: Film): Measure.Category =
    IdentityMeasures.titleRelation(listing, film, houses, qualifiers)
}
