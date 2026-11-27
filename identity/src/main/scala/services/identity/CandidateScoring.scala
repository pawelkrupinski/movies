package services.identity

import services.identity.measures.{BannerHouses, Candidate, EvidenceNode, Film, Measure, MeasuredListing}
import services.identity.vocabulary.{Denial, PinConstraints}

/** How every node scores the candidates it has an evidence path to, family by family
 *  (each family its [[FamilyScope]]), with the corpus-wide facts the scoring reads: which house each banner is,
 *  and which titles the venues list together (`venues.corroborating`). */
private[identity] final class CandidateScoring(val generation: CandidateGeneration, val calibration: IdentityCalibration,
                                               weights: EvidenceWeights, val pins: PinConstraints) {

  /** Whether the curation's pins deny `node`'s listings the film `id` (`PinConstraints.deniedFilms`, by its first listing). */
  def pinDenies(node: EvidenceNode, id: Int): Boolean = pins.deniedFilms(node.key)(id)
  /** The film the curation's pins give `node`'s listings (`PinConstraints.filmOf`, by its first listing), if any. */
  def pinnedFilm(node: EvidenceNode): Option[Int] = pins.filmOf(node.key)

  /** Which house each listing banner is, learned from how every node's candidates bill its works
   *  (`BannerHouses`): the title relation reads a record of the listing's house as
   *  naming it, and a season production must be of it when it is known. */
  val houses: BannerHouses = generation.context.houses
  /** Each banner's contending houses, as [[houses]] ranked them — for a report reading why a banner is, or is not, a house. */
  def houseRanking: Map[String, Seq[BannerHouses.Contender]] = generation.context.houseRanking
  /** The films the listing's own evidence rules out ([[ListingDenials]]), banners read as [[houses]] reads them. */
  private val denials = new ListingDenials(houses, calibration, weights)
  /** The pieces of a node's title that name a candidate's film ([[NamingPieces]]), read once for this resolve. */
  private val naming = new NamingPieces(generation)

  /** [[ListingDenials.namesItsSeasonProduction]]. */
  def namesItsSeasonProduction(listing: MeasuredListing, film: Film): Boolean = denials.namesItsSeasonProduction(listing, film)
  /** Does the listing's own evidence rule the film out ([[ListingDenials.evidenceDenies]])? */
  def evidenceDenies(listing: MeasuredListing, film: Film, measures: Map[String, Measure], country: Option[String]): Boolean =
    denials.evidenceDenies(listing, film, measures, country)
  /** Which of its reasons the listing's own evidence rules the film out by ([[ListingDenials.evidenceDenial]]). */
  def evidenceDenial(listing: MeasuredListing, film: Film, measures: Map[String, Measure], country: Option[String]): Option[Denial] =
    denials.evidenceDenial(listing, film, measures, country)

  /** [[NamingPieces.namesOnlyItsVenue]]. */
  def namesOnlyItsVenue(node: EvidenceNode, candidate: Candidate): Boolean = naming.namesOnlyItsVenue(node, candidate)
  /** [[NamingPieces.namesOnlyATag]]. */
  def namesOnlyATag(node: EvidenceNode, candidate: Candidate): Boolean = naming.namesOnlyATag(node, candidate)
  /** [[NamingPieces.namesOnlyAProgrammeSlot]]. */
  def namesOnlyAProgrammeSlot(node: EvidenceNode, candidate: Candidate): Boolean = naming.namesOnlyAProgrammeSlot(node, candidate)
  /** The pieces of `node`'s title that name `candidate`'s film ([[NamingPieces.namingPieces]]). */
  def namingPieces(node: EvidenceNode, candidate: Candidate): Set[Seq[String]] = naming.namingPieces(node, candidate)

  /** Which venues back a listing's film, over the corpus's title groups (`CorpusContext.titleGroups`):
   *  every node's, not a family's — a title a learned decoration wraps (`IdentityMeasures.titleGroups`)
   *  is listed plain by venues whose listings are not title-linked to it, so not in its family. */
  val backing: VenueBacking = generation.context.backing

}
