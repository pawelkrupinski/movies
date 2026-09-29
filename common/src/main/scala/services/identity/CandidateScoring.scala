package services.identity

import services.identity.IdentityMeasures.Measure
import services.movies.ListingConstraints

import scala.collection.mutable

/** How every node scores the candidates it has an evidence path to, family by family
 *  (each family its [[FamilyScope]]), with the corpus-wide facts the scoring reads: which house each banner is,
 *  and which titles the venues list together (`venues.corroborating`). */
private[identity] final class CandidateScoring(val generation: CandidateGeneration, val calibration: IdentityCalibration,
                                               weights: EvidenceWeights, val pins: PinConstraints) {
  import EvidenceNode.placesOf

  /** Which house each listing banner is, learned from how every node's candidates bill its works
   *  (`IdentityMeasures.Houses`): the title relation reads a record of the listing's house as
   *  naming it, and a season production must be of it when it is known. */
  val houses: IdentityMeasures.Houses = generation.context.houses
  /** Each banner's contending houses, as [[houses]] ranked them — for a report reading why a banner is, or is not, a house. */
  def houseRanking: Map[String, Seq[IdentityMeasures.Houses.Contender]] = generation.context.houseRanking
  def namesItsSeasonProduction(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film): Boolean =
    IdentityMeasures.namesSeasonProduction(listing, film) && !IdentityMeasures.billing(listing, film).exists(houses.other)

  /** Does the listing's own evidence rule the film out: a learned cannot-link, its facts'
   *  probability below the certified cut, a season its title names that the film is not of, or
   *  its season's production of its work by ANOTHER house than its banner's (`houses`). */
  def evidenceDenies(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film, measures: Map[String, Measure]): Boolean =
    evidenceDenial(listing, film, measures).isDefined

  /** [[evidenceDenies]], saying which of its reasons rules the film out. */
  def evidenceDenial(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film, measures: Map[String, Measure]): Option[String] =
    ListingConstraints.seasonsApart(listing.seasonYear, IdentityMeasures.filmSeason(film), film.year).map(_.toString)
      .orElse(Option.when(IdentityMeasures.namesSeasonProduction(listing, film) && !namesItsSeasonProduction(listing, film))("another house's season production"))
      .orElse(ListingConstraints.learnedListingFilm(calibration, measures, weights.factsProbability(measures)).map(_.toString))

  /** Does `n`'s title name the film only by a PIECE that is its venue's own name or place — every
   *  listing's, the venue's name or its city's? Kino Twierdza's "TWIERDZA - VINCENT. LEGENDA
   *  OCEANU" bills the venue, not *The Rock*, whose Polish title is "Twierdza"; the Alamo
   *  Drafthouse circuit's "Dismember the Alamo 2026 - Chicago" at its Chicago venue names the
   *  city, not the musical. A title that is the venue's name and nothing more (a year aside) still
   *  names its film, whatever the venue is called. */
  def namesOnlyItsVenue(node: EvidenceNode, candidate: Candidate): Boolean = {
    val pieces = namingPieces(node, candidate)
    // The rest of the title must say something beside the venue: "Charlotte (2021)" at a
    // Charlotte venue is the film, its year only dating it.
    def besideIt(piece: Seq[String]) = node.titleWords.diff(piece).exists(_.exists(Character.isLetter))
    pieces.nonEmpty && pieces.forall(piece => besideIt(piece) && node.listings.forall(listing => places(listing.cinema).exists(_.containsSlice(piece))))
  }

  // Once per (node, film) and per venue for THIS resolve, dropped with it: every node meets every
  // candidate of its family's pool here and in the constraint edges (`ConstraintEdges.namesBeside`).
  private val piecesOf = mutable.HashMap.empty[(String, Int), Set[Seq[String]]]
  private val placesOfVenue = mutable.HashMap.empty[models.Cinema, Seq[Seq[String]]]
  /** The pieces of `node`'s title that name `candidate`'s film ([[IdentityMeasures.namingPieces]]). */
  def namingPieces(node: EvidenceNode, candidate: Candidate): Set[Seq[String]] =
    piecesOf.getOrElseUpdate((node.id, candidate.tmdbId), IdentityMeasures.namingPieces(node.evidence.measured, candidate.film))
  private def places(cinema: models.Cinema): Seq[Seq[String]] = placesOfVenue.getOrElseUpdate(cinema, placesOf(cinema))

  /** Which venues back a listing's film, over the corpus's title groups (`CorpusContext.titleGroups`):
   *  every node's, not a family's — a title a learned decoration wraps (`IdentityMeasures.titleGroups`)
   *  is listed plain by venues whose listings are not title-linked to it, so not in its family. */
  val backing: IdentityMeasures.VenueBacking = generation.context.backing

}
