package services.identity

import services.identity.IdentityMeasures.Measure
import services.movies.ListingConstraints

/** How every node scores the candidates it has an evidence path to, family by family
 *  (each family its [[FamilyScope]]), with the corpus-wide facts the scoring reads: which house each banner is,
 *  and which titles the venues list together (`venues.corroborating`). */
private[identity] final class CandidateScoring(val generation: CandidateGeneration, val calibration: IdentityCalibration,
                                               weights: EvidenceWeights, val pins: PinConstraints) {
  import generation.{candidateById, nodes, ownSearch, ownWalk}
  import EvidenceNode.placesOf

  /** Which house each listing banner is, learned from how every node's candidates bill its works
   *  (`IdentityMeasures.Houses`): the title relation reads a record of the listing's house as
   *  naming it, and a season production must be of it when it is known. */
  val houses: IdentityMeasures.Houses = IdentityMeasures.Houses.learn(nodes.flatMap { node =>
    IdentityMeasures.Houses.evidence(node.evidence.measured, (ownSearch(node.id).keys ++ ownWalk(node.id)).toSeq.distinct.sorted.map(candidateById(_).film))
  })
  def namesItsSeasonProduction(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film): Boolean =
    IdentityMeasures.namesSeasonProduction(listing, film) && !IdentityMeasures.billing(listing, film).exists(houses.other)

  /** Does the listing's own evidence rule the film out: a learned cannot-link, its facts'
   *  probability below the certified cut, a season its title names that the film is not of, or
   *  its season's production of its work by ANOTHER house than its banner's (`houses`). */
  def evidenceDenies(listing: IdentityMeasures.Listing, film: IdentityMeasures.Film, measures: Map[String, Measure]): Boolean = {
    ListingConstraints.seasonsApart(listing.seasonYear, IdentityMeasures.filmSeason(film), film.year).isDefined ||
      (IdentityMeasures.namesSeasonProduction(listing, film) && !namesItsSeasonProduction(listing, film)) ||
      ListingConstraints.learnedListingFilm(calibration, measures, weights.factsProbability(measures)).isDefined
  }

  /** Does `n`'s title name the film only by a PIECE that is its venue's own name or place — every
   *  listing's, the venue's name or its city's? Kino Twierdza's "TWIERDZA - VINCENT. LEGENDA
   *  OCEANU" bills the venue, not *The Rock*, whose Polish title is "Twierdza"; the Alamo
   *  Drafthouse circuit's "Dismember the Alamo 2026 - Chicago" at its Chicago venue names the
   *  city, not the musical. A title that is the venue's name and nothing more (a year aside) still
   *  names its film, whatever the venue is called. */
  def namesOnlyItsVenue(node: EvidenceNode, film: IdentityMeasures.Film): Boolean = {
    val whole  = services.movies.TitleContainment.tokens(node.evidence.title)
    val pieces = IdentityMeasures.namingPieces(node.evidence.measured, film)
    // The rest of the title must say something beside the venue: "Charlotte (2021)" at a
    // Charlotte venue is the film, its year only dating it.
    def besideIt(piece: Seq[String]) = whole.diff(piece).exists(_.exists(Character.isLetter))
    pieces.nonEmpty && pieces.forall(piece => besideIt(piece) && node.listings.forall(listing => placesOf(listing.cinema).exists(_.containsSlice(piece))))
  }

  /** The listings by title key, with their venues: `venues.corroborating`'s groups. Every node's,
   *  not a family's: a title a learned decoration wraps (`IdentityMeasures.titleGroups`) is listed
   *  plain by venues whose listings are not title-linked to it, so not in its family. */
  val titleGroups: Map[String, Seq[(String, IdentityMeasures.Listing)]] =
    nodes.flatMap(node => node.listings.map(listing => IdentityMeasures.key(node.evidence.title) -> (listing.venue -> node.evidence.measured)))
      .groupMap(_._1)(_._2)
  val backing = new IdentityMeasures.VenueBacking(titleGroups)

}
