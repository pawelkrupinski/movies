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
  val houses: IdentityMeasures.Houses = IdentityMeasures.Houses.learn(nodes.flatMap { n =>
    IdentityMeasures.Houses.evidence(n.evidence.measured, (ownSearch(n.id).keys ++ ownWalk(n.id)).toSeq.distinct.sorted.map(candidateById(_).film))
  })
  def namesItsSeasonProduction(l: IdentityMeasures.Listing, f: IdentityMeasures.Film): Boolean =
    IdentityMeasures.namesSeasonProduction(l, f) && !IdentityMeasures.billing(l, f).exists(houses.other)

  /** Does the listing's own evidence rule the film out: a learned cannot-link, its facts'
   *  probability below the certified cut, a season its title names that the film is not of, or
   *  its season's production of its work by ANOTHER house than its banner's (`houses`). */
  def evidenceDenies(l: IdentityMeasures.Listing, f: IdentityMeasures.Film, measures: Map[String, Measure]): Boolean = {
    ListingConstraints.seasonsApart(l.seasonYear, IdentityMeasures.filmSeason(f), f.year).isDefined ||
      (IdentityMeasures.namesSeasonProduction(l, f) && !namesItsSeasonProduction(l, f)) ||
      ListingConstraints.learnedListingFilm(calibration, measures, weights.factsProbability(measures)).isDefined
  }

  /** Does `n`'s title name the film only by a PIECE that is its venue's own name or place — every
   *  listing's, the venue's name or its city's? Kino Twierdza's "TWIERDZA - VINCENT. LEGENDA
   *  OCEANU" bills the venue, not *The Rock*, whose Polish title is "Twierdza"; the Alamo
   *  Drafthouse circuit's "Dismember the Alamo 2026 - Chicago" at its Chicago venue names the
   *  city, not the musical. A title that is the venue's name and nothing more (a year aside) still
   *  names its film, whatever the venue is called. */
  def namesOnlyItsVenue(n: EvidenceNode, f: IdentityMeasures.Film): Boolean = {
    val whole  = services.movies.TitleContainment.tokens(n.evidence.title)
    val pieces = IdentityMeasures.namingPieces(n.evidence.measured, f)
    // The rest of the title must say something beside the venue: "Charlotte (2021)" at a
    // Charlotte venue is the film, its year only dating it.
    def besideIt(p: Seq[String]) = whole.diff(p).exists(_.exists(Character.isLetter))
    pieces.nonEmpty && pieces.forall(p => besideIt(p) && n.listings.forall(l => placesOf(l.cinema).exists(_.containsSlice(p))))
  }

  /** The listings by title key, with their venues: `venues.corroborating`'s groups. Every node's,
   *  not a family's: a title a learned decoration wraps (`IdentityMeasures.titleGroups`) is listed
   *  plain by venues whose listings are not title-linked to it, so not in its family. */
  val titleGroups: Map[String, Seq[(String, IdentityMeasures.Listing)]] =
    nodes.flatMap(n => n.listings.map(l => IdentityMeasures.key(n.evidence.title) -> (l.venue -> n.evidence.measured)))
      .groupMap(_._1)(_._2)
  val backing = new IdentityMeasures.VenueBacking(titleGroups)

}
