package services.identity

import scala.collection.mutable

import services.identity.measures.{Candidate, EvidenceNode, IdentityMeasures}
import services.titles.billing.BillingParse

/** The pieces of a node's title that name a candidate's film ([[namingPieces]]), and whether they name it only by what is
 *  no film's title: the venue's own name or place, a programme tag the corpus's titles repeat (the `generation`'s
 *  context, its pieces read as its normalizer sanitizes them), a festival's programme slot. Each node's pieces and each venue's places are read once
 *  for the resolve holding it, dropped with it. */
private[identity] final class NamingPieces(generation: CandidateGeneration) {
  import EvidenceNode.placesOf

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

  /** Does `node`'s title name the film only by a PROGRAMME TAG — a whole delimited piece billed in capitals beside a
   *  rest that is not ([[IdentityMeasures.capitalisedTags]]), that RECURS across the corpus's titles
   *  ([[CorpusContext.recurringSegment]]: no listing's whole title, a piece of three whole titles or more) while no
   *  other piece of the title recurs? Learned by frequency, as a decoration is, not listed: PL Kino Amondo bills the
   *  Ukrainian Film Festival "Alim | UFF", "Atlantyda | UFF", "Odblask | UFF", and "Uff" (2020) is an Indian film
   *  TMDB ranks first for the tag. A work billed under recurring banners is the work ("UFF | Kino Seniora" beside
   *  "Lalka | Kino Seniora" and more), and a tag billed once is a title piece like any other. */
  def namesOnlyATag(node: EvidenceNode, candidate: Candidate): Boolean = {
    val listing = node.evidence.published
    val billed  = listing.rawTitle.getOrElse(listing.title)
    val tags    = IdentityMeasures.capitalisedTags(listing).map(IdentityMeasures.key).toSet
    tags.nonEmpty && {
      val pieces = namingPieces(node, candidate).map(words => IdentityMeasures.key(words.mkString(" ")))
      val (tagged, rest) = listing.shapesOfOwn(billed).partition(shape => tags(IdentityMeasures.key(shape)))
      def recurs(piece: String) = generation.context.recurringSegment(generation.normalizer.sanitize(piece))
      pieces.nonEmpty && pieces.forall(tags) && tagged.filter(tag => pieces(IdentityMeasures.key(tag))).forall(recurs) && !rest.exists(recurs)
    }
  }

  /** Does `node`'s title name the film only by the PROGRAMME SLOT it bills under a festival's banner
   *  ([[services.titles.billing.BillingParse.programmeSlot]])? UK "Unrestricted View Horror Film Festival 2026: Opening Night" names the 2016
   *  "Opening Night" (and Cassavetes' 1977 one) by its slot alone: the festival's opening night, an event. */
  def namesOnlyAProgrammeSlot(node: EvidenceNode, candidate: Candidate): Boolean = {
    val listing = node.evidence.published
    BillingParse(listing.rawTitle.getOrElse(listing.title)).programmeSlot.exists { slot =>
      val pieces = namingPieces(node, candidate)
      pieces.nonEmpty && pieces.forall(_ == services.titles.TitleContainment.tokens(slot).toSeq)
    }
  }

  // Once per (node, film) and per venue for THIS resolve, dropped with it: every node meets every
  // candidate of its family's pool here and in the constraint edges (`ConstraintEdges.namesBeside`).
  private val piecesOf = mutable.HashMap.empty[(String, Int), Set[Seq[String]]]
  private val placesOfVenue = mutable.HashMap.empty[models.Cinema, Seq[Seq[String]]]
  /** The pieces of `node`'s title that name `candidate`'s film ([[IdentityMeasures.namingPieces]]). */
  def namingPieces(node: EvidenceNode, candidate: Candidate): Set[Seq[String]] =
    piecesOf.getOrElseUpdate((node.id, candidate.tmdbId), IdentityMeasures.namingPieces(node.evidence.measured, candidate.film))
  private def places(cinema: models.Cinema): Seq[Seq[String]] = placesOfVenue.getOrElseUpdate(cinema, placesOf(cinema))
}
