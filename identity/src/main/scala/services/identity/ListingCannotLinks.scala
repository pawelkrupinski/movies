package services.identity

import services.evidence.ListingConstraints

/** Whether two nodes' own evidence keeps them apart ([[ConstraintEdges]]): the seasons their titles name, or the learned
 *  "listing-listing" scope over a fact both publish, as each node's scope reads its facts; and the film-less event each
 *  node bills, which keeps two billings of one event together. */
private[identity] final class ListingCannotLinks(scoring: CandidateScoring, families: Families, links: TitleLinks) {
  import scoring.calibration
  import links.{searchForm, titlesBeside}

  // The two listings' own evidence apart: the seasons their titles name, or the learned
  // "listing-listing" scope when they compare a fact both published.
  // A pair's own evidence apart reads neither film nor round: asked by the first round over every pair and again by
  // each family's, it is weighed once per resolve.
  private val apart = scala.collection.mutable.HashMap.empty[(String, String), Option[String]]
  def of(first: EvidenceNode, second: EvidenceNode): Option[String] =
    apart.getOrElseUpdate((first.id, second.id), weighApart(first, second))

  /** The search form two nodes' titles share — unless it is only what both bill BESIDE the films they name: a
   *  festival's spellings of its films all search as the festival's suffix. */
  def sharedSearchForm(first: EvidenceNode, second: EvidenceNode): Option[String] =
    Some(searchForm(first.evidence.cleanTitle))
      .filter(form => form.nonEmpty && form == searchForm(second.evidence.cleanTitle) && !titlesBeside(first, form) && !titlesBeside(second, form))

  private def weighApart(first: EvidenceNode, second: EvidenceNode): Option[String] = {
    // Two spellings the same-search-form must-link joins ("Kino Konesera: Róża", "FILMOWY KLUB SENIORA I
    // SENIORKI: RÓŻA") name their film by that form; their programmes are not what the learned scope
    // weighs — compared as decorated titles they only overlapped, and the cut kept them apart on that
    // alone. Their facts are compared as ever.
    // A double bill shares its first film's form and is neither film ("The Gruffalo + The Gruffalo's Child").
    val sameForm = sharedSearchForm(first, second)
      .filterNot(_ => MultiFilmBill.billsTwoWorks(first.evidence.published) || MultiFilmBill.billsTwoWorks(second.evidence.published))
    def titled(l: IdentityMeasures.Listing) = sameForm.fold(l)(form => l.copy(title = form, rawTitle = None))
    // as each node's scope reads its facts: an overruled feed link's copied facts keep no listing apart ([[FeedLink]])
    val (firstListing, secondListing) = (titled(families.evidenceOf(first).published), titled(families.evidenceOf(second).published))
    lazy val measures = IdentityMeasures.listingListing(firstListing, secondListing, sameVenue = (first.venues intersect second.venues).nonEmpty, sharedChainId = None)
    ListingConstraints.seasonsApart(firstListing.seasonYear, secondListing.seasonYear, secondListing.year)
      .orElse(ListingConstraints.seasonsApart(secondListing.seasonYear, firstListing.seasonYear, firstListing.year))
      // Two billings of one film-less event ([[oneEvent]]) are kept apart, as one relay's are, by a fact a film would be
      // told by -- never by the running time each venue books the event for (UK "League of Legends Worlds 26": Odeon's
      // 420 minutes, Cineworld's 300) or its spelling beside the event's name ("The CBeebies Panto 2026", 75 minutes,
      // beside the 60 of "CBeebies Panto 2026"). Two that state another edition, other episodes or a session's length
      // are weighed as ever.
      .orElse(Option.when((!RelayStage.oneRelaySpeltAlike(first.evidence.measured, second.evidence.measured) && !oneEvent(first, second)) ||
        relayFactsApart(measures))(LearnedCannotLinks.learnedListingListing(calibration, measures)).flatten)
      .map(_.toString)
  }

  /** The event each node's listings bill ([[NonFilmEvents.facts]]), read beside what the node's own searches
   *  found ([[CandidateGeneration.hitsOf]]: a film titled as the marathon it seems to bill), once a node. */
  private val eventOfNode = scala.collection.mutable.HashMap.empty[String, Option[NonFilmEvents.Facts]]
  private def eventOf(node: EvidenceNode) =
    eventOfNode.getOrElseUpdate(node.id, NonFilmEvents.facts(node.listings, NonFilmEvents.titledBy(scoring.generation.hitsOf(node.id))))
  /** Do both nodes name their event alike ([[NonFilmEvents.Facts.name]])? */
  def sameEventName(first: EvidenceNode, second: EvidenceNode): Boolean =
    eventOf(first).exists(a => a.name.nonEmpty && eventOf(second).exists(_.name == a.name))
  /** Do both nodes bill one film-less event ([[NonFilmEvents.Facts.filmless]]: of a kind no film record is, one
   *  production billed alike at many venues, [[NonFilmEvents.Facts.distributed]]), their own facts agreeing on
   *  it ([[NonFilmEvents.Facts.apartFrom]])? */
  def oneEvent(first: EvidenceNode, second: EvidenceNode): Boolean =
    (for (a <- eventOf(first); b <- eventOf(second))
      yield a.filmless && a.distributed && b.filmless && b.distributed && !a.apartFrom(b)).contains(true)

  /** Do two billings of one relay disagree on a fact — two directors, years further apart than one season's two? */
  private def relayFactsApart(measures: Map[String, IdentityMeasures.Measure]): Boolean =
    measures.get("director").contains(IdentityMeasures.Category("different")) ||
      Seq("year.delta", "titleYear.delta", "season.delta").exists(fact => measures.get(fact).exists {
        case IdentityMeasures.Number(delta) => delta > 1 || (fact == "season.delta" && delta > 0)
        case _                              => false
      })
}
