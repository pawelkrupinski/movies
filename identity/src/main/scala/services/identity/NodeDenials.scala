package services.identity

import scala.collection.mutable

/** What rules a film out for one node of a family ([[FamilyScope]]) before its evidence is scored: a pin, its feed link
 *  overruled, a copied title, a title naming it only by the venue's name, a tag or a programme slot, a festival billed
 *  alone, a silent billing, the venues' cast, a relay's own text. */
private[identity] final class NodeDenials(scoring: CandidateScoring, links: FeedLinkOverrule, copied: CopiedTitleDenial) {
  import scoring.{namesOnlyAProgrammeSlot, namesOnlyATag, namesOnlyItsVenue, pins}
  import scoring.generation.candidateById

  private val billing = new BillingDenials

  /** Why `node` rules `id` out before its evidence is scored: a pin, an overruled feed link, a feed link's copied title
   *  (a short its feed links another film beside among them), a title naming it only by the venue's own name, a programme tag or
   *  slot, a title billing a film festival alone, a billing as a silent film of a sound-era film, or a relay's record a
   *  plain billing's own text contradicts ([[RelayRecords.contradictedByOwnText]]). */
  def of(node: EvidenceNode, id: Int): Option[Denial] = denialMemo.getOrElseUpdate((node.id, id), denialOf(node, id))
  // A pure function of the node and the film within this family (its pins, links, pool and generation fixed): a node's own
  // score and every pooled cluster holding it ask it again for each candidate of the pool, 5.5-9% of a convergence or
  // order row's CPU (JFR, docs/investigations/2026-11-01-twenty-runners.md section 9).
  private val denialMemo = mutable.HashMap.empty[(String, Int), Option[Denial]]
  private def denialOf(node: EvidenceNode, id: Int): Option[Denial] =
    Option.when(pins.deniedFilms(node.listings.head.key)(id))("pinned never this film")
      .orElse(links.overruled(node).filter(_.film == id).map(_.denial)).map(Denial(_))
      .orElse(copied.namedOnlyByCopiedTitle(node, candidateById(id)))
      .orElse((Option.when(namesOnlyItsVenue(node, candidateById(id)))("its title names it only by the venue's own name"))
      .orElse(Option.when(namesOnlyATag(node, candidateById(id)))("its title names it only by a programme tag billed beside many titles"))
      .orElse(Option.when(namesOnlyAProgrammeSlot(node, candidateById(id)))("its title names it only by a festival's programme slot"))
      .orElse(billing.of(node, candidateById(id).film))
      // a plain billing whose own text credits another film's director, never the relay's
      .orElse(Option.when(RelayRecords.contradictedByOwnText(node.listings, candidateById(id).film,
        scoring.generation.reached(node).filterNot(_ == id).map(other => candidateById(other).film)))(RelayRecords.OwnTextDenial)).map(Denial(_)))
}
