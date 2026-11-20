package services.identity

import models.ListingKey
import services.identity.vocabulary.{FamilyClosure, ResolverEdge}

/** The pins' own edges between a family's nodes ([[ConstraintEdges]]), and which derived edges they leave standing. */
private[identity] final class PinEdges(scoring: CandidateScoring) {
  import scoring.pins
  import scoring.generation.{nodeById, nodes}

  private val nodeOfListing: Map[ListingKey, EvidenceNode] = nodes.flatMap(node => node.listings.map(_.key -> node)).toMap

  /** The pins' own edges between `members`, each node's film given by `filmOf`. */
  def of(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] = {
    val here = members.map(_.id).toSet
    def edge(link: FamilyClosure.Edge[ListingKey], tier: Int) =
      Option.when(here(nodeOfListing(link.a).id) && here(nodeOfListing(link.b).id) && nodeOfListing(link.a).id != nodeOfListing(link.b).id)(
        ResolverEdge(nodeOfListing(link.a).id, nodeOfListing(link.b).id, link.must, tier, link.reason))
    (pins.mustLinks.filter(link => nodeOfListing.contains(link.a) && nodeOfListing.contains(link.b)).flatMap(edge(_, 0)) ++
      pins.cannotLinks(members.flatMap(_.listings.map(_.key)), listingKey => filmOf(nodeOfListing(listingKey).id)).flatMap(edge(_, 0))).distinct
  }
  /** Does a derived edge stand beside the pins? */
  def admitted(link: ResolverEdge): Boolean =
    pins.admits(FamilyClosure.Edge(nodeById(link.a).listings.head.key, nodeById(link.b).listings.head.key, link.must, link.reason))
}
