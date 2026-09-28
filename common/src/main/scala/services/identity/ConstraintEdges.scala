package services.identity

import services.movies.{ListingConstraints, ListingKey}

/** The constraint edges between nodes sharing a block key ([[of]]): must-links by tier (0 pinned,
 *  1 same accepted film, 2 same sanitised title, 3 same search form — unless either title carries
 *  another listing's title beside it — or original title, 4 one's title a delimited segment of the
 *  other's, unless the rest of the other's names a film of its own) and cannot-links (different
 *  accepted films; a node denying the other's film; the two listings' own evidence apart). */
private[identity] final class ConstraintEdges(scoring: CandidateScoring, families: Families, links: TitleLinks) {
  import scoring.{calibration, pins}
  import scoring.generation.{nodeById, nodes}
  import links.{sanitized, searchForm, segmentOf, titlesBeside}

  private def pairsSharingAKey(members: Seq[EvidenceNode]): Seq[(EvidenceNode, EvidenceNode)] = {
    val index = members.zipWithIndex.flatMap { case (node, position) => families.blockKeysOf(node.id).map(_ -> position) }.groupMap(_._1)(_._2)
    index.values.iterator.flatMap { positions =>
      val sorted = positions.distinct.sorted
      for (first <- sorted.iterator; second <- sorted.iterator if first < second) yield (first, second)
    }.toSeq.distinct.sorted.map { case (position, otherPosition) => (members(position), members(otherPosition)) }
  }

  // The two listings' own evidence apart: the seasons their titles name, or the learned
  // "listing-listing" scope when they compare a fact both published.
  private def listingsApart(first: EvidenceNode, second: EvidenceNode): Option[String] = {
    val (firstListing, secondListing) = (first.evidence.published, second.evidence.published)
    lazy val measures = IdentityMeasures.listingListing(firstListing, secondListing, sameVenue = (first.venues intersect second.venues).nonEmpty, sharedChainId = None)
    ListingConstraints.seasonsApart(firstListing.seasonYear, secondListing.seasonYear, secondListing.year)
      .orElse(ListingConstraints.seasonsApart(secondListing.seasonYear, firstListing.seasonYear, firstListing.year))
      .orElse(ListingConstraints.learnedListingListing(calibration, measures))
      .map(_.toString)
  }

  // The pins' own edges between `members`, and the derived edges they leave standing.
  private val nodeOfListing: Map[ListingKey, EvidenceNode] = nodes.flatMap(node => node.listings.map(_.key -> node)).toMap
  private def pinEdges(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] = {
    val here = members.map(_.id).toSet
    def edge(link: FamilyClosure.Edge[ListingKey], tier: Int) =
      Option.when(here(nodeOfListing(link.a).id) && here(nodeOfListing(link.b).id) && nodeOfListing(link.a).id != nodeOfListing(link.b).id)(
        ResolverEdge(nodeOfListing(link.a).id, nodeOfListing(link.b).id, link.must, tier, link.reason))
    (pins.mustLinks.filter(link => nodeOfListing.contains(link.a) && nodeOfListing.contains(link.b)).flatMap(edge(_, 0)) ++
      pins.cannotLinks(members.flatMap(_.listings.map(_.key)), listingKey => filmOf(nodeOfListing(listingKey).id)).flatMap(edge(_, 0))).distinct
  }
  private def admitted(link: ResolverEdge): Boolean =
    pins.admits(FamilyClosure.Edge(nodeById(link.a).listings.head.key, nodeById(link.b).listings.head.key, link.must, link.reason))

  /** Does the rest of `decorated`'s title name a film of its own, beside the title `whole` — a
   *  candidate it may still take whose naming pieces share no word with `whole`? "Lalka (Dolly)"
   *  carries "Lalka" whole, but its "Dolly" names Blackhurst's film: it is not merely a decorated
   *  "Lalka", and the segment must not decide between the two for it. */
  private def namesBeside(decorated: EvidenceNode, whole: String): Boolean = {
    val words = services.movies.TitleContainment.tokens(whole).toSet
    families.scopeOf(decorated).of(decorated).exists { scored =>
      val pieces = IdentityMeasures.namingPieces(decorated.evidence.measured, scored.candidate.film)
      !scored.denied && pieces.nonEmpty && pieces.forall(piece => (piece.toSet intersect words).isEmpty)
    }
  }

  /** The edges between `members`, each node's film (accepted or voted) given by `filmOf`. */
  def of(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] =
    pinEdges(members, filmOf) ++ pairsSharingAKey(members).flatMap { case (first, second) =>
      val (firstEvidence, secondEvidence) = (first.evidence, second.evidence)
      val (firstFilm, secondFilm) = (filmOf(first.id), filmOf(second.id))
      val sameFilm = firstFilm.isDefined && firstFilm == secondFilm
      def edge(must: Boolean, tier: Int, reason: String) = ResolverEdge(first.id, second.id, must, tier, reason)
      val cannots = if (sameFilm) Nil else Seq(
        Option.when(firstFilm.isDefined && secondFilm.isDefined)("different-films"),
        Option.when(firstFilm.exists(families.denies(second, _)) || secondFilm.exists(families.denies(first, _)))("denies-film"),
        listingsApart(first, second)
      ).flatten
      val titleX = sanitized(firstEvidence.cleanTitle)
      val originals = (firstEvidence.originalTitle.map(sanitized) ++ secondEvidence.originalTitle.map(sanitized)).filter(_.nonEmpty).toSet
      val musts = Seq(
        Option.when(sameFilm)((1, "same-film")),
        Option.when(titleX.nonEmpty && titleX == sanitized(secondEvidence.cleanTitle))((2, "same-title")),
        // Unless the form is only what the two titles share BESIDE the films they name: a
        // festival's spellings of its films all search as the festival's suffix.
        Option.when(searchForm(firstEvidence.cleanTitle).nonEmpty && searchForm(firstEvidence.cleanTitle) == searchForm(secondEvidence.cleanTitle) &&
          !titlesBeside(first, searchForm(firstEvidence.cleanTitle)) && !titlesBeside(second, searchForm(secondEvidence.cleanTitle)))((3, "same-search-form")),
        Option.when(originals.contains(titleX) || originals.contains(sanitized(secondEvidence.cleanTitle)) ||
          (firstEvidence.originalTitle.isDefined && firstEvidence.originalTitle.map(sanitized) == secondEvidence.originalTitle.map(sanitized) && originals.nonEmpty))((3, "original-title")),
        // One listing's whole title is a delimited SEGMENT of the other's ("Oficjalna premiera:
        // Lalka" and "Lalka"): the decorated spelling joins its plain sibling's cluster, so group
        // voting and the venue signal reach it. Whole segments only — "Zärtlich kreist die Faust"
        // has no delimiter before "Faust" — and the ambiguity rule leaves a spelling whose segment
        // names two films apart, as it does one whose rest names a film of its own (`namesBeside`).
        Option.when((segmentOf(first, second) && !namesBeside(second, firstEvidence.cleanTitle)) || (segmentOf(second, first) && !namesBeside(first, secondEvidence.cleanTitle)))((4, "title-segment"))
      ).flatten
      cannots.map(edge(must = false, 0, _)) ++ musts.sortBy(_._1).take(1).map { case (tier, reason) => edge(must = true, tier, reason) }
    }.filter(admitted)
}

private[identity] object ConstraintEdges {
  /** The must-link tiers a title draws: same title, same search form or original title, one
   *  title a segment of the other. */
  val TitleTiers: Set[Int] = Set(2, 3, 4)
}
