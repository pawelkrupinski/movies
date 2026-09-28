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
    val index = members.zipWithIndex.flatMap { case (n, i) => families.blockKeysOf(n.id).map(_ -> i) }.groupMap(_._1)(_._2)
    index.values.iterator.flatMap { is =>
      val sorted = is.distinct.sorted
      for (x <- sorted.iterator; y <- sorted.iterator if x < y) yield (x, y)
    }.toSeq.distinct.sorted.map { case (i, j) => (members(i), members(j)) }
  }

  // The two listings' own evidence apart: the seasons their titles name, or the learned
  // "listing-listing" scope when they compare a fact both published.
  private def listingsApart(x: EvidenceNode, y: EvidenceNode): Option[String] = {
    val (a, b) = (x.evidence.published, y.evidence.published)
    lazy val m = IdentityMeasures.listingListing(a, b, sameVenue = (x.venues intersect y.venues).nonEmpty, sharedChainId = None)
    ListingConstraints.seasonsApart(a.seasonYear, b.seasonYear, b.year)
      .orElse(ListingConstraints.seasonsApart(b.seasonYear, a.seasonYear, a.year))
      .orElse(ListingConstraints.learnedListingListing(calibration, m))
      .map(_.toString)
  }

  // The pins' own edges between `members`, and the derived edges they leave standing.
  private val nodeOfListing: Map[ListingKey, EvidenceNode] = nodes.flatMap(n => n.listings.map(_.key -> n)).toMap
  private def pinEdges(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] = {
    val here = members.map(_.id).toSet
    def edge(e: FamilyClosure.Edge[ListingKey], tier: Int) =
      Option.when(here(nodeOfListing(e.a).id) && here(nodeOfListing(e.b).id) && nodeOfListing(e.a).id != nodeOfListing(e.b).id)(
        ResolverEdge(nodeOfListing(e.a).id, nodeOfListing(e.b).id, e.must, tier, e.reason))
    (pins.mustLinks.filter(e => nodeOfListing.contains(e.a) && nodeOfListing.contains(e.b)).flatMap(edge(_, 0)) ++
      pins.cannotLinks(members.flatMap(_.listings.map(_.key)), k => filmOf(nodeOfListing(k).id)).flatMap(edge(_, 0))).distinct
  }
  private def admitted(e: ResolverEdge): Boolean =
    pins.admits(FamilyClosure.Edge(nodeById(e.a).listings.head.key, nodeById(e.b).listings.head.key, e.must, e.reason))

  /** Does the rest of `decorated`'s title name a film of its own, beside the title `whole` — a
   *  candidate it may still take whose naming pieces share no word with `whole`? "Lalka (Dolly)"
   *  carries "Lalka" whole, but its "Dolly" names Blackhurst's film: it is not merely a decorated
   *  "Lalka", and the segment must not decide between the two for it. */
  private def namesBeside(decorated: EvidenceNode, whole: String): Boolean = {
    val words = services.movies.TitleContainment.tokens(whole).toSet
    families.scopeOf(decorated).of(decorated).exists { s =>
      val pieces = IdentityMeasures.namingPieces(decorated.evidence.measured, s.c.film)
      !s.denied && pieces.nonEmpty && pieces.forall(p => (p.toSet intersect words).isEmpty)
    }
  }

  /** The edges between `members`, each node's film (accepted or voted) given by `filmOf`. */
  def of(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] =
    pinEdges(members, filmOf) ++ pairsSharingAKey(members).flatMap { case (x, y) =>
      val (ex, ey) = (x.evidence, y.evidence)
      val (fx, fy) = (filmOf(x.id), filmOf(y.id))
      val sameFilm = fx.isDefined && fx == fy
      def edge(must: Boolean, tier: Int, reason: String) = ResolverEdge(x.id, y.id, must, tier, reason)
      val cannots = if (sameFilm) Nil else Seq(
        Option.when(fx.isDefined && fy.isDefined)("different-films"),
        Option.when(fx.exists(families.denies(y, _)) || fy.exists(families.denies(x, _)))("denies-film"),
        listingsApart(x, y)
      ).flatten
      val titleX = sanitized(ex.cleanTitle)
      val originals = (ex.originalTitle.map(sanitized) ++ ey.originalTitle.map(sanitized)).filter(_.nonEmpty).toSet
      val musts = Seq(
        Option.when(sameFilm)((1, "same-film")),
        Option.when(titleX.nonEmpty && titleX == sanitized(ey.cleanTitle))((2, "same-title")),
        // Unless the form is only what the two titles share BESIDE the films they name: a
        // festival's spellings of its films all search as the festival's suffix.
        Option.when(searchForm(ex.cleanTitle).nonEmpty && searchForm(ex.cleanTitle) == searchForm(ey.cleanTitle) &&
          !titlesBeside(x, searchForm(ex.cleanTitle)) && !titlesBeside(y, searchForm(ey.cleanTitle)))((3, "same-search-form")),
        Option.when(originals.contains(titleX) || originals.contains(sanitized(ey.cleanTitle)) ||
          (ex.originalTitle.isDefined && ex.originalTitle.map(sanitized) == ey.originalTitle.map(sanitized) && originals.nonEmpty))((3, "original-title")),
        // One listing's whole title is a delimited SEGMENT of the other's ("Oficjalna premiera:
        // Lalka" and "Lalka"): the decorated spelling joins its plain sibling's cluster, so group
        // voting and the venue signal reach it. Whole segments only — "Zärtlich kreist die Faust"
        // has no delimiter before "Faust" — and the ambiguity rule leaves a spelling whose segment
        // names two films apart, as it does one whose rest names a film of its own (`namesBeside`).
        Option.when((segmentOf(x, y) && !namesBeside(y, ex.cleanTitle)) || (segmentOf(y, x) && !namesBeside(x, ey.cleanTitle)))((4, "title-segment"))
      ).flatten
      cannots.map(edge(must = false, 0, _)) ++ musts.sortBy(_._1).take(1).map { case (t, r) => edge(must = true, t, r) }
    }.filter(admitted)
}

private[identity] object ConstraintEdges {
  /** The must-link tiers a title draws: same title, same search form or original title, one
   *  title a segment of the other. */
  val TitleTiers: Set[Int] = Set(2, 3, 4)
}
