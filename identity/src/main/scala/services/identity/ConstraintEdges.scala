package services.identity

import services.movies.{ListingConstraints, ListingKey}

/** The constraint edges between nodes sharing a block key ([[of]]): must-links by tier (0 pinned,
 *  1 same accepted film, 2 same catalogue id or same sanitised title, 3 same search form — unless either title carries
 *  another listing's title beside it — or original title, 4 one's title a delimited segment of the
 *  other's, unless the rest of the other's names a film of its own) and cannot-links (different
 *  accepted films; a node denying the other's film; the two listings' own evidence apart; a feed's copied original
 *  title alone joining them; a reused page's copied facts denying the other's feed-linked film). */
private[identity] final class ConstraintEdges(scoring: CandidateScoring, families: Families, links: TitleLinks) {
  import scoring.{calibration, pins}
  import scoring.generation.{nodeById, nodes}
  import links.{sanitized, searchForm, segmentOf, titlesBeside}


  private def pairsSharingAKey(group: Seq[EvidenceNode]): Seq[(EvidenceNode, EvidenceNode)] = {
    // By position, so indexed: a resolve's first round pairs every node of the region.
    val members = group.toIndexedSeq
    val index = members.zipWithIndex.flatMap { case (node, position) => families.blockKeysOf(node.id).map(_ -> position) }.groupMap(_._1)(_._2)
    index.values.iterator.flatMap { positions =>
      val sorted = positions.distinct.sorted
      for (first <- sorted.iterator; second <- sorted.iterator if first < second) yield (first, second)
    }.toSeq.distinct.sorted.map { case (position, otherPosition) => (members(position), members(otherPosition)) }
  }

  // The two listings' own evidence apart: the seasons their titles name, or the learned
  // "listing-listing" scope when they compare a fact both published.
  // A pair's own evidence apart reads neither film nor round: asked by the first round over every pair and again by
  // each family's, it is weighed once per resolve.
  private val apart = scala.collection.mutable.HashMap.empty[(String, String), Option[String]]
  private def listingsApart(first: EvidenceNode, second: EvidenceNode): Option[String] =
    apart.getOrElseUpdate((first.id, second.id), weighApart(first, second))

  /** The search form two nodes' titles share — unless it is only what both bill BESIDE the films they name: a
   *  festival's spellings of its films all search as the festival's suffix. */
  private def sharedSearchForm(first: EvidenceNode, second: EvidenceNode): Option[String] =
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
    val (firstListing, secondListing) = (titled(families.scopeOf(first).evidenceOf(first).published), titled(families.scopeOf(second).evidenceOf(second).published))
    lazy val measures = IdentityMeasures.listingListing(firstListing, secondListing, sameVenue = (first.venues intersect second.venues).nonEmpty, sharedChainId = None)
    ListingConstraints.seasonsApart(firstListing.seasonYear, secondListing.seasonYear, secondListing.year)
      .orElse(ListingConstraints.seasonsApart(secondListing.seasonYear, firstListing.seasonYear, firstListing.year))
      .orElse(LearnedCannotLinks.learnedListingListing(calibration, measures))
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
      val pieces = scoring.namingPieces(decorated, scored.candidate)
      !scored.denied && pieces.nonEmpty && pieces.forall(piece => (piece.toSet intersect words).isEmpty)
    }
  }

  /** What one catalogue id binds `node` by: the id itself — a chain's own catalogue lists one film under one id — but a
   *  feed's ([[SourceReliability.linkDecides]]) only beside the node's title: an aggregator links screenings to its entries
   *  and can link two venues' different films to one (Filmstarts' placeholder 51988 "Film Program"), so its id joins
   *  only listings whose titles already agree. */
  def catalogueKey(id: CatalogueId, node: EvidenceNode): String =
    if (!SourceReliability.linkDecides(id)) s"${id.key}\u0000${sanitized(node.evidence.cleanTitle)}" else id.key

  private def catalogueKeys(node: EvidenceNode): Seq[String] = node.listings.flatMap(_.catalogueIds).distinct.map(catalogueKey(_, node))

  private def sharesCatalogueId(first: EvidenceNode, second: EvidenceNode): Boolean = {
    val keys = catalogueKeys(first)
    keys.nonEmpty && catalogueKeys(second).exists(keys.contains)
  }

  /** Do two nodes meet only through a feed's copied original title — no billed title or search form of theirs alike, every
   *  original title either carries a feed's copy ([[SourceReliability.copiesOriginalTitle]]) — while `linked`'s TMDB-keyed
   *  feed links a film that `other`, holding none yet, denies by its own facts? The copy is the linked entry's English
   *  title restated, so it joins its namesakes to the link's film, not to the venue's. DE Filmmuseum München's 95-minute
   *  "The Stranger" (Jonah Manibusan) met Cineamo's "Der Fremde" venues only by the "The Stranger" Cineamo copied from
   *  Ozon's 122-minute "L'Étranger" (1429348m), and took that film. Not where `other` already holds another film than the
   *  link's: a venue billing a film's English title, whose operator linked a namesake record (Traum-Kino's "TOGETHER
   *  APART", Andreasstadel's "Departures"), is the copy's film, and that wider form lost 5 right takes to cure CITY 46's
   *  "THE BLACK HOLE" (recording 38713274617, docs/identity-experiments.md). */
  private def copiedOriginalJoinsAnotherFilm(linked: EvidenceNode, other: EvidenceNode, linkedFilm: Option[Int], otherFilm: Option[Int]): Boolean =
    scoring.generation.numbersFilmsByTmdbId && FeedLink.linked(linked.listings).exists { case (_, film) =>
      val bothTitles = Seq(linked, other)
      bothTitles.exists(_.evidence.originalTitle.isDefined) &&
        bothTitles.forall(node => node.evidence.originalTitle.isEmpty || node.listings.forall(SourceReliability.copiesOriginalTitle)) &&
        sanitized(linked.evidence.cleanTitle) != sanitized(other.evidence.cleanTitle) && sharedSearchForm(linked, other).isEmpty &&
        !linkedFilm.exists(_ != film) && otherFilm.isEmpty && families.denies(other, film)
    }

  /** Does `other`, holding no film, deny the film `linked`'s TMDB-keyed feed links only by the facts of a REUSED page
   *  ([[Families.readsAsReused]]) — another record's original title, director and year copied onto it? Its title may be
   *  the linked film's word for word, but its facts are that other record's, and a cluster pooling them vetoes the film
   *  for every member. DE Bambi's "De Gaulle Zeit des Widerstands" sits on kinoheld's page of Schauder's "Vater, Sohn
   *  und der Preis des Widerstands" (1675748): joined by its title to E-Werk's, Weltspiegel's and Zeise's billings, which
   *  Cineamo links to Baudry's De Gaulle (1165366m), it denied them their film (recording 528). */
  private def reusedPageDeniesLink(linked: EvidenceNode, other: EvidenceNode, linkedFilm: Option[Int], otherFilm: Option[Int]): Boolean =
    scoring.generation.numbersFilmsByTmdbId && otherFilm.isEmpty && FeedLink.linked(linked.listings).exists { case (_, film) =>
      !linkedFilm.exists(_ != film) && !FeedLink.linked(other.listings).exists(_._2 == film) && families.denies(other, film) &&
        families.readsAsReused(other)
    }

  /** Do the TMDB-keyed feeds of the two nodes link two different films, neither link overruled by the title's other
   *  sources ([[FamilyScope.overruled]])? Each operator picked its film's record: two picks are two films, and a title
   *  alone joining them hands one the other's namesake. DE Universum-City Karlsruhe's "Lauras Stern" (cineamo 903746m,
   *  the 2021 film, its 75 minutes) joined by its same title the three venues Cineamo links to the 2004 film (12697m)
   *  and took it (recording 42332112286). An overruled link is the operator's slip against the title's other sources
   *  (DE "Karla", "Momo"): it keeps apart nothing. Billed alike only: two titles meeting by an original title keep the
   *  join, the cluster's other spellings reading through it (DE "L'ÂGE D'OR" ×3 lost Buñuel's film when Studio-Kino's
   *  "Das goldene Zeitalter", linked to a 2025 namesake, was kept apart from its linked sibling). And only a pick each
   *  node's own evidence holds up -- its link's film rated no lower than the other's: DE Neues Arena's "IN THE MOOD FOR
   *  LOVE", Cineamo-linked to a 31-minute 2015 short (646676m), is Wong Kar-wai's film beside Kino im DFF's 843m. */
  private def linkedApart(first: EvidenceNode, second: EvidenceNode): Boolean =
    scoring.generation.numbersFilmsByTmdbId && sanitized(first.evidence.cleanTitle) == sanitized(second.evidence.cleanTitle) && {
      def link(node: EvidenceNode) = { val scope = families.scopeOf(node); scope.feedLinked(node).filter(_ => scope.overruled(node).isEmpty).map(_._2) }
      // the node's own evidence rates its link's film no lower than the other's: a pick its own scoring holds up
      def holdsUp(node: EvidenceNode, own: Int, others: Int) = {
        val scored = families.scopeOf(node).of(node)
        def probability(film: Int) = scored.find(_.candidate.tmdbId == film).filterNot(_.denied).map(_.probability)
        probability(own).exists(mine => probability(others).forall(mine >= _))
      }
      (link(first), link(second)) match {
        case (Some(one), Some(other)) => one != other && holdsUp(first, one, other) && holdsUp(second, other, one)
        case _                        => false
      }
    }

  /** Does `node`, taking no film alone, credit a director that the film `other` holds was not directed by (`director`
   *  `different`: no credit of the record's is the same person), its credit standing -- every listing's own statement
   *  ([[SourceReliability.Fact.Directors]] decided), or a credit naming a record of its very title by that director that
   *  the billing's own year or running time fits too (a copied credit then names the film its billing is, not a slip)?
   *  A title alone joining them would lend it a film its own credit contradicts. DE Bambi's "Les Misérables" [2026, 99′]
   *  {Fred Cavayé, kinoheld's} joined ScharfrichterKino's "Les Misérables - Die Geschichte von Jean Valjean" by a title
   *  segment and took Besnard's "Jean Valjean" (1376439), beside Cavayé's own "Les Misérables" (2026, 1241803).
   *
   *  A copied credit with no year or running time beside it is no such statement: DE Mal seh'n Kino's "THE THING WITH
   *  FEATHERS" {Anja Oelofse} carries the credit kinoheld copied from Oelofse's 24-minute short of that title, and is
   *  Southern's film the venue also bills as "Das Ding mit Federn". */
  private def creditDeniesHeld(node: EvidenceNode, otherFilm: Option[Int], nodeFilm: Option[Int]): Boolean =
    nodeFilm.isEmpty && otherFilm.exists { held =>
      lazy val scored = families.scopeOf(node).of(node)
      node.listings.forall(_.directors.nonEmpty) &&
        scored.find(_.candidate.tmdbId == held).exists(_.category("director").contains("different")) &&
        (node.listings.forall(SourceReliability.decides(_, SourceReliability.Fact.Directors)) ||
          scored.exists(s => s.candidate.tmdbId != held && IdentityMeasures.sameDirector(s.measures) && s.category("title").contains("exact") &&
            (IdentityMeasures.ownAgreement(s.measures)._1("year") || IdentityMeasures.runtimeGap(s.measures).exists(_ <= Corroboration.RuntimeBand))))
    }

  /** Is `kept` a node of kept listings alone ([[Listing.kept]]) and `other` one whose feed link the present sources
   *  overrule toward another film ([[FamilyScope.overruled]])? The kept facts keep their own film's details alive; they
   *  never join a listing whose own sources say it is another film. DE "Eternity", prod-latest 41265286306: Caligari's
   *  kept kinoheld listing of Freyne's 2025 film joined the bare "Eternity" [2025] Cineamo links to it, which Zazie's and
   *  kinoheld's 2026 entries credit to Meures, and moved 18 venues onto Freyne's film. */
  private def keptBesideOverruled(kept: EvidenceNode, other: EvidenceNode): Boolean =
    !Authority.may(Authority.of(kept), Authority.Act.JoinOverruled) && Authority.of(other) != Authority.Provenance.Kept &&
      families.scopeOf(other).overruled(other).isDefined

  /** The edges between `members`, each node's film (accepted or voted) given by `filmOf`. */
  def of(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] =
    pinEdges(members, filmOf) ++ pairsSharingAKey(members).flatMap { case (first, second) =>
      val (firstEvidence, secondEvidence) = (first.evidence, second.evidence)
      val (firstFilm, secondFilm) = (filmOf(first.id), filmOf(second.id))
      val sameFilm = firstFilm.isDefined && firstFilm == secondFilm
      def edge(must: Boolean, tier: Int, reason: String) = ResolverEdge(first.id, second.id, must, tier, reason)
      // One chain's one catalogue id is one film (a feed's, under one title: `catalogueKey`): the facts each venue publishes of it (a bare
      // re-release dated by its title, "9 to 5 (2026)") never keep its listings apart, as a pin's
      // group is never kept apart. The cluster's pooled evidence then names the film.
      val cannots = if (sameFilm || sharesCatalogueId(first, second)) Nil else Seq(
        Option.when(firstFilm.isDefined && secondFilm.isDefined)("different-films"),
        Option.when(firstFilm.exists(families.denies(second, _)) || secondFilm.exists(families.denies(first, _)))("denies-film"),
        listingsApart(first, second),
        Option.when(copiedOriginalJoinsAnotherFilm(first, second, firstFilm, secondFilm) ||
          copiedOriginalJoinsAnotherFilm(second, first, secondFilm, firstFilm))(ConstraintEdges.CopiedOriginalApart),
        Option.when(reusedPageDeniesLink(first, second, firstFilm, secondFilm) || reusedPageDeniesLink(second, first, secondFilm, firstFilm))(
          ConstraintEdges.ReusedPageApart),
        Option.when(keptBesideOverruled(first, second) || keptBesideOverruled(second, first))(ConstraintEdges.KeptOverruledApart),
        Option.when(linkedApart(first, second))(ConstraintEdges.LinkedApart),
        Option.when(creditDeniesHeld(first, secondFilm, firstFilm) || creditDeniesHeld(second, firstFilm, secondFilm))(ConstraintEdges.CreditApart)
      ).flatten.filterNot(reason => RuleAblation.disables("apart:" + DecisionTrace.id(reason)))
      val musts = Seq(
        Option.when(sameFilm)((1, "same-film")),
        // One chain lists them under one catalogue id: its one film, whatever each venue calls it (a feed's id, only
        // where their titles agree: `catalogueKey`).
        Option.when(sharesCatalogueId(first, second))((2, ConstraintEdges.SameCatalogueId)),
        // The titles' must-link, by the one title relation (TitleRelation.tier: same title, same search form, original
        // title, whole segment). One listing's whole title a delimited SEGMENT of the other's ("Oficjalna premiera:
        // Lalka" and "Lalka") joins the decorated spelling to its plain sibling's cluster, so group voting and the venue
        // signal reach it. Whole segments only — "Zärtlich kreist die Faust" has no delimiter before "Faust" — and the
        // ambiguity rule leaves a spelling whose segment names two films apart, as it does one whose rest names a film of
        // its own (`namesBeside`).
        links.tier(first, second).filter { case (tier, _) => tier != 4 ||
          (segmentOf(first, second) && !namesBeside(second, firstEvidence.cleanTitle)) || (segmentOf(second, first) && !namesBeside(first, secondEvidence.cleanTitle)) }
      ).flatten.filterNot { case (_, reason) => RuleAblation.disables("join:" + reason) }
      cannots.map(edge(must = false, 0, _)) ++ musts.sortBy(_._1).take(1).map { case (tier, reason) => edge(must = true, tier, reason) }
    }.filter(admitted)
}

private[identity] object ConstraintEdges {
  /** The must-link reasons a title or a catalogue id gives — the ones [[Corroboration.Vouching]] lets vouch for a node. */
  val SameTitle        = "same-title"
  val SameCatalogueId  = "same-catalogue-id"
  val SameSearchForm   = "same-search-form"
  val TitleSegment     = "title-segment"
  val OriginalTitle    = "original-title"
  /** The cannot-link of two nodes a feed's copied original title alone joins, one's feed linking a film the other's is not. */
  val CopiedOriginalApart = "copied-original-apart"
  /** The cannot-link of a node a feed links to a film and one whose reused page's copied facts deny it. */
  val ReusedPageApart = "reused-page-apart"
  /** The cannot-link of a node of kept listings and one whose feed link the present sources overrule. */
  val KeptOverruledApart = "kept-overruled-apart"
  /** The cannot-link of two nodes whose TMDB-keyed feeds link two films, neither link overruled. */
  val LinkedApart = "linked-apart"
  /** The cannot-link of a node crediting a director of its own and a node holding a film that director did not make. */
  val CreditApart = "credit-apart"

  /** The must-link tiers a title draws: same title, same search form or original title, one
   *  title a segment of the other. */
  val TitleTiers: Set[Int] = Set(2, 3, 4)
}
