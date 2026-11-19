package services.identity

/** The constraint edges between nodes sharing a block key ([[of]]): must-links by tier (0 pinned,
 *  1 same accepted film, 2 same catalogue id or same sanitised title, 3 same search form — unless either title carries
 *  another listing's title beside it — or original title, 4 one's title a delimited segment of the
 *  other's, unless the rest of the other's names a film of its own) and cannot-links (different
 *  accepted films; a node denying the other's film; the two listings' own evidence apart; a feed's copied original
 *  title alone joining them; a reused page's copied facts denying the other's feed-linked film). */
private[identity] final class ConstraintEdges(scoring: CandidateScoring, families: Families, links: TitleLinks) {
  import scoring.generation.candidateOf
  import links.{sanitized, segmentOf}

  private val apart   = new ListingCannotLinks(scoring, families, links)
  private val sources = new SourceCannotLinks(scoring, families, links, apart)
  private val pinned  = new PinEdges(scoring)

  private def pairsSharingAKey(group: Seq[EvidenceNode]): Seq[(EvidenceNode, EvidenceNode)] = {
    // By position, so indexed: a resolve's first round pairs every node of the region.
    val members = group.toIndexedSeq
    val index = members.zipWithIndex.flatMap { case (node, position) => families.blockKeysOf(node.id).map(_ -> position) }.groupMap(_._1)(_._2)
    index.values.iterator.flatMap { positions =>
      val sorted = positions.distinct.sorted
      for (first <- sorted.iterator; second <- sorted.iterator if first < second) yield (first, second)
    }.toSeq.distinct.sorted.map { case (position, otherPosition) => (members(position), members(otherPosition)) }
  }

  /** Do two nodes bill one house's relay of one stage work in one season ([[RelayStage.oneRelay]]), as their venues
   *  published them? */
  private def oneRelay(first: EvidenceNode, second: EvidenceNode): Boolean =
    RelayStage.oneRelay(first.evidence.measured, second.evidence.measured)
  /** Does `other` hold a record naming another house than the relay `node` bills ([[StageRelays.billsAnotherHouse]])? One
   *  relay's billings are one relay, not one house's record another took: PL Multikino's "Balet z Opery Paryskiej
   *  2026-2027: Jezioro łabędzie" took the Royal Ballet & Opera's 2026/27 "Swan Lake" record (judged wrong, another
   *  house's), and Kino 1410's "Jezioro łabędzie | Paris Opera Ballet In Cinemas" is no reason to take it too. */
  private def holdsAnotherHousesRecord(node: EvidenceNode, otherFilm: Option[Int]): Boolean =
    otherFilm.flatMap(candidateOf).exists(candidate => StageRelays.billsAnotherHouse(node.evidence.measured, candidate.film))

  /** Does the rest of `decorated`'s title name a film of its own, beside the title `whole` — a
   *  candidate it may still take whose naming pieces share no word with `whole`? "Lalka (Dolly)"
   *  carries "Lalka" whole, but its "Dolly" names Blackhurst's film: it is not merely a decorated
   *  "Lalka", and the segment must not decide between the two for it. */
  private def namesBeside(decorated: EvidenceNode, whole: String): Boolean = {
    val words = services.titles.TitleContainment.tokens(whole).toSet
    families.scopeOf(decorated).of(decorated).exists { scored =>
      val pieces = scoring.namingPieces(decorated, scored.candidate)
      !scored.denied && pieces.nonEmpty && pieces.forall(piece => (piece.toSet intersect words).isEmpty)
    }
  }

  /** What one catalogue id binds `node` by: the id itself — a chain's own catalogue lists one film under one id — but a
   *  feed's ([[SourceReliability.linkDecides]]) only beside the node's title: an aggregator links screenings to its entries
   *  and can link two venues' different films to one (Filmstarts' placeholder 51988 "Film Program"), so its id joins
   *  only listings whose titles already agree. */
  private def catalogueKey(id: CatalogueId, node: EvidenceNode): String =
    if (!SourceReliability.linkDecides(id)) s"${id.key}\u0000${sanitized(node.evidence.cleanTitle)}" else id.key

  /** The keys `node`'s catalogue ids bind it by ([[catalogueKey]]). */
  def catalogueKeys(node: EvidenceNode): Seq[String] = node.listings.flatMap(_.catalogueIds).distinct.map(catalogueKey(_, node))

  private def sharesCatalogueId(first: EvidenceNode, second: EvidenceNode): Boolean = {
    val keys = catalogueKeys(first)
    keys.nonEmpty && catalogueKeys(second).exists(keys.contains)
  }

  /** The edges between `members`, each node's film (accepted or voted) given by `filmOf`. */
  def of(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] =
    pinned.of(members, filmOf) ++ pairsSharingAKey(members).flatMap { case (first, second) =>
      val pair = ConstraintEdges.Pair(first, second, filmOf(first.id), filmOf(second.id))
      def edge(must: Boolean, tier: Int, reason: String) = ResolverEdge(first.id, second.id, must, tier, reason)
      val cannots = cannotLinks(pair)
      cannots.map(edge(must = false, 0, _)) ++ mustLinks(pair).sortBy(_._1).take(1).map { case (tier, reason) => edge(must = true, tier, reason) }
    }.filter(pinned.admitted)

  /** Why a pair is kept apart. One chain's one catalogue id is one film (a feed's, under one title: `catalogueKey`): the
   *  facts each venue publishes of it (a bare re-release dated by its title, "9 to 5 (2026)") never keep its listings
   *  apart, as a pin's group is never kept apart. The cluster's pooled evidence then names the film. */
  private def cannotLinks(pair: ConstraintEdges.Pair): Seq[String] = {
    import pair.{first, second, firstFilm, secondFilm}
    if (pair.sameFilm || sharesCatalogueId(first, second)) Nil
    else Seq(
      Option.when(firstFilm.isDefined && secondFilm.isDefined)("different-films"),
      Option.when(firstFilm.exists(families.denies(second, _)) || secondFilm.exists(families.denies(first, _)))("denies-film"),
      apart.of(first, second),
      Option.when(pair.eitherWay(sources.copiedOriginalJoinsAnotherFilm))(ConstraintEdges.CopiedOriginalApart),
      Option.when(pair.eitherWay(sources.reusedPageDeniesLink))(ConstraintEdges.ReusedPageApart),
      Option.when(sources.keptBesideOverruled(first, second) || sources.keptBesideOverruled(second, first))(ConstraintEdges.KeptOverruledApart),
      Option.when(sources.linkedApart(first, second))(ConstraintEdges.LinkedApart),
      Option.when(pair.eitherWay((node, _, nodeFilm, otherFilm) => sources.creditDeniesHeld(node, otherFilm, nodeFilm)))(ConstraintEdges.CreditApart)
    ).flatten.filterNot(reason => RuleAblation.disables("apart:" + DecisionTrace.id(reason)))
  }

  /** The must-links a pair's evidence draws, by tier: the strongest is the pair's edge. */
  private def mustLinks(pair: ConstraintEdges.Pair): Seq[(Int, String)] = {
    import pair.{first, second, firstFilm, secondFilm}
    Seq(
      Option.when(pair.sameFilm)((1, "same-film")),
      // One chain lists them under one catalogue id: its one film, whatever each venue calls it (a feed's id, only
      // where their titles agree: `catalogueKey`).
      Option.when(sharesCatalogueId(first, second))((2, ConstraintEdges.SameCatalogueId)),
      titleLink(first, second),
      // One house's relay of one work in one season, however each venue spells the house beside the work: what no title
      // form says when no record joins them (UK "Opéra De Paris Ballet - La Bayadère", "La Bayadère (Opéra de Paris)").
      Option.when(oneRelay(first, second) && !holdsAnotherHousesRecord(first, secondFilm) && !holdsAnotherHousesRecord(second, firstFilm))(
        (3, ConstraintEdges.SameRelay)),
      // One film-less event, however each venue spells it beside its name ([[ListingCannotLinks.oneEvent]], the same words an article and
      // the edition aside), while neither holds a film: no record joins them, and their titles share no form
      // (UK "CBeebies Panto 2026: Treasure Island", "Cbeebies Panto: Treasure Island", "The CBeebies Panto 2026: …").
      Option.when(firstFilm.isEmpty && secondFilm.isEmpty && apart.oneEvent(first, second) && apart.sameEventName(first, second))(
        (3, ConstraintEdges.SameEvent))
    ).flatten.filterNot { case (_, reason) => RuleAblation.disables("join:" + reason) }
  }

  /** The titles' must-link, by the one title relation (TitleRelation.tier: same title, same search form, original
   *  title, whole segment). One listing's whole title a delimited SEGMENT of the other's ("Oficjalna premiera: Lalka"
   *  and "Lalka") joins the decorated spelling to its plain sibling's cluster, so group voting and the venue signal
   *  reach it. Whole segments only — "Zärtlich kreist die Faust" has no delimiter before "Faust" — and the ambiguity
   *  rule leaves a spelling whose segment names two films apart, as it does one whose rest names a film of its own
   *  (`namesBeside`). */
  private def titleLink(first: EvidenceNode, second: EvidenceNode): Option[(Int, String)] =
    links.tier(first, second).filter { case (tier, _) => tier != 4 ||
      (segmentOf(first, second) && !namesBeside(second, first.evidence.cleanTitle)) ||
      (segmentOf(second, first) && !namesBeside(first, second.evidence.cleanTitle)) }
}

private[identity] object ConstraintEdges {
  /** Two nodes sharing a block key, with the film each holds (accepted or voted). */
  private final case class Pair(first: EvidenceNode, second: EvidenceNode, firstFilm: Option[Int], secondFilm: Option[Int]) {
    def sameFilm: Boolean = firstFilm.isDefined && firstFilm == secondFilm
    /** Does `test` hold of the pair read either way round: (node, other, node's film, other's film)? */
    def eitherWay(test: (EvidenceNode, EvidenceNode, Option[Int], Option[Int]) => Boolean): Boolean =
      test(first, second, firstFilm, secondFilm) || test(second, first, secondFilm, firstFilm)
  }

  /** The must-link reasons a title or a catalogue id gives — the ones [[Corroboration.Vouching]] lets vouch for a node. */
  val SameTitle        = "same-title"
  val SameCatalogueId  = "same-catalogue-id"
  val SameSearchForm   = "same-search-form"
  val TitleSegment     = "title-segment"
  val OriginalTitle    = "original-title"
  /** The must-link of two billings of one film-less event, its name spelt alike ([[agreement.NonFilmEvents.Facts]]). */
  val SameEvent        = "same-event"
  /** The must-link of two billings of one house's relay of one stage work in one season ([[RelayStage.oneRelay]]). */
  val SameRelay        = "same-relay"
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
