package services.identity

/**
 * THE EVIDENCE PROVENANCE TABLE (docs/investigations/2026-11-01-flip-unification.md, stage 3): what each kind of evidence
 * may do, said once. Every rule that weighs one listing's evidence for another reads it here -- the backing that counts
 * venues for a film, the overrule of a feed's link, the kept listing's join, a sibling's denial withdrawing a member's own
 * match -- instead of re-deriving it at each reader, where two readers disagreed (UK Keswick: a credit spared a node in
 * the group vote, not from a sibling's withdrawal) or one counted what another excluded (DE "Eternity": a kept listing
 * backed a present listing's feed link toward its overrule).
 *
 * The PROVENANCE of a piece of evidence ([[Provenance]]): the listing's own statement, a present sibling's, a KEPT
 * sibling's ([[Listing.kept]]: it left, its title is still billed), a feed's copy of facts onto every venue it lists.
 * The ACTS ([[Act]]): back a film for its own node; corroborate a sibling's film (count as a venue backing it); count
 * toward overruling a present listing's feed link; join a present listing whose feed link the present sources overrule;
 * withdraw a member's own match by its denial. The table ([[Table]]) is today's rules written once:
 *
 *   | provenance | back | corroborate | overrule | join an overruled | withdraw |
 *   |------------|------|-------------|----------|-------------------|----------|
 *   | own        | yes  | -           | yes      | -                 | -        |
 *   | present    | yes  | yes         | yes      | yes               | yes      |
 *   | kept       | yes  | yes         | NO       | NO                | yes      |
 *   | feed copy  | yes  | once a feed | once     | yes               | yes      |
 *
 * A kept listing keeps the family deciding as if it were present (retention's purpose: the take reads the same input
 * while its title is billed, so a departure moves nothing), so it backs, corroborates and withdraws as it did; it never
 * makes a present listing's own link lose to the rest (DE "Eternity", 871125cf7). A feed's copied facts count once per
 * feed toward an overrule ([[VenueBacking.sources]]).
 *
 * The WITHDRAWAL rows ([[spares]]): a sibling's denial of a film withdraws a member's own match of it unless the member's
 * own statement answers that very denial -- its banner billing the record's house when the denial is another house's
 * season (two houses' stagings are two productions); its credit of the film's director when the denial is that the film
 * is not the WORK billed and a credit overrules that (a screen namesake of a stage house's work, UK Keswick #629). The
 * group vote reads the same rows ([[ClusterVoting]]).
 */
private[identity] object Authority {

  /** Whose statement a piece of evidence is. */
  enum Provenance {
    case Own, Present, Kept, Feed
  }

  /** What a piece of evidence may be counted for. */
  enum Act {
    case Back, Corroborate, Overrule, JoinOverruled, Withdraw
  }

  import Act.*
  import Provenance.*

  /** THE TABLE: what each provenance may do. */
  val Table: Map[Provenance, Set[Act]] = Map(
    Own     -> Set(Back, Overrule),
    Present -> Set(Back, Corroborate, Overrule, JoinOverruled, Withdraw),
    Kept    -> Set(Back, Corroborate, Withdraw),
    Feed    -> Set(Back, Corroborate, Overrule, JoinOverruled, Withdraw))

  def may(provenance: Provenance, act: Act): Boolean = Table(provenance)(act)

  /** A sibling listing's provenance: kept, or present. */
  def of(listing: Listing): Provenance = if (listing.kept) Kept else Present

  /** A node's provenance as a sibling: kept when every listing of it is kept, else present. */
  def of(node: EvidenceNode): Provenance = if (node.listings.forall(_.kept)) Kept else Present

  /** A backing entry's provenance ([[Backer]]): kept when it is a kept listing's. */
  def ofBacker(key: String): Provenance = if (Backer.kept(key)) Kept else Present

  /** May a backing entry count for `act`? */
  def counts(key: String, act: Act): Boolean = may(ofBacker(key), act)

  /** The WITHDRAWAL rows: why `member`'s own match is spared a sibling's denial of `kind`, if its own statement answers
   *  that denial -- its banner billing the record under its house against another house's season; its credit of the
   *  film's director against a "not the work" denial a credit overrules. */
  def spares(member: Scored, kind: Denial.Kind): Option[String] = kind match {
    case Denial.Kind.AnotherHousesSeason if member.houseProduction =>
      Some("its banner bills the record under its house")
    case Denial.Kind.NotTheWork(true) if IdentityMeasures.sameDirector(member.measures) =>
      Some("it credits the film's director")
    case _ => None
  }

  /** Is `member`'s own match spared EVERY sibling's withdrawal? Its own title names the record's production -- its house
   *  by its banner and its season ([[Scored.seasonProduction]], [[StageRelays.billsItsHouse]]), or its relay's record of
   *  its house, work and season ([[RelayStage.particularRecordOf]]): one house stages a work once a season, so a
   *  sibling's denial says the sibling is another production, never that the member is. */
  def sparesEvery(member: Scored): Boolean =
    member.seasonProduction && StageRelays.billsItsHouse(member.listing, member.candidate.film) ||
      RelayStage.particularRecordOf(member.listing, member.candidate.film)

  /** May `sibling`'s `denial` withdraw `member`'s own match? Its provenance must, the member's statement must not
   *  answer the denial ([[spares]]), and the sibling's own facts must not split on it ([[splitDenial]]). */
  def withdraws(sibling: EvidenceNode, denial: Scored, member: Scored): Boolean =
    may(of(sibling), Withdraw) && !sparesEvery(member) && denial.denial.forall(d => spares(member, d.kind).isEmpty) &&
      !splitDenial(denial)

  /** A sibling's learned listing-film rule against a film whose director the sibling itself credits: its own facts split
   *  -- its credit names the film, its runtime or year (a Q&A event's length, a re-release's year) set it apart -- so
   *  the rule says the sibling's screening differs, never that the member's title means another film. The member keeps
   *  its own match; the denial still holds for the sibling. UK "Oxide Ghosts: The Brass Eye Tapes + Q&A": Cultplex
   *  (Michael Cumming, 21 years off, +30′) withdrew MAC's own match crediting him; UK "It's Nice Up North + Q&A": Hexham
   *  (Graham Fellows, +40′) withdrew Kendal's -- each none -> film -> none along an arrival chain. */
  def splitDenial(denial: Scored): Boolean =
    !denial.deniedByPin && denial.denial.exists(_.kind == Denial.Kind.Learned) && IdentityMeasures.sameDirector(denial.measures)
}
