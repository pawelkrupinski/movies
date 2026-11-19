package services.identity

import scala.collection.mutable

/** One family's scoring: every member node's candidates — the family's pool — scored on the node's
 *  own evidence ([[of]]), or on a cluster's evidence pooled into one listing ([[pooled]]). A `fallback` scope's pool is
 *  instead the members' fallback-source films ([[CandidateGeneration.fallbackOf]]), scored by the very same measures and
 *  denials: what a cluster no TMDB film was taken for may fall back to.
 *
 *  A film whose record is not answered yet is in the pool as its hits carry it, its record's facts unknown -- neither
 *  for it nor against it (the owner, 2026-10-31, identity-flips-structural step 6: decide from the inputs as they stand,
 *  unanswered questions treated as unknown). The decision is the same as on any other input, and Pending on the record
 *  ([[ResolverDecision.awaits]]): provisional until it is filed, never held to a previous take nor to none here (the agreement stage may serve a
 *  cluster that stood settled on a film that take, Pending, meanwhile: `agreement.AgreementStage.standing`). It replaces
 *  AGREE OR NONE (2026-10-29), which took no film where the pool with and without the pending films disagreed: UK
 *  flicks.co.uk "Rose (2023)" now serves "Rose Parade 2023", its search's one hit, provisionally until the parade's record
 *  denies it, and the pending tally (IdentityDrift) counts how often such a take differs from the final one. */
private[identity] final class FamilyScope(val members: Seq[EvidenceNode], scoring: CandidateScoring, acceptance: Acceptance,
                                          counted: () => Unit, related: () => Unit = () => (), fallback: Boolean = false) {
  import scoring.generation.{candidateById, candidateOf, fallbackOf, sharedOf}

  val pool: Seq[Candidate] =
    if (fallback) members.flatMap(member => fallbackOf(member.id)).distinct.sorted.flatMap(candidateOf)
    else members.flatMap(member => scoring.generation.reached(member)).distinct.sorted.map(candidateById)

  private val family  = new FamilyPool(pool, scoring.houses, related)
  private val links   = new FeedLinkOverrule(family, scoring.generation.numbersFilmsByTmdbId, scoring.backing)
  private val denials = new NodeDenials(scoring, links, new CopiedTitleDenial(family, links, scoring.generation.recorded))

  /** The qualifiers the pool's records bill ([[FamilyPool.qualifiers]]). */
  def qualifiers: IdentityMeasures.Qualifiers = family.qualifiers

  /** Every candidate `l` has an evidence path to, scored ([[ListingScoring]]); a `denial` marks, with its reason, the ones
   *  its node (`deniedByNode`: a pin, a venue's own name, an overruled feed link) or its own evidence rules out
   *  (`CandidateScoring.evidenceDenial`), which are never eligible. */
  def score(listing: IdentityMeasures.Listing, venue: String, source: String, ranks: Map[Int, Int], walked: Set[Int], shared: Set[Int],
            deniedByNode: Int => Option[Denial], suggested: Set[Int] = Set.empty, directedBy: Set[Int] = Set.empty,
            imdbTitles: Map[Int, Set[String]] = Map.empty, country: Option[String] = None, linked: Set[Int] = Set.empty,
            names: String => Boolean = _ => false): Seq[Scored] = {
    counted()
    new ListingScoring(family, scoring, acceptance, listing, venue, source, ranks, walked, shared, deniedByNode, suggested,
      directedBy, imdbTitles, country, linked, names).scored
  }

  /** The TMDB film a TMDB-keyed feed links `node` to ([[FeedLinkOverrule.linked]]). */
  private[identity] def feedLinked(node: EvidenceNode): Option[(CatalogueId, Int)] = links.linked(node)

  /** The film a TMDB-keyed feed links `node` to ([[feedLinked]]), unless the title's other sources overrule the link ([[overruled]]). */
  def standingFeedLink(node: EvidenceNode): Option[Int] = feedLinked(node).filter(_ => overruled(node).isEmpty).map(_._2)

  /** `node`'s feed link, when the title's other sources overrule it ([[FeedLinkOverrule.overruled]]). */
  def overruled(node: EvidenceNode): Option[FeedLink] = links.overruled(node)

  /** `node`'s evidence as this scope reads it ([[FeedLinkOverrule.evidenceOf]]): its titles alone when its feed link is
   *  overruled, else its own. */
  def evidenceOf(node: EvidenceNode): Evidence = links.evidenceOf(node)

  /** Does the venue's own text of any of `nodes`' listings name `name` WHOLE ([[VenueNames.names]]) — none of a node
   *  whose evidence was overruled to its titles ([[evidenceOf]])? What [[CreditMeasures.creditedByText]] credits by. */
  private def textNames(nodes: Seq[EvidenceNode]): String => Boolean = {
    val texts = nodes.filter(overruled(_).isEmpty).flatMap(_.listings.map(_.names)).filter(_.nonEmpty).distinct
    if (texts.isEmpty) _ => false else name => texts.exists(_.names(name))
  }

  /** What `node`'s own queries reached, as this scope reads its evidence ([[evidenceOf]]): an overruled link's node only
   *  what its titles' queries did — a search for the original title the feed copied is the link's own pull, restated. DE
   *  Cineamo's "Mira", linked to a record TMDB no longer holds, copied the original title "Last Signal", whose search
   *  ranked Kiselyov's 2022 film first and let IMDb's suggestion take it. Its pool stays the family's: a candidate the
   *  link alone reached is scored as any other the title names, with no rank. */
  private[identity] def reach(node: EvidenceNode): CandidateGeneration.Reach =
    if (overruled(node).isEmpty) scoring.generation.reach(node.id)
    else reachMemo.getOrElseUpdate(node.id, scoring.generation.reachOf(CandidateQueries.of(node.evidence.titlesOnly)))
  private val reachMemo = mutable.HashMap.empty[String, CandidateGeneration.Reach]

  /** Whose claim `node`'s facts are, for the sources backing a film ([[SourceReliability.claimOf]]). */
  private def sourceOf(node: EvidenceNode): String = SourceReliability.claimOf(node.listings.head)

  private val memo = mutable.HashMap.empty[String, Seq[Scored]]
  def of(node: EvidenceNode): Seq[Scored] = memo.getOrElseUpdate(node.id, scoreOf(node))
  private def scoreOf(node: EvidenceNode): Seq[Scored] = {
    val reached = reach(node)
    val soles   = soleResultsOf(evidenceOf(node).measured, reached)
    score(evidenceOf(node).measured, node.venue, sourceOf(node), reached.search, reached.walk, sharedOf(node),
      id => denials.of(node, id), reached.imdbOnly, reached.directed, reached.imdbTitled, node.country, feedLinked(node).map(_._2).toSet,
      textNames(Seq(node))).map(placedByImdb(Seq(node)))
      .map(scored => if (soles(scored.candidate.tmdbId)) scored.copy(soleResult = true) else scored)
      .map(scored => reached.imdbTitled.get(scored.candidate.tmdbId).fold(scored)(titles => scored.copy(imdbTitled = titles)))
  }

  /** The films that were the only result of one of `listing`'s title searches ([[Acceptance]]'s `sole-result`) — but for
   *  a billing naming no house, not of a search of its work beside a season ([[StageRelays.stageWorkQueries]]): those put
   *  the season's records in its pool for the broadcast join to tell apart by its days ([[agreement.Broadcast]]), and one
   *  record titled with that year is no answer to which house's staging a bare work is. UK Empire Halstead's "Carmen"
   *  [215′] and Riverside Woodbridge's "Carmen (2026)" screen the RBO 2026/27 relay on its day, 10 November 2026; "Carmen
   *  de Bizet 2026" found only Salzburg's festival Carmen, which they took. */
  private def soleResultsOf(listing: IdentityMeasures.Listing, reached: CandidateGeneration.Reach): Set[Int] =
    if (RelayStage.searchedInItsSeason(listing)) reached.soleResults
    else {
      val seasonSearches = StageRelays.stageWorkQueries(listing).map(_.trim).toSet
      if (seasonSearches.isEmpty) reached.soleResults else reached.soleResultsBeside(seasonSearches)
    }

  /** What `node` is taken by ALONE, with the rule ([[Acceptance.aloneNamed]]) — once per node for this scope's life:
   *  every round of `Families.grow` keeping the scope asks again, and so do the decisions, each node's trace for
   *  every title-linked sibling among them. */
  def takenAlone(node: EvidenceNode): Option[(Scored.Accepted, String)] = takenAloneMemo.getOrElseUpdate(node.id, acceptance.aloneNamed(of(node)))
  private val takenAloneMemo = mutable.HashMap.empty[String, Option[(Scored.Accepted, String)]]
  /** The rule `node` took `film` by alone, if it took that film. */
  def acceptedBy(node: EvidenceNode, film: Int): Option[String] = Acceptance.ruleTaking(takenAlone(node), film)

  /** Each film's place in IMDb's suggestions for the nodes' own titles — its best place, among the most suggested. */
  private def placedByImdb(nodes: Seq[EvidenceNode])(scored: Scored): Scored = {
    val lists = nodes.map(node => reach(node).imdbSuggested).filter(_.nonEmpty)
    val place = lists.flatMap(list => Option(list.indexOf(scored.candidate.tmdbId)).filter(_ >= 0).map(i => Scored.ImdbPlace(i + 1, list.size)))
    place.minByOption(_.place).fold(scored)(best => scored.copy(imdb = Some(best.copy(of = lists.map(_.size).max))))
  }

  /** The cluster's members read as ONE listing: the title most of its listings carry (the
   *  smaller node on a tie), the year most of them publish (a title's bracket or season stays the lead title's own measure), every director and country, the
   *  median runtime, the modal original title, and every candidate any of them named. The
   *  directors credited beside that year are only those of the members publishing it. */
  def pooled(cluster: Seq[EvidenceNode]): Seq[Scored] = pooledMemo.getOrElseUpdate(cluster.map(_.id), scorePooled(cluster))
  // Voting, the vote on a cluster's rest and the decisions each pool the same clusters again.
  private val pooledMemo = mutable.HashMap.empty[Seq[String], Seq[Scored]]

  private def scorePooled(cluster: Seq[EvidenceNode]): Seq[Scored] = {
    val lead    = cluster.minBy(node => (-node.weight, node.id))
    val listing = pooledListing(cluster, lead)
    val reached = cluster.map(reach)
    val linked  = cluster.flatMap(node => feedLinked(node).map(_._2)).toSet
    val ranks = reached.flatMap(_.search).groupMapReduce(_._1)(_._2)(math.min)
    score(listing, lead.venue, sourceOf(lead), ranks, reached.flatMap(_.walk).toSet, cluster.flatMap(sharedOf).toSet,
      id => cluster.iterator.flatMap(denials.of(_, id)).nextOption(), reached.flatMap(_.imdbOnly).toSet -- ranks.keySet,
      reached.flatMap(_.directed).toSet,
      reached.flatMap(_.imdbTitled).groupMapReduce(_._1)(_._2)(_ ++ _), lead.country, linked, textNames(cluster))
      .map(deniedByAMember(cluster))
      .map(placedByImdb(cluster))
      .map(scored => reached.flatMap(_.imdbTitled.getOrElse(scored.candidate.tmdbId, Set.empty)).toSet match {
        case titles if titles.nonEmpty => scored.copy(imdbTitled = titles)
        case _                         => scored
      })
  }

  /** The cluster read as one listing ([[pooled]]), under its `lead`'s titles. */
  private def pooledListing(cluster: Seq[EvidenceNode], lead: EvidenceNode): IdentityMeasures.Listing = {
    import FamilyScope.modal
    val evidence = cluster.map(node => node -> evidenceOf(node))
    val runtimes = evidence.flatMap { case (node, stated) => stated.runtime.toSeq.flatMap(runtime => Seq.fill(node.weight)(runtime)) }.sorted
    val year     = modal(evidence.flatMap { case (node, stated) => stated.year.map(_ -> node.weight) })
    evidenceOf(lead).measured.copy(
      year          = year,
      yearCredits   = Some(evidence.collect { case (_, stated) if year.nonEmpty && stated.year == year => stated.directors }.flatten.distinct.sorted),
      originalTitle = modal(evidence.flatMap { case (node, stated) => stated.originalTitle.map(_ -> node.weight) }),
      directors     = evidence.flatMap(_._2.directors).distinct.sorted,
      runtime       = runtimes.lift(runtimes.size / 2),
      countries     = evidence.flatMap(_._2.countries).distinct.sorted,
      // a certificate rates one listing's screening of its film: the cluster read as one listing states none
      certificate   = None)
  }

  /** A pooled candidate a member's own evidence rules out is denied the cluster too. */
  private def deniedByAMember(cluster: Seq[EvidenceNode])(scored: Scored): Scored =
    if (scored.denied || cluster.forall(node => !of(node).exists(other => other.candidate.tmdbId == scored.candidate.tmdbId && other.denied))) scored
    else scored.copy(denial = RuleAblation.live(Some(Denial("a member's own evidence rules it out"))))
}

private[identity] object FamilyScope {
  /** The value of the greatest summed weight, the smallest on a tie. */
  private def modal[A: Ordering](values: Seq[(A, Int)]): Option[A] =
    values.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (value, weight) => (-weight, value) }.headOption.map(_._1)

  /** What a title relation reads of a listing ([[FamilyScope]]'s relations). The decorations by identity:
   *  one resolve's listings share its instance, and its structural hash was itself a cost. */
  final class TitleInputs(listing: IdentityMeasures.Listing) {
    private val title         = listing.title
    private val rawTitle      = listing.rawTitle
    private val originalTitle = listing.originalTitle
    private val searchTitles  = listing.searchTitles
    private val decorations   = listing.decorations
    override val hashCode: Int = (title, rawTitle, originalTitle, searchTitles).## * 31 + System.identityHashCode(decorations)
    override def equals(other: Any): Boolean = other match {
      case that: TitleInputs => (that.decorations eq decorations) && that.title == title && that.rawTitle == rawTitle &&
        that.originalTitle == originalTitle && that.searchTitles == searchTitles
      case _ => false
    }
  }
}
