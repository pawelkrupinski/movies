package services.identity

import services.identity.IdentityMeasures.ListingFilm

import scala.collection.mutable

/** One family's scoring: every member node's candidates — the family's pool — scored on the node's
 *  own evidence ([[of]]), or on a cluster's evidence pooled into one listing ([[pooled]]). A `fallback` scope's pool is
 *  instead the members' fallback-source films ([[CandidateGeneration.fallbackOf]]), scored by the very same measures and
 *  denials: what a cluster no TMDB film was taken for may fall back to. */
private[identity] final class FamilyScope(val members: Seq[EvidenceNode], scoring: CandidateScoring, acceptance: Acceptance,
                                          counted: () => Unit, related: () => Unit = () => (), fallback: Boolean = false) {
  import scoring.{backing, calibration, evidenceDenial, houses, namesItsSeasonProduction, namesOnlyAProgrammeSlot, namesOnlyATag, namesOnlyItsVenue, pins}
  import scoring.generation.{candidateById, candidateOf, fallbackOf, sharedOf}

  val pool: Seq[Candidate] =
    if (fallback) members.flatMap(member => fallbackOf(member.id)).distinct.sorted.flatMap(candidateOf)
    else members.flatMap(member => scoring.generation.reached(member)).distinct.sorted.map(candidateById)
  /** Which pieces of the members' titles are qualifiers — an edition, a banner — rather than
   *  works, learned from how the pool's records bill them (`IdentityMeasures.Qualifiers`): a
   *  record titled only a listing's qualifier does not name it. The family's own pool, so a
   *  family resolves alone as it does among the others. */
  val qualifiers: IdentityMeasures.Qualifiers = IdentityMeasures.Qualifiers.learn(pool.map(_.film))

  /** Every candidate `l` has an evidence path to, scored; a `denial` marks, with its reason, the ones
   *  its node (`deniedByNode`: a pin, a venue's own name, an overruled feed link) or its own evidence rules out
   *  (`CandidateScoring.evidenceDenial`), which are never eligible. */
  def score(listing: IdentityMeasures.Listing, venue: String, ranks: Map[Int, Int], walked: Set[Int], shared: Set[Int],
            deniedByNode: Int => Option[String], suggested: Set[Int] = Set.empty, directedBy: Set[Int] = Set.empty,
            imdbTitles: Map[Int, Set[String]] = Map.empty, country: Option[String] = None): Seq[Scored] = {
    counted()
    val related    = pool.map(candidate => candidate.tmdbId -> titleRelation(listing, candidate)).toMap
    // The ONE film IMDb lists under the listing's title (an AKA TMDB does not carry), when no film beside it carries the
    // title already: it carries it too, so the listing's facts are read against a film its title names. PL "Camino dla
    // opornych" [97′] read IMDb's Polish title of "Compostelle" as no relation, and its runtime alone denied the film.
    // A namesake of the listing's own title, not of the original it publishes beside it: Helios bills "Camino dla
    // opornych" with the original "Santiago", which Gordon Douglas's 1956 film is titled — the original is a fact its
    // own measure weighs, and IMDb's title names the film the listing's title is.
    lazy val untranslated = listing.copy(originalTitle = None)
    val titledBy   = IdentityMeasures.soleImdbTitled(imdbTitles).filter { case (id, _) =>
      // a film of the pool's, its IMDb title its to take, that no other film of the pool carries as its own
      this.pool.find(_.tmdbId == id).exists(candidate => IdentityMeasures.takesImdbTitle(listing, candidate.film)) &&
        !this.pool.exists(other => other.tmdbId != id &&
          IdentityMeasures.TitlesItsOwn(IdentityMeasures.titleRelation(untranslated, other.film, houses, qualifiers).value)) }
    val scoredPool = titledBy.fold(this.pool) { case (id, titles) => this.pool.map(candidate =>
      if (candidate.tmdbId == id) candidate.copy(film = IdentityMeasures.withVenueTitles(candidate.film, titles.toSeq.sorted)) else candidate) }
    val relationOf = titledBy.fold(related) { case (id, _) =>
      related.updated(id, IdentityMeasures.titleRelation(listing, scoredPool.find(_.tmdbId == id).get.film, houses, qualifiers)) }
    val relation   = relationOf.view.mapValues(_.value).toMap
    val reachable = scoredPool.filter(candidate => ranks.contains(candidate.tmdbId) || walked(candidate.tmdbId) || shared(candidate.tmdbId) ||
      IdentityMeasures.names(relation(candidate.tmdbId), listing, candidate.film))
    // A film only IMDb suggested, under a title the listing does not carry, rivals nothing: the rules other than
    // IMDb's own never see it, as before IMDb's other-language matches were followed.
    val onlySuggested = (id: Int) => suggested(id) && !IdentityMeasures.Rivalling(relation(id))
    val rivalling = (id: Int) => IdentityMeasures.Rivalling(relation(id)) && !onlySuggested(id)
    val close     = reachable.count(candidate => rivalling(candidate.tmdbId))
    val groups    = IdentityMeasures.titleGroups(listing)
    val measured = reachable.map { candidate =>
      val rivals   = close - (if (rivalling(candidate.tmdbId)) 1 else 0)
      val measures = IdentityMeasures.creditedByTitle(listing, candidate.film, IdentityMeasures.creditedBySearch(
        IdentityMeasures.listingFilmTitled(listing, candidate.film, ranks.get(candidate.tmdbId), rivals,
          backing.corroborating((groups ++ IdentityMeasures.searchGroups(listing, candidate.film)).distinct, candidate.film, venue),
          relationOf(candidate.tmdbId), country), directedBy(candidate.tmdbId)))
      val probability = calibration.probability(ListingFilm, measures)
      val byNode = deniedByNode(candidate.tmdbId)
      Scored(candidate, probability, measures, byNode.orElse(evidenceDenial(listing, candidate.film, measures)), listing, ranks.get(candidate.tmdbId),
        namesItsSeasonProduction(listing, candidate.film), byNode.isDefined, StageRelays.billsUnderItsHouse(listing, candidate.film, houses),
        suggestedOnly = onlySuggested(candidate.tmdbId))
    }
    // A namesake the venue's country never saw released, beside one it did, that nothing backs is denied, and no
    // rival in the evidence class of the namesakes left ([[ReleaseVeto]]). A namesake here is a film the title names
    // EXACTLY: an original or alternative title is the venue's spelling of another language's film, not its own.
    val vetoed = country.fold(Map.empty[Int, String])(ReleaseVeto.of(_, measured, scored =>
      relation(scored.candidate.tmdbId) == "exact" && !onlySuggested(scored.candidate.tmdbId),
      scored => scored.number("venues.corroborating").exists(_ > 0) || acceptance.weights.factsSupport(scored)))
    val candidates = if (vetoed.isEmpty) measured else measured.map { scored =>
      val id = scored.candidate.tmdbId
      if (vetoed.contains(id)) scored.copy(denial = scored.denial.orElse(vetoed.get(id)))
      else if (rivalling(id)) scored.copy(vetoedRivals = vetoed.size)
      else scored
    }
    // The listing's whole title (or its original or an alternative title) and its credited
    // director name ONE film together: another film of that director, which its title does not
    // name, is not the listing's — however its runtime or year fits. A director's filmography is a path to candidates, never a reason to leave
    // the one its title names (Syndicated's "Zodiac", Fincher, 139 minutes, is not Fight Club).
    val titled = candidates.exists(scored => !scored.denied && IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures))
    // The listing's title numbers its instalment as an eligible record of its series does
    // (`numeral` `same`): another instalment of that series — one the listing numbers and it
    // does not, or numbers otherwise — is another film, however its crew or runtime fits (Kinoteka's
    // "Niesamowite przygody skarpetek 4. Do roboty! – zestaw" is part 4, not the 2025 first set
    // whose animators it credits).
    val instalment = candidates.exists(scored => !scored.denied && scored.category("numeral").contains("same"))
    // A stage house's relay that an eligible record bills under its own house is not another house's staging of the
    // work, however its screen director's credit or running time fits that one: UK Odeon's "English National Ballet: The
    // Sleeping Beauty" {Ross MacGibbon} ×22 is ENB's 2026 record, not the Royal Ballet's 2017 one he also filmed. Only
    // beside its own house's: UK's "RBO Cinema Season" relays the Met's productions, which RBO never staged.
    // Its own house's by a banner word the other record lacks: "OPERA- SILENT NIGHT" names no house by "silent".
    val housed = if (listing.billsAStageHouse) candidates.filterNot(_.denied) else Nil
    def besideItsOwnHouse(other: Scored) = !other.denied && StageRelays.billsAnotherHouse(listing, other.candidate.film) &&
      housed.exists(own => own.candidate.tmdbId != other.candidate.tmdbId &&
        StageRelays.billsItsHouse(listing, own.candidate.film, other.candidate.film.titleWords))
    // One broadcast its film database holds twice is one film: the record every listing of it takes is the first in
    // `broadcastRecordOrder`, and the other is no listing's ([[StageRelays.sameBroadcast]]).
    def anotherRecordOfItsBroadcast(scored: Scored) = candidates.exists(other => other.candidate.tmdbId != scored.candidate.tmdbId &&
      StageRelays.sameBroadcast(scored.candidate.film, other.candidate.film) &&
      Ordering[(Int, Int, Int)].lt(StageRelays.broadcastRecordOrder(other.candidate.tmdbId, other.candidate.film),
        StageRelays.broadcastRecordOrder(scored.candidate.tmdbId, scored.candidate.film)))
    candidates.map(scored =>
      if (anotherRecordOfItsBroadcast(scored))
        scored.copy(denial = Some("another record of a broadcast its film database holds twice"))
      else if (titled && !IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures))
        scored.copy(denial = Some("its director's other film, which the title does not name"))
      else if (instalment && scored.category("numeral").exists(IdentityMeasures.OtherInstalment))
        scored.copy(denial = Some("another instalment than the one the title numbers"))
      else if (housed.nonEmpty && besideItsOwnHouse(scored))
        scored.copy(denial = Some("another house's staging of its work, beside its own house's record"))
      else scored)
      .sortBy(scored => (-scored.probability, scored.candidate.tmdbId))
  }

  // A title relation reads only the listing's titles — its title, raw, original and search titles and its
  // decorations — never its facts, beside the film and this scope's houses and qualifiers. Every node and
  // cluster of the family is related to the whole pool, so the nodes billing one title, and a cluster read
  // under its lead's titles, share the pool's relations instead of reading them again.
  private val relations = mutable.HashMap.empty[(FamilyScope.TitleInputs, Int), IdentityMeasures.Category]
  private def titleRelation(listing: IdentityMeasures.Listing, candidate: Candidate): IdentityMeasures.Category =
    relations.getOrElseUpdate((listing.titleInputs, candidate.tmdbId),
      { related(); IdentityMeasures.titleRelation(listing, candidate.film, houses, qualifiers) })

  /** The TMDB film a TMDB-keyed feed links `node` to ([[FeedLink.linked]]) — none where this resolve's films are a
   *  family's own numbers, which no TMDB id names ([[IdentityLookups.numbersFilmsByTmdbId]]). */
  private def feedLinked(node: EvidenceNode): Option[(CatalogueId, Int)] =
    if (scoring.generation.numbersFilmsByTmdbId) FeedLink.linked(node) else None

  /** [[feedLinked]], where the original title on `node` is the feed's copy, only Support ([[SourceReliability.Fact.OriginalTitle]]):
   *  the link the copied-title and short denials read — a venue's own original title is its claim, never a copy's pull. */
  private def copyingLink(node: EvidenceNode): Option[(CatalogueId, Int)] =
    feedLinked(node).filter(_ => node.listings.forall(listing => !SourceReliability.decides(listing, SourceReliability.Fact.OriginalTitle)))

  /** `node`'s feed link, when the title's other sources overrule it ([[FeedLink]]): once per node for this scope's life. */
  def overruled(node: EvidenceNode): Option[FeedLink] = overruledMemo.getOrElseUpdate(node.id, overrule(node))
  private val overruledMemo = mutable.HashMap.empty[String, Option[FeedLink]]
  private def overrule(node: EvidenceNode): Option[FeedLink] = feedLinked(node).flatMap { case (id, film) =>
    val listing = node.evidence.measured
    val groups  = IdentityMeasures.titleGroups(listing)
    val own     = sourceOf(node)
    // a film the title carries as one of its delimited pieces is backed by the venues billing that piece alone: DE Astor's
    // "ATMOS: Michael", which Cineamo links to Dreyer's 1924 film, is the "Michael" sixteen venues credit to Antoine Fuqua
    def sources(candidate: Candidate) = backing.sources((groups ++ IdentityMeasures.searchGroups(listing, candidate.film) ++
      listing.shapeKeys.filter(candidate.film.titleKeys)).distinct, candidate.film, own)
    if (pool.exists(candidate => candidate.tmdbId == film && sources(candidate) > 0)) None
    else pool.filter(candidate => candidate.tmdbId != film && IdentityMeasures.TitlesItsOwn(titleRelation(listing, candidate).value))
      .map(candidate => candidate.tmdbId -> sources(candidate)).filter(_._2 >= FeedLink.OverrulingSources)
      .sortBy { case (rival, count) => (-count, rival) }.headOption
      .map { case (rival, count) => FeedLink(id, film, rival, count) }
      .orElse(titledRival(node.evidence.titlesOnly.measured, film).map(rival => FeedLink(id, film, rival, FeedLink.TitledRival)))
  }
  /** The one film of the pool `titles` (a node's titles alone) name exactly, when they name the linked `film` not at all. */
  private def titledRival(titles: IdentityMeasures.Listing, film: Int): Option[Int] = {
    def relation(candidate: Candidate) = IdentityMeasures.titleRelation(titles, candidate.film, houses, qualifiers).value
    if (!pool.exists(_.tmdbId == film) || pool.exists(candidate => candidate.tmdbId == film && IdentityMeasures.NamingRelations(relation(candidate)))) None
    else pool.filter(candidate => candidate.tmdbId != film && relation(candidate) == "exact").map(_.tmdbId) match {
      case Seq(rival) => Some(rival)
      case _          => None
    }
  }

  /** `node`'s evidence as this scope reads it: its titles alone when its feed link is overruled ([[FeedLink]]) — the facts
   *  were the link's, restated — else its own. */
  def evidenceOf(node: EvidenceNode): Evidence = if (overruled(node).isEmpty) node.evidence else node.evidence.titlesOnly

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
  def of(node: EvidenceNode): Seq[Scored] = memo.getOrElseUpdate(node.id,
    { val reached = reach(node)
      score(evidenceOf(node).measured, node.venue, reached.search, reached.walk, sharedOf(node),
        id => denialByNode(node, id), reached.imdbOnly, reached.directed, reached.imdbTitled, node.country).map(placedByImdb(Seq(node)))
        .map(scored => if (reached.soleResults(scored.candidate.tmdbId)) scored.copy(soleResult = true) else scored)
        .map(scored => reached.imdbTitled.get(scored.candidate.tmdbId).fold(scored)(titles => scored.copy(imdbTitled = titles))) })

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

  /** Why a TMDB-keyed feed's link of `node` to ANOTHER film denies it `candidate`: only the original title the feed copied
   *  from the film it linked names it, never the billed title. The copied title is that film's, restated ([[FeedLink]]),
   *  so a namesake it alone names is the link's pull, not the venue's film. DE Mathäser's "Rodychi (UKRAJINA CINEMA)":
   *  Cineamo links Chereshnya's "Родичі" (1658840m) and copies its English title "Kin", which took Baker's 2018 "Kin";
   *  "Karavan" took the 1946 "Caravan", "Akten des Missbrauchs" Zobel's "The Hunt". Measured on convergence run
   *  38529260149's DE model takes: 7 listings took a film only the copied title of another linked film named, all 7 wrong.
   *  A feed that is not TMDB-keyed names no film its copy is of, and its copied title stays evidence. */
  private def namedOnlyByCopiedTitle(node: EvidenceNode, candidate: Candidate): Option[String] =
    copyingLink(node).filter(_._2 != candidate.tmdbId).flatMap { case (id, linkedFilm) =>
      val listing = evidenceOf(node).measured
      // a year the listing states that the film shares — the linked film's, copied — makes the two one film under two
      // records: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" [2003] links 1486412m and is the 2003 film 68030
      def sameYear = listing.year.exists(year => candidate.film.year.exists(film => math.abs(film - year) <= 1))
      Option.when(listing.originalTitle.nonEmpty && !sameYear &&
        IdentityMeasures.names(titleRelation(listing, candidate).value, listing, candidate.film) && {
          // the billed titles — without the copied original, nor a search title that is only it again — against the
          // record as its database holds it: the venue titles the corpus learned for it by that very original
          // ([[IdentityMeasures.venueTitles]]) are the copy's pull again
          val copied = listing.originalTitle.map(IdentityMeasures.key)
          val billed = listing.copy(originalTitle = None, searchTitles = listing.searchTitles.filterNot(title => copied.contains(IdentityMeasures.key(title))))
          val record = scoring.generation.recorded.get(candidate.tmdbId).fold(candidate.film)(_.film)
          !IdentityMeasures.names(IdentityMeasures.titleRelation(billed, record, houses, qualifiers).value, billed, record)
        })(s"named only by the title its feed (${id.source} ${id.id}) copied from $linkedFilm")
    }

  /** Why `node` rules `id` out before its evidence is scored: a pin, a feed link's copied title, a title naming it only
   *  by the venue's own name, a programme tag or slot, or a title billing a film festival alone. */
  private def denialByNode(node: EvidenceNode, id: Int): Option[String] =
    Option.when(pins.deniedFilms(node.listings.head.key)(id))("pinned never this film")
      .orElse(overruled(node).filter(_.film == id).map(_.denial))
      .orElse(copyingLink(node).filter(_ => FeedLink.copiesAnotherTitle(evidenceOf(node).measured)).flatMap(FeedLink.deniesShort(_, id, candidateById(id).film)))
      .orElse(namedOnlyByCopiedTitle(node, candidateById(id)))
      .orElse(Option.when(namesOnlyItsVenue(node, candidateById(id)))("its title names it only by the venue's own name"))
      .orElse(Option.when(namesOnlyATag(node, candidateById(id)))("its title names it only by a programme tag billed beside many titles"))
      .orElse(Option.when(namesOnlyAProgrammeSlot(node, candidateById(id)))("its title names it only by a festival's programme slot"))
      .orElse(Option.when(billingOf(node).festival)("its title bills a film festival by its name alone: the programme, no film"))
      // a billing as a silent film dates it to the silent era, as the agreement reads it (DE "STUMMFILM MIT TOBIAS
      // RANK:CHARLIE CHAPLIN" is no 1999 "Charlie Chaplin")
      .orElse(Option.when(candidateById(id).film.year.exists(_ > ListingShape.SilentEraEnd) && billingOf(node).silent)(
        "it bills a silent film, and this one is of the sound era"))

  /** What `node`'s billing says it is beside a film, once per node — asked of every candidate of its pool: a film
   *  festival billed by its name alone ([[ListingShape.billsAFilmFestival]]), a silent film ([[ListingShape.billsASilentFilm]]). */
  private final case class Billing(festival: Boolean, silent: Boolean)
  private def billingOf(node: EvidenceNode): Billing = billingMemo.getOrElseUpdate(node.id, {
    val listing = node.evidence.published
    Billing(ListingShape.billsAFilmFestival(listing.rawTitle.getOrElse(listing.title)), node.listings.exists(ListingShape.billsASilentFilm))
  })
  private val billingMemo = mutable.HashMap.empty[String, Billing]

  /** The cluster's members read as ONE listing: the title most of its listings carry (the
   *  smaller node on a tie), the year most of them publish (a title's bracket or season stays the lead title's own measure), every director and country, the
   *  median runtime, the modal original title, and every candidate any of them named. The
   *  directors credited beside that year are only those of the members publishing it. */
  def pooled(cluster: Seq[EvidenceNode]): Seq[Scored] = pooledMemo.getOrElseUpdate(cluster.map(_.id), scorePooled(cluster))
  // Voting, the vote on a cluster's rest and the decisions each pool the same clusters again.
  private val pooledMemo = mutable.HashMap.empty[Seq[String], Seq[Scored]]

  private def scorePooled(cluster: Seq[EvidenceNode]): Seq[Scored] = {
    def modal[A: Ordering](values: Seq[(A, Int)]): Option[A] =
      values.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (value, weight) => (-weight, value) }.headOption.map(_._1)
    val lead     = cluster.minBy(node => (-node.weight, node.id))
    val evidence = cluster.map(node => node -> evidenceOf(node))
    val runtimes = evidence.flatMap { case (node, stated) => stated.runtime.toSeq.flatMap(runtime => Seq.fill(node.weight)(runtime)) }.sorted
    val year     = modal(evidence.flatMap { case (node, stated) => stated.year.map(_ -> node.weight) })
    val listing  = evidenceOf(lead).measured.copy(
      year          = year,
      yearCredits   = Some(evidence.collect { case (_, stated) if year.nonEmpty && stated.year == year => stated.directors }.flatten.distinct.sorted),
      originalTitle = modal(evidence.flatMap { case (node, stated) => stated.originalTitle.map(_ -> node.weight) }),
      directors     = evidence.flatMap(_._2.directors).distinct.sorted,
      runtime       = runtimes.lift(runtimes.size / 2),
      countries     = evidence.flatMap(_._2.countries).distinct.sorted)
    val reached = cluster.map(reach)
    val ranks = reached.flatMap(_.search).groupMapReduce(_._1)(_._2)(math.min)
    score(listing, lead.venue, ranks, reached.flatMap(_.walk).toSet, cluster.flatMap(sharedOf).toSet,
      id => cluster.iterator.flatMap(denialByNode(_, id)).nextOption(), reached.flatMap(_.imdbOnly).toSet -- ranks.keySet,
      reached.flatMap(_.directed).toSet,
      reached.flatMap(_.imdbTitled).groupMapReduce(_._1)(_._2)(_ ++ _), lead.country)
      .map(scored => if (scored.denied || cluster.forall(node => !of(node).exists(other => other.candidate.tmdbId == scored.candidate.tmdbId && other.denied))) scored else scored.copy(denial = Some("a member's own evidence rules it out")))
      .map(placedByImdb(cluster))
      .map(scored => reached.flatMap(_.imdbTitled.getOrElse(scored.candidate.tmdbId, Set.empty)).toSet match {
        case titles if titles.nonEmpty => scored.copy(imdbTitled = titles)
        case _                         => scored
      })
  }
}

private[identity] object FamilyScope {
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
