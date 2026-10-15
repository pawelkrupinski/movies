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
            imdbTitles: Map[Int, Set[String]] = Map.empty, country: Option[String] = None, linked: Set[Int] = Set.empty,
            decided: Option[IdentityMeasures.Listing] = None): Seq[Scored] = {
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
    // What backs a namesake past the veto is a fact its source DECIDES ([[SourceReliability]]): the listing's facts
    // a feed did not copy (`decided`), another venue's own claim — never a feed entry's claim, however many venues it is
    // copied onto. DE City-Kinos' "The Killer" [2024] {John Woo} [125′], Filmstarts' entry for Woo's streaming-only 2024
    // remake, took it beside the 1989 film whose 4K restoration seven venues date and Germany saw released.
    def ownFacts(scored: Scored): Boolean = decided.fold(acceptance.weights.factsSupport(scored)) { stated =>
      val candidate = scored.candidate
      acceptance.weights.factsSupport(scored.copy(measures = IdentityMeasures.creditedByTitle(stated, candidate.film, IdentityMeasures.creditedBySearch(
        IdentityMeasures.listingFilmTitled(stated, candidate.film, ranks.get(candidate.tmdbId), 0, 0, relationOf(candidate.tmdbId), country),
        directedBy(candidate.tmdbId)))))
    }
    val vetoed = country.fold(Map.empty[Int, String])(ReleaseVeto.of(_, measured, scored =>
      relation(scored.candidate.tmdbId) == "exact" && !onlySuggested(scored.candidate.tmdbId),
      scored => backing.ownClaims((groups ++ IdentityMeasures.searchGroups(listing, scored.candidate.film)).distinct, scored.candidate.film, venue) > 0 ||
        ownFacts(scored)))
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
    // A billing naming the director of a film its title names ([[IdentityMeasures.bannerNamesItsDirector]]: a series'
    // banner, "Hitchcock & Chabrol: Spellbound") says whose film of the title it shows: a namesake none of the directors
    // it names made is not it, however popular — the 2024 animated "Spellbound" is no Hitchcock.
    val directorBilled = candidates.exists(scored => !scored.denied && IdentityMeasures.TitlesItsOwn(relation(scored.candidate.tmdbId)) &&
      IdentityMeasures.bannerNamesItsDirector(listing, scored.candidate.film))
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
    // A short beside a feature the title names as its own is that feature's namesake, unless the venue bills a short: no
    // rule takes it, and it stays a rival ([[ShortFilms]]): DE "GINA - mit der Domschule Würzburg" is no 7-minute "Gina"
    // beside three feature-length ones. So is a short the billing opens on and goes on past ([[ShortFilms.billedBeyond]]): DE
    // "UNLESS WE DANCE - QUEERE FILME ZUR LÄNGSTEN NACHT" is an evening named after the 14-minute short, no feature beside it.
    // A short of the director the listing credits is its own film, unless a feature of that director carries the title
    // too (DE Filmmuseum München's "Nuisance Bear" {Osio Vanden, Weisman}: their 2026 feature, not their 2021 short).
    val features = candidates.filter(scored => !scored.denied && !ShortFilms.isShort(scored.candidate.film) &&
      IdentityMeasures.TitlesItsOwn(relation(scored.candidate.tmdbId)))
    val creditedFeature = features.exists(scored => IdentityMeasures.sameDirector(scored.measures))
    def shortUntaken(scored: Scored) =
      if (scored.denied || (IdentityMeasures.sameDirector(scored.measures) && !creditedFeature)) None
      else ShortFilms.untaken(listing, scored.candidate.tmdbId, scored.candidate.film, linked,
        features.nonEmpty || ShortFilms.isShort(scored.candidate.film) && !IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) &&
          ShortFilms.billedBeyond(listing, scored.candidate.film, venue))
    // A film of the venue's own language is billed by its title: one the listing's titles share no word with is another
    // film, however its director's credit or running time fits — a director's filmography is a path to candidates, never a
    // title. UK Picture House Uckfield's "Gremlins Special Event" {Joe Dante} [120′] took Innerspace,
    // "Social Reckoning" {Aaron Sorkin} [130′] The Trial of the Chicago 7, US "Sabotage (1937)" Hitchcock's Young and
    // Innocent (prod 2026-10-14: every such take in the UK, DE and US wrong). A film of another language may be billed by a
    // translation TMDB does not carry (PL "Kupiłem motocykl, który jest wampirem", the 1990 British film): left to the facts.
    // Only one that would outrank every other candidate: a lower one is taken by nothing, and denying it anyway moved the
    // family's vote — UK "Fallen Angels by Noël Coward" ×315, a stage capture, lost its film or took Lukas Dhont's "Coward"
    // once its listings denied Scott Ellis's other Roundabout captures ("Harvey", "She Loves Me") ranked far below it
    // (convergence 38670105172).
    def untitled(scored: Scored) = !scored.denied && relation(scored.candidate.tmdbId) == "none" &&
      country.exists(venue => scored.candidate.film.countries.exists(_.exists(IdentityMeasures.sharesLanguage(venue, _)))) &&
      IdentityMeasures.sharesNoWord(listing, scored.candidate.film)
    lazy val named = candidates.filterNot(scored => scored.denied || untitled(scored)).map(_.probability).maxOption
    def ownLanguageUntitled(scored: Scored) = untitled(scored) && named.forall(scored.probability >= _)
    candidates.map(scored =>
      if (ownLanguageUntitled(scored))
        scored.copy(denial = Some("a film of the venue's own language its title shares no word with"))
      else if (anotherRecordOfItsBroadcast(scored))
        scored.copy(denial = Some("another record of a broadcast its film database holds twice"))
      else if (titled && !IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures))
        scored.copy(denial = Some("its director's other film, which the title does not name"))
      else if (directorBilled && IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) &&
               !IdentityMeasures.bannerNamesItsDirector(listing, scored.candidate.film))
        scored.copy(denial = Some("a namesake of the film whose director its billing names"))
      else if (instalment && scored.category("numeral").exists(IdentityMeasures.OtherInstalment))
        scored.copy(denial = Some("another instalment than the one the title numbers"))
      else if (housed.nonEmpty && besideItsOwnHouse(scored))
        scored.copy(denial = Some("another house's staging of its work, beside its own house's record"))
      else scored.copy(untaken = shortUntaken(scored)))
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

  /** `node`'s evidence ([[evidenceOf]]) with only the facts their source DECIDES ([[SourceReliability.Trust.Decide]]) —
   *  a fact the listing states read as its own source's, one only its page states as the page's — or `None` when every
   *  fact it holds is so: what may back a film past the release veto. */
  private[identity] def decidedOf(node: EvidenceNode): Option[IdentityMeasures.Listing] = {
    import SourceReliability.Fact
    val listing = node.listings.head
    val stated  = evidenceOf(node)
    def decides(fact: Fact, ownField: Boolean) = SourceReliability.decides(if (ownField) listing else listing.copy(pageFacts = true), fact)
    val decided = stated.copy(
      year          = stated.year.filter(_ => decides(Fact.Year, listing.year.isDefined)),
      directors     = if (decides(Fact.Directors, listing.directors.nonEmpty)) stated.directors else Nil,
      runtime       = stated.runtime.filter(_ => decides(Fact.Runtime, listing.runtime.isDefined)),
      originalTitle = stated.originalTitle.filter(_ => decides(Fact.OriginalTitle, listing.originalTitle.isDefined)))
    Option.when(decided != stated)(decided.measured)
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
  def of(node: EvidenceNode): Seq[Scored] = memo.getOrElseUpdate(node.id,
    { val reached = reach(node)
      score(evidenceOf(node).measured, node.venue, reached.search, reached.walk, sharedOf(node),
        id => denialByNode(node, id), reached.imdbOnly, reached.directed, reached.imdbTitled, node.country, feedLinked(node).map(_._2).toSet,
        decidedOf(node)).map(placedByImdb(Seq(node)))
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

  /** Why `node`'s original title, a FEED's copy ([[SourceReliability.copiesOriginalTitle]]), denies it `candidate`: only that
   *  copied title names it, never the billed title. The copied title is the feed entry's, restated — the entry's claim of
   *  which film it is ([[FeedLink]]) — so a namesake it alone names is the copy's pull, not the venue's film; and the year,
   *  director and running time the feed copied beside it agree with that entry by construction, so they confirm nothing
   *  (one origin, counted once). A TMDB-keyed feed's link names the entry's film, which stays the node's to take. DE
   *  Mathäser's "Rodychi (UKRAJINA CINEMA)": Cineamo links Chereshnya's "Родичі" (1658840m) and copies its English title
   *  "Kin", which took Baker's 2018 "Kin"; "Karavan" took the 1946 "Caravan", "Akten des Missbrauchs" Zobel's "The Hunt".
   *  Measured on convergence run 38529260149's DE model takes: 7 listings took a film only the copied title of another
   *  linked film named, all 7 wrong. A page feed names no film, its copy no less one when it shares nothing with the
   *  billing ([[IdentityMeasures.Listing.originalSharesNothingBilled]]): DE Cinecitta's "Über Unterbiberger - Vom Woid in
   *  die Welt" links kinoheld's page of another event, which copied Eisenstein's "Октябрь", his credit and its 142
   *  minutes, and took "Oktober 1917" at 61.9% while thirteen venues billed Ditscherlein's 2025 documentary. */
  private def namedOnlyByCopiedTitle(node: EvidenceNode, candidate: Candidate): Option[String] =
    copyingLink(node).map { case (id, linked) => (s"${id.source} ${id.id}", Some(linked)) }
      // a page feed's copy, which names no film: on a node no TMDB-keyed feed links — whose link, where this resolve's
      // films are a family's own numbers, is read as nothing, its copied title the linked film's own (DE "Der Film 444
      // und ein Treffen …") — and only a copy sharing nothing with the billing: one spelling the billed title, or
      // translating it word for word, is the venue's work restated (DE "Mein Opa der Bankräuber" copied as "Mijn Opa de
      // Bankrover", "Kardes Türküler ile 30 yil" as "Kardeş Türküler ile 30 Yıl")
      .orElse(Option.when(FeedLink.linked(node).isEmpty && node.listings.forall(SourceReliability.copiesOriginalTitle) &&
        evidenceOf(node).measured.originalSharesNothingBilled)(sourceOf(node) -> None))
      .filter(_._2.forall(_ != candidate.tmdbId)).flatMap { case (feed, linked) =>
      val listing = evidenceOf(node).measured
      // a page feed's copy names no film, so the record must not carry the billing's words either: one sharing two of
      // them, which a mere spelling keeps from naming it, is the billing's ("Madoka Magica - The Movie: Rebellion" is
      // "Mahou Shoujo Madoka Magica the Movie (Part 3): The Story of the Rebellion"); one word is a namesake's chance
      // ("Über Unterbiberger - Vom Woid in die Welt" and "Oktober 1917 – Zehn Tage, die die Welt erschütterten")
      def billsItsWords = linked.isEmpty &&
        (Seq(candidate.film.title) ++ candidate.film.originalTitle).exists(title => listing.wordsBilledOf(title) >= 2)
      // a year the listing states that the film shares — the linked film's, copied — makes the two one film under two
      // records: DE "Tabaluga und Leo - Die Weihnachtsgeschichte" [2003] links 1486412m and is the 2003 film 68030
      def sameYear = listing.year.exists(year => candidate.film.year.exists(film => math.abs(film - year) <= 1))
      Option.when(listing.originalTitle.nonEmpty && !sameYear && !billsItsWords &&
        IdentityMeasures.names(titleRelation(listing, candidate).value, listing, candidate.film) && {
          // the billed titles — without the copied original, nor a search title that is only it again — against the
          // record as its database holds it: the venue titles the corpus learned for it by that very original
          // ([[IdentityMeasures.venueTitles]]) are the copy's pull again
          val copied = listing.originalTitle.map(IdentityMeasures.key)
          val billed = listing.copy(originalTitle = None, searchTitles = listing.searchTitles.filterNot(title => copied.contains(IdentityMeasures.key(title))))
          val record = scoring.generation.recorded.get(candidate.tmdbId).fold(candidate.film)(_.film)
          // nor by one word a first name runs into, no delimited piece of the billing: DE Centraltheater Brake's "RAY
          // WILSON", a singer's concert Cineamo links to the 1944 "Wilson" and copies that title from, named "Wilson - Der
          // Weltverbesserer" (2017) only by the surname (prod 2026-10-14)
          val pieces = IdentityMeasures.namingPieces(billed, record)
          def byOneWordOnly = pieces.nonEmpty && pieces.forall(_.sizeIs == 1) && !billed.shapeKeys.exists(key => record.forms.exists(_.key == key))
          !IdentityMeasures.names(IdentityMeasures.titleRelation(billed, record, houses, qualifiers).value, billed, record) || byOneWordOnly
        })(s"named only by the title its feed ($feed) copied" + linked.fold("")(film => s" from $film"))
    }

  /** Why `node` rules `id` out before its evidence is scored: a pin, an overruled feed link, a short its feed links
   *  another film beside, a feed link's copied title, a title naming it only by the venue's own name, a programme tag or
   *  slot, a title billing a film festival alone, or a billing as a silent film of a sound-era film. */
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
    val linked  = cluster.flatMap(node => feedLinked(node).map(_._2)).toSet
    val ranks = reached.flatMap(_.search).groupMapReduce(_._1)(_._2)(math.min)
    score(listing, lead.venue, ranks, reached.flatMap(_.walk).toSet, cluster.flatMap(sharedOf).toSet,
      id => cluster.iterator.flatMap(denialByNode(_, id)).nextOption(), reached.flatMap(_.imdbOnly).toSet -- ranks.keySet,
      reached.flatMap(_.directed).toSet,
      reached.flatMap(_.imdbTitled).groupMapReduce(_._1)(_._2)(_ ++ _), lead.country, linked)
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
