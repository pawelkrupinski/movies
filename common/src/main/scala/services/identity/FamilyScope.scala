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
  import scoring.generation.{candidateById, candidateOf, directed, fallbackOf, imdbOnly, imdbSuggested, imdbTitled, ownSearch, ownWalk, sharedOf, soleResults}

  val pool: Seq[Candidate] =
    if (fallback) members.flatMap(member => fallbackOf(member.id)).distinct.sorted.flatMap(candidateOf)
    else members.flatMap(member => ownSearch(member.id).keys ++ ownWalk(member.id)).distinct.sorted.map(candidateById)
  /** Which pieces of the members' titles are qualifiers — an edition, a banner — rather than
   *  works, learned from how the pool's records bill them (`IdentityMeasures.Qualifiers`): a
   *  record titled only a listing's qualifier does not name it. The family's own pool, so a
   *  family resolves alone as it does among the others. */
  val qualifiers: IdentityMeasures.Qualifiers = IdentityMeasures.Qualifiers.learn(pool.map(_.film))

  /** Every candidate `l` has an evidence path to, scored; a `denial` marks, with its reason, the ones
   *  its node (`deniedByNode`: a pin, a venue's own name) or its own evidence rules out
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
      this.pool.exists(_.tmdbId == id) && !this.pool.exists(other => other.tmdbId != id &&
        IdentityMeasures.TitlesItsOwn(IdentityMeasures.titleRelation(untranslated, other.film, houses, qualifiers).value)) &&
        this.pool.find(_.tmdbId == id).exists(candidate => IdentityMeasures.takesImdbTitle(listing, candidate.film)) }
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
      val measures = IdentityMeasures.creditedBySearch(IdentityMeasures.listingFilmTitled(listing, candidate.film, ranks.get(candidate.tmdbId), rivals,
        backing.corroborating((groups ++ IdentityMeasures.searchGroups(listing, candidate.film)).distinct, candidate.film, venue),
        relationOf(candidate.tmdbId), country), directedBy(candidate.tmdbId))
      val probability = calibration.probability(ListingFilm, measures)
      val byNode = deniedByNode(candidate.tmdbId)
      Scored(candidate, probability, measures, byNode.orElse(evidenceDenial(listing, candidate.film, measures)), listing, ranks.get(candidate.tmdbId),
        namesItsSeasonProduction(listing, candidate.film), byNode.isDefined, IdentityMeasures.billsUnderItsHouse(listing, candidate.film, houses),
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
    candidates.map(scored =>
      if (titled && !IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures))
        scored.copy(denial = Some("its director's other film, which the title does not name"))
      else if (instalment && scored.category("numeral").exists(IdentityMeasures.OtherInstalment))
        scored.copy(denial = Some("another instalment than the one the title numbers"))
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

  private val memo = mutable.HashMap.empty[String, Seq[Scored]]
  def of(node: EvidenceNode): Seq[Scored] = memo.getOrElseUpdate(node.id,
    score(node.evidence.measured, node.venue, ownSearch(node.id), ownWalk(node.id), sharedOf(node),
      id => denialByNode(node, id), imdbOnly(node.id), directed(node.id), imdbTitled(node.id), countryOf(node)).map(placedByImdb(Seq(node)))
      .map(scored => if (soleResults(node.id)(scored.candidate.tmdbId)) scored.copy(soleResult = true) else scored)
      .map(scored => imdbTitled(node.id).get(scored.candidate.tmdbId).fold(scored)(titles => scored.copy(imdbTitled = titles))))

  /** What `node` is taken by ALONE, with the rule ([[Acceptance.aloneNamed]]) — once per node for this scope's life:
   *  every round of `Families.grow` keeping the scope asks again, and so do the decisions, each node's trace for
   *  every title-linked sibling among them. */
  def takenAlone(node: EvidenceNode): Option[(Scored.Accepted, String)] = takenAloneMemo.getOrElseUpdate(node.id, acceptance.aloneNamed(of(node)))
  private val takenAloneMemo = mutable.HashMap.empty[String, Option[(Scored.Accepted, String)]]
  /** The rule `node` took `film` by alone, if it took that film. */
  def acceptedBy(node: EvidenceNode, film: Int): Option[String] = Acceptance.ruleTaking(takenAlone(node), film)

  /** Each film's place in IMDb's suggestions for the nodes' own titles — its best place, among the most suggested. */
  private def placedByImdb(nodes: Seq[EvidenceNode])(scored: Scored): Scored = {
    val lists = nodes.map(node => imdbSuggested(node.id)).filter(_.nonEmpty)
    val place = lists.flatMap(list => Option(list.indexOf(scored.candidate.tmdbId)).filter(_ >= 0).map(i => Scored.ImdbPlace(i + 1, list.size)))
    place.minByOption(_.place).fold(scored)(best => scored.copy(imdb = Some(best.copy(of = lists.map(_.size).max))))
  }

  /** The country (ISO-3166-1) of `node`'s venue — what [[ReleaseVeto]] asks a namesake's releases of. */
  private[identity] def countryOf(node: EvidenceNode): Option[String] =
    models.City.forCinema(node.listings.head.cinema).map(_.country.language.getCountry).filter(_.nonEmpty)

  /** Why `node` rules `id` out before its evidence is scored: a pin, or a title naming it only by the venue's own name
   *  or by a programme tag. */
  private def denialByNode(node: EvidenceNode, id: Int): Option[String] =
    Option.when(pins.deniedFilms(node.listings.head.key)(id))("pinned never this film")
      .orElse(Option.when(namesOnlyItsVenue(node, candidateById(id)))("its title names it only by the venue's own name"))
      .orElse(Option.when(namesOnlyATag(node, candidateById(id)))("its title names it only by a programme tag billed beside many titles"))
      .orElse(Option.when(namesOnlyAProgrammeSlot(node, candidateById(id)))("its title names it only by a festival's programme slot"))

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
    val runtimes = cluster.flatMap(node => node.evidence.runtime.toSeq.flatMap(runtime => Seq.fill(node.weight)(runtime))).sorted
    val year     = modal(cluster.flatMap(node => node.evidence.year.map(_ -> node.weight)))
    val listing  = lead.evidence.measured.copy(
      year          = year,
      yearCredits   = Some(cluster.filter(node => year.nonEmpty && node.evidence.year == year).flatMap(_.evidence.directors).distinct.sorted),
      originalTitle = modal(cluster.flatMap(node => node.evidence.originalTitle.map(_ -> node.weight))),
      directors     = cluster.flatMap(_.evidence.directors).distinct.sorted,
      runtime       = runtimes.lift(runtimes.size / 2),
      countries     = cluster.flatMap(_.evidence.countries).distinct.sorted)
    val ranks = cluster.flatMap(node => ownSearch(node.id)).groupMapReduce(_._1)(_._2)(math.min)
    score(listing, lead.venue, ranks, cluster.flatMap(node => ownWalk(node.id)).toSet, cluster.flatMap(sharedOf).toSet,
      id => cluster.iterator.flatMap(denialByNode(_, id)).nextOption(), cluster.flatMap(node => imdbOnly(node.id)).toSet -- ranks.keySet,
      cluster.flatMap(node => directed(node.id)).toSet,
      cluster.flatMap(node => imdbTitled(node.id)).groupMapReduce(_._1)(_._2)(_ ++ _), countryOf(lead))
      .map(scored => if (scored.denied || cluster.forall(node => !of(node).exists(other => other.candidate.tmdbId == scored.candidate.tmdbId && other.denied))) scored else scored.copy(denial = Some("a member's own evidence rules it out")))
      .map(placedByImdb(cluster))
      .map(scored => cluster.flatMap(node => imdbTitled(node.id).getOrElse(scored.candidate.tmdbId, Set.empty)).toSet match {
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
