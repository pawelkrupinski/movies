package services.identity

import services.identity.IdentityMeasures.ListingFilm
import services.identity.vocabulary.Denial

/** One [[FamilyScope.score]] of `listing` against its family's pool: the pool as the listing's titles relate to it, the candidates it reaches
 *  measured, the release veto, then the denials one candidate's evidence makes of another. */
private[identity] final class ListingScoring(pool: FamilyPool, scoring: CandidateScoring, acceptance: Acceptance,
                                             listing: MeasuredListing, venue: String, source: String, ranks: Map[Int, Int],
                                             walked: Set[Int], shared: Set[Int], deniedByNode: Int => Option[Denial], suggested: Set[Int],
                                             directedBy: Set[Int], imdbTitles: Map[Int, Set[String]], country: Option[String],
                                             linked: Set[Int], names: String => Boolean) {
  import scoring.{backing, calibration, evidenceDenial, houses, namesItsSeasonProduction}

  private val related    = pool.candidates.map(candidate => candidate.tmdbId -> pool.titleRelation(listing, candidate)).toMap
  // The ONE film IMDb lists under the listing's title (an AKA TMDB does not carry), when no film beside it carries the
  // title already: it carries it too, so the listing's facts are read against a film its title names. PL "Camino dla
  // opornych" [97′] read IMDb's Polish title of "Compostelle" as no relation, and its runtime alone denied the film.
  // A namesake of the listing's own title, not of the original it publishes beside it: Helios bills "Camino dla
  // opornych" with the original "Santiago", which Gordon Douglas's 1956 film is titled — the original is a fact its
  // own measure weighs, and IMDb's title names the film the listing's title is.
  private lazy val untranslated = listing.copy(originalTitle = None)
  private val titledBy   = IdentityMeasures.soleImdbTitled(imdbTitles).filter { case (id, _) =>
    // a film of the pool's, its IMDb title its to take, that no other film of the pool carries as its own
    pool.candidates.find(_.tmdbId == id).exists(candidate => IdentityMeasures.takesImdbTitle(listing, candidate.film)) &&
      !pool.candidates.exists(other => other.tmdbId != id &&
        IdentityMeasures.TitlesItsOwn(pool.relate(untranslated, other.film).value)) }
  private val scoredPool = titledBy.fold(pool.candidates) { case (id, titles) => pool.candidates.map(candidate =>
    if (candidate.tmdbId == id) candidate.copy(film = IdentityMeasures.withVenueTitles(candidate.film, titles.toSeq.sorted)) else candidate) }
  private val relationOf = titledBy.fold(related) { case (id, _) =>
    related.updated(id, pool.relate(listing, scoredPool.find(_.tmdbId == id).get.film)) }
  private val relation   = relationOf.view.mapValues(_.value).toMap
  private def ownReach(id: Int) = ranks.contains(id) || walked(id) || shared(id)
  private val reachable = scoredPool.filter(candidate => ownReach(candidate.tmdbId) ||
    IdentityMeasures.names(relation(candidate.tmdbId), listing, candidate.film))
  // A film only IMDb suggested, under a title the listing does not carry, rivals nothing: the rules other than
  // IMDb's own never see it, as before IMDb's other-language matches were followed.
  private def onlySuggested(id: Int) = suggested(id) && !IdentityMeasures.Rivalling(relation(id))
  private def rivalling(id: Int) = IdentityMeasures.Rivalling(relation(id)) && !onlySuggested(id)
  private val close     = reachable.count(candidate => rivalling(candidate.tmdbId))
  private val groups    = IdentityMeasures.titleGroups(listing)
  private val textNamed = IdentityMeasures.textNamedDirectors(listing, names, reachable.map(_.film))
  private val measured  = reachable.map(measure)

  private def measure(candidate: Candidate): Scored = {
    val rivals   = close - (if (rivalling(candidate.tmdbId)) 1 else 0)
    val measures = IdentityMeasures.creditedByText(candidate.film, textNamed, IdentityMeasures.creditedByTitle(listing, candidate.film,
      IdentityMeasures.creditedBySearch(IdentityMeasures.listingFilmTitled(listing, candidate.film, ranks.get(candidate.tmdbId), rivals,
        backing.corroborating((groups ++ IdentityMeasures.searchGroups(listing, candidate.film)).distinct, candidate.film, venue, source),
        relationOf(candidate.tmdbId), country), directedBy(candidate.tmdbId))))
    val probability = calibration.probability(ListingFilm, measures)
    val byNode = RuleAblation.live(deniedByNode(candidate.tmdbId))
    Scored(candidate, probability, measures, byNode.orElse(RuleAblation.live(evidenceDenial(listing, candidate.film, measures, country))), listing, ranks.get(candidate.tmdbId),
      namesItsSeasonProduction(listing, candidate.film), byNode.isDefined, StageRelays.billsUnderItsHouse(listing, candidate.film, houses),
      suggestedOnly = onlySuggested(candidate.tmdbId), ownReach = ownReach(candidate.tmdbId))
  }

  // A namesake the venue's country never saw released, beside one it did, that nothing backs is denied, and no
  // rival in the evidence class of the namesakes left ([[ReleaseVeto]]). A namesake here is a film the title names
  // EXACTLY: an original or alternative title is the venue's spelling of another language's film, not its own.
  private def namesake(scored: Scored) = relation(scored.candidate.tmdbId) == "exact" && !onlySuggested(scored.candidate.tmdbId)
  private val vetoed = country.fold(Map.empty[Int, Denial])(ReleaseVeto.of(_, measured, namesake,
    scored => scored.number("venues.corroborating").exists(_ > 0) || acceptance.weights.factsSupport(scored))) ++
    country.fold(Map.empty[Int, Denial])(ReleaseVeto.ofCurrent(_, listing.screeningsSeason, measured, namesake, ownFacts = source == venue,
      acceptance.weights.factsSupport))
  private val candidates = if (vetoed.isEmpty) measured else measured.map { scored =>
    val id = scored.candidate.tmdbId
    if (vetoed.contains(id)) scored.copy(denial = scored.denial.orElse(RuleAblation.live(vetoed.get(id))))
    else if (rivalling(id)) scored.copy(vetoedRivals = vetoed.size)
    else scored
  }

  // The listing's whole title (or its original or an alternative title) and its credited
  // director name ONE film together: another film of that director, which its title does not
  // name, is not the listing's — however its runtime or year fits. A director's filmography is a path to candidates, never a reason to leave
  // the one its title names (Syndicated's "Zodiac", Fincher, 139 minutes, is not Fight Club).
  private val titled = candidates.exists(scored => !scored.denied && IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures))
  // A film its credited director made that its title names as its own — reached by their filmography
  // (`directedBy`), whether or not its record is answered yet — says whose film of the title the venue bills: a
  // namesake their filmography did not reach, whose record does not credit them, is another director's film. DE "Rose":
  // Metropol Düsseldorf credits Marie-Camille Babayan, KiKuBi Aurélie Saada, each the maker of a "Rose" of their own;
  // while the three records went unanswered nothing else denied them Schleinzer's 2026 film, which they took by IMDb's
  // suggestion and joined (prod 10-19, and again with every deploy's new questions).
  // Not by a short of theirs: a feature's billing whose page copied a short namesake's credits names no short (DE Mal
  // seh'n Kino's "THE THING WITH FEATHERS", a week of late shows, credits Anja Oelofse's 24-minute film — Southern's).
  private val creditedNamesake = listing.directors.nonEmpty && candidates.exists(scored =>
    !scored.denied && directedBy(scored.candidate.tmdbId) && IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) &&
      !ShortFilms.isShort(scored.candidate.film))
  // A billing naming the director of a film its title names ([[IdentityMeasures.bannerNamesItsDirector]]: a series'
  // banner, "Hitchcock & Chabrol: Spellbound") says whose film of the title it shows: a namesake none of the directors
  // it names made is not it, however popular — the 2024 animated "Spellbound" is no Hitchcock.
  // Asked only of a candidate the title names as its own, stopping at the first: most listings bill no banner, and the
  // denial below asks again only when one does.
  private val directorBilled = candidates.exists(scored => !scored.denied && IdentityMeasures.TitlesItsOwn(relation(scored.candidate.tmdbId)) &&
    IdentityMeasures.bannerNamesItsDirector(listing, scored.candidate.film))
  // The listing's title numbers its instalment as an eligible record of its series does
  // (`numeral` `same`): another instalment of that series — one the listing numbers and it
  // does not, or numbers otherwise — is another film, however its crew or runtime fits (Kinoteka's
  // "Niesamowite przygody skarpetek 4. Do roboty! – zestaw" is part 4, not the 2025 first set
  // whose animators it credits).
  private val instalment = candidates.exists(scored => !scored.denied && scored.category("numeral").contains("same"))
  // A stage house's relay that an eligible record bills under its own house is not another house's staging of the
  // work, however its screen director's credit or running time fits that one: UK Odeon's "English National Ballet: The
  // Sleeping Beauty" {Ross MacGibbon} ×22 is ENB's 2026 record, not the Royal Ballet's 2017 one he also filmed. Only
  // beside its own house's: UK's "RBO Cinema Season" relays the Met's productions, which RBO never staged.
  // Its own house's by a banner word the other record lacks: "OPERA- SILENT NIGHT" names no house by "silent".
  private val housed = if (listing.billsAStageHouse) candidates.filterNot(_.denied) else Nil
  private def besideItsOwnHouse(other: Scored) = !other.denied && StageRelays.billsAnotherHouse(listing, other.candidate.film) &&
    housed.exists(own => own.candidate.tmdbId != other.candidate.tmdbId &&
      StageRelays.billsItsHouse(listing, own.candidate.film, other.candidate.film.titleWords))
  // One broadcast its film database holds twice is one film: the record every listing of it takes is the first in
  // `broadcastRecordOrder`, and the other is no listing's ([[StageRelays.sameBroadcast]]).
  private def anotherRecordOfItsBroadcast(scored: Scored) = candidates.exists(other => other.candidate.tmdbId != scored.candidate.tmdbId &&
    StageRelays.sameBroadcast(scored.candidate.film, other.candidate.film) &&
    Ordering[(Int, Int, Int)].lt(StageRelays.broadcastRecordOrder(other.candidate.tmdbId, other.candidate.film),
      StageRelays.broadcastRecordOrder(scored.candidate.tmdbId, scored.candidate.film)))
  // A short beside a feature the title names as its own is that feature's namesake, unless the venue bills a short: no
  // rule takes it, and it stays a rival ([[ShortFilms]]): DE "GINA - mit der Domschule Würzburg" is no 7-minute "Gina"
  // beside three feature-length ones. So is a short the billing opens on and goes on past ([[ShortFilms.billedBeyond]]): DE
  // "UNLESS WE DANCE - QUEERE FILME ZUR LÄNGSTEN NACHT" is an evening named after the 14-minute short, no feature beside it.
  // A short of the director the listing credits is its own film, unless a feature of that director carries the title
  // too (DE Filmmuseum München's "Nuisance Bear" {Osio Vanden, Weisman}: their 2026 feature, not their 2021 short).
  private val features = candidates.filter(scored => !scored.denied && !ShortFilms.isShort(scored.candidate.film) &&
    IdentityMeasures.TitlesItsOwn(relation(scored.candidate.tmdbId)))
  private val creditedFeature = features.exists(scored => IdentityMeasures.sameDirector(scored.measures))
  private def shortUntaken(scored: Scored) =
    if (scored.denied || (IdentityMeasures.sameDirector(scored.measures) && !creditedFeature)) None
    else ShortFilms.untaken(listing, scored.candidate.tmdbId, scored.candidate.film, linked,
      features.nonEmpty || ShortFilms.isShort(scored.candidate.film) && !IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) &&
        ShortFilms.billedBeyond(listing, scored.candidate.film, venue))
  // A film the billing names only by the event it joins ("+ Q&A": Q & A, 1990) stays a rival no rule takes
  // ([[IdentityMeasures.eventNamed]]).
  private def eventUntaken(scored: Scored): Option[String] =
    Option.when(!scored.denied && IdentityMeasures.eventNamed(listing, scored.candidate.film))(Scored.EventNamed)
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
  private def untitled(scored: Scored) = !scored.denied && relation(scored.candidate.tmdbId) == "none" &&
    country.exists(venue => scored.candidate.film.countries.exists(_.exists(IdentityMeasures.sharesLanguage(venue, _)))) &&
    IdentityMeasures.sharesNoWord(listing, scored.candidate.film)
  private lazy val named = candidates.filterNot(scored => scored.denied || untitled(scored)).map(_.probability).maxOption
  private def ownLanguageUntitled(scored: Scored) = untitled(scored) && named.forall(scored.probability >= _)

  /** The denials one candidate's evidence makes of another, in the order they are asked: each a test of the scored
   *  candidate and why it is denied. */
  private val besideDenials: Seq[(Scored => Boolean, String)] = Seq(
    (ownLanguageUntitled, "a film of the venue's own language its title shares no word with"),
    (anotherRecordOfItsBroadcast, "another record of a broadcast its film database holds twice"),
    (scored => titled && !IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures),
      "its director's other film, which the title does not name"),
    (scored => creditedNamesake && !scored.denied && !directedBy(scored.candidate.tmdbId) &&
       IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) && !IdentityMeasures.sameDirector(scored.measures),
      "a namesake of the film its credited director made of its title"),
    (scored => directorBilled && IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) &&
       !IdentityMeasures.bannerNamesItsDirector(listing, scored.candidate.film),
      "a namesake of the film whose director its billing names"),
    (scored => instalment && scored.category("numeral").exists(IdentityMeasures.OtherInstalment),
      "another instalment than the one the title numbers"),
    (scored => housed.nonEmpty && besideItsOwnHouse(scored), "another house's staging of its work, beside its own house's record"))

  /** Why another candidate's evidence denies `scored`, if it does. */
  private def deniedBeside(scored: Scored): Option[String] =
    besideDenials.collectFirst { case (denies, why) if denies(scored) => why }

  def scored: Seq[Scored] =
    candidates.map(scored => RuleAblation.live(deniedBeside(scored).map(Denial(_))).fold(scored.copy(untaken = eventUntaken(scored).orElse(shortUntaken(scored))))(denial => scored.copy(denial = Some(denial))))
      .sortBy(scored => (-scored.probability, scored.candidate.tmdbId))
}
