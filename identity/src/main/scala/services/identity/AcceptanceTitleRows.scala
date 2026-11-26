package services.identity

import services.identity.AcceptanceRule.{Need, NeedOf, NoneOf, One, PickedBy}
import services.identity.measures.{FactRelations, IdentityMeasures}

/** The rows of the acceptance table that take a film by how its title is searched: its exact and segment top hits, its
 *  searches' sole result and IMDb's suggestion ([[AcceptanceRuleTable]] orders them), each check a question of
 *  `titleRules` or a step of `steps`. */
private[identity] final class AcceptanceTitleRows(titleRules: AcceptanceTitleRules, steps: AcceptanceRuleSteps) {
  import titleRules.{alternativeAgainstTmdbsSearch, asItsBareTitle, asItsTitlesTopHit, creditsAnotherDirector, filmsTheTitleNames, imdbRung,
    itsOwnInstalmentOrASlip, itsTitlesFactsBeside, namesBeside, namesItsOnlyResult, otherInstalment, resultsFound, searchNamesAnother,
    soleResults, standsForItsTitle, theExactTopHit}
  import steps.{BillsOneWork, HasCandidate, NotJoinedToAnotherWork, Uncontradicted, atItsClass, numeral, titleRelation, titleYearDelta, unopposedAs}

  /** The listing's EXACT TOP HIT, accepted on what the calibration measured for its evidence as a CLASS
   *  (`IdentityCalibration.classProbability`): the one film its whole title names exactly that its own title search
   *  returned first, in TMDB's order ([[IdentityMeasures.exactTopHits]]), when the listing's own evidence rules it out on
   *  nothing, published nothing against it, and gives no rival a better fit ([[EvidenceWeights.fitsBetter]]). A bare title
   *  credits it with the naive-Bayes sum of missing facts, a low popularity and its same-titled rivals, which undersells
   *  what the class measured on the labels; here the search standing LENDS confidence and never withdraws it. */
  private[identity] lazy val ExactTopHit = AcceptanceRule("exact-top-hit",
    requires = List(HasCandidate),
    picks    = PickedBy(theExactTopHit),
    checks   = Nil,
    takes    = unopposedAs(identity))

  /** A programme listing's EXACT TOP HIT once its banner is off: the one candidate whose title is a whole delimited piece
   *  of the listing's (`segment`) and which the listing's own search returned FIRST, when the title names no other
   *  candidate at all (the rest is a banner, not a second film), it bills no two works, and nothing it publishes speaks
   *  against it or fits a rival better. The piece IS a bare title, so it is credited with what the exact-top-hit class
   *  measured for bare titles. PL's "DZIEŃ KINA POLSKIEGO: Przepraszam, czy tu biją" and ~30 programme listings like it
   *  sat at 28.9% with no rule to take them. */
  private[identity] lazy val SegmentTopHit = AcceptanceRule("segment-top-hit",
    requires = List(HasCandidate),
    picks    = One(filmsTheTitleNames, "the title names no film", "the title names two films"),
    checks   = List(
      NeedOf("the film it names is denied", (_, scored) => !scored.denied, (_, scored) => scored.denial.fold("")(_.why)),
      NeedOf("not its search's first hit", (_, scored) => scored.rank.contains(1), (_, scored) => scored.rank.fold("not ranked")(rank => s"ranked $rank")),
      NeedOf("not named by a piece of the title", (_, scored) => scored.category("title").contains("segment"), (_, scored) => titleRelation(scored)),
      BillsOneWork,
      NeedOf("its piece cannot stand for the whole title", (weighing, scored) => IdentityMeasures.standsForTheWhole(weighing.listing, scored.candidate.film)),
      NotJoinedToAnotherWork,
      // a year the title states is the record's: "Disney Junior Cinema Club 2026" is not the 2024 edition
      NeedOf("the title dates another year", (_, scored) => scored.number("titleYear.delta").forall(FactRelations.nearDelta), (_, scored) => titleYearDelta(scored))),
    // read as the bare title its piece is: the banner beside it is no evidence against the film
    takes    = unopposedAs(asItsBareTitle))

  /** The ONLY film one of the listing's own title searches returned — the old pipeline's `searchUnique`, which took it
   *  unless a fact CONTRADICTED it: "Loving Karma" [78′] is TMDB's 85-minute record, the one "Loving Karma" finds, though
   *  seven minutes weigh against it in the probability. The title must name it — a piece standing for the whole
   *  ([[IdentityMeasures.standsForTheWhole]]: not "Bhutan" of "Bhutan – Trails of Happiness") — no other candidate the
   *  title names may stand beside it, and a double bill is neither film. A director the venue credits is the listing's
   *  own fact: a search's only film directed by another, that the title names only in part, is no answer (PL "Carmen"
   *  [2026] {Richard Eyre} and "Carmen: Salzburger Festspiele 2026") — a film carrying the very title still is (Kino
   *  Parczew's "Tedi i magiczna lampa" credits its co-directors). A film the title names by fewer of its words is no rival
   *  to one carrying them all ("Manon" beside "BALLET LIVE. MANON. ROYAL ÓPERA HOUSE" for "Manon (BALLET LIVE)"). */
  private[identity] lazy val SoleResult = AcceptanceRule("sole-result",
    requires = List(HasCandidate),
    picks    = One(soleResults, "no search of its returned a single film", "its searches returned different single films", resultsFound),
    checks   = List(
      NeedOf("the title does not name its search's only film", (_, scored) => namesItsOnlyResult(scored), (_, scored) => titleRelation(scored)),
      Uncontradicted,
      NeedOf("it credits another director", (_, scored) => !creditsAnotherDirector(scored)),
      BillsOneWork,
      NotJoinedToAnotherWork,
      NeedOf("its piece cannot stand for the whole title", (weighing, scored) => standsForItsTitle(weighing.listing, scored)),
      NoneOf("the title names another film", (weighing, sole) => weighing.eligible.find(namesBeside(sole, _))),
      NotAnotherInstalment),
    takes    = atItsClass(itsTitlesFactsBeside))

  /** The film IMDb's suggestions for the listing's own title name ([[imdbRung]]) — the route that reaches a film whose
   *  local title TMDB lacks, since IMDb matches a query against a film's other-language titles ("Superfutrzak i złośliwa
   *  wiewiórka" suggests only the Finnish "Supermarsu ja suuri huijaus"). Read as a title the film answers to, nothing
   *  the listing publishes may speak against it, no rival may fit better, and a double bill is neither film. A fallback,
   *  as the old pipeline's was: IMDb answers only when TMDB's own search ranks no OTHER film the title names above it
   *  ([[searchNamesAnother]]), and never for an instalment the title numbers otherwise ("Recepta na szczęście 2" is not
   *  the first film) — but a billing that is the film's own title but for a slip numbers what that title numbers (a slip
   *  is never a numeral): the series' numbered alias it shares words with is no other instalment (UK "The Hunger Games
   *  Sunrise of the Reaping" beside TMDB's numbered alias of "… Sunrise on the Reaping"). */
  private[identity] lazy val ImdbSuggested = AcceptanceRule("imdb-suggested",
    requires = List(HasCandidate, Need("IMDb suggests none of its candidates", _.suggested.nonEmpty), BillsOneWork),
    picks    = One(weighing => weighing.suggested.filter(imdbRung(weighing, _)), "no IMDb suggestion is taken by its year, its director or as the only one",
                 "two IMDb suggestions qualify"),
    checks   = List(
      NotJoinedToAnotherWork,
      NeedOf("a bare billing names the film only by an alternative title, and TMDB's search of it finds other films, not this one",
        (weighing, scored) => !alternativeAgainstTmdbsSearch(scored, weighing.ranked), (_, scored) => titleRelation(scored)),
      NoneOf("TMDB ranks another film the title names above it", (weighing, scored) => weighing.undenied.find(searchNamesAnother(scored, _))),
      NeedOf("the title numbers another instalment", (_, scored) => itsOwnInstalmentOrASlip(scored), (_, scored) => numeral(scored))),
    takes    = unopposedAs(asItsTitlesTopHit, among = _.undenied))

  /** A film the title numbers as ANOTHER instalment than its own ([[IdentityMeasures.OtherInstalment]]). */
  private lazy val NotAnotherInstalment = NeedOf("the title numbers another instalment", (_, scored) => !otherInstalment(scored), (_, scored) => numeral(scored))
}
