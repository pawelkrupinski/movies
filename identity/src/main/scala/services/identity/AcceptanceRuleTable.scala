package services.identity

import services.identity.measures.{FactRelations, IdentityMeasures, MultiFilmBill}
import services.identity.AcceptanceRule.{First, Need, NeedOf, NoneOf, One, PickedBy, Veto}

/** EVERY ACCEPTANCE RULE, one row each, in the order they are tried: what a node takes ALONE ([[aloneRules]]), what a
 *  cluster takes on its POOLED evidence ([[pooledRules]]), and the VETOES every rule's take is asked after it
 *  ([[Vetoes]]). Before them all, a node's season production pre-empts every rule (`Acceptance.seasonProduction`), and
 *  then its relay's record or its broadcast season's ([[RelayRecord]], [[SeasonRecord]]), asked only whether the title
 *  numbers another instalment. A row reads top to bottom as the rule does: what it requires of the ranking, how it
 *  picks its one candidate, each check that candidate must pass with the refusal the trace names it by, and what it
 *  takes the candidate at ([[AcceptanceRule]]); what a check asks is a named question of the rule files beside this
 *  one. Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceRuleTable { this: Acceptance =>

  /** The rules a node may be taken by on its own evidence, first to accept deciding ([[Acceptance.aloneNamed]]). */
  private[identity] lazy val aloneRules: List[AcceptanceRule] = List(SoleWork, FavouredCalibrated, ExactTopHit, SegmentTopHit,
    SoleResult, ImdbSuggested, DirectorsWork, DirectorsTitle, DatedTitle, HouseProduction, SeasonRecord, RelayRecord)

  /** The rules a cluster may be taken by on its pooled evidence, first to accept deciding ([[Acceptance.pooledNamed]]). */
  private[identity] lazy val pooledRules: List[AcceptanceRule] = List(UnrivalledCalibrated, ExactTopHit, ImdbSuggested)

  /** What refuses any rule's take, whichever rule took it, asked in this order. */
  private[identity] lazy val Vetoes: List[Veto] = List(NumbersItsOwnInstalment, NoOtherFilmOfItsDirectorBeside, Takeable,
    OnNoReusedPage, NotPerformedBeside, NotAnAuthorsNamesake, NotANameAlone, NotAStagedShowsOwn, ReadsAsAFilm)

  // ── steps several rules share ───────────────────────────────────────────────────────────────

  private lazy val HasCandidate = Need("no candidate", _.ranked.nonEmpty)
  /** A listing billing two works ([[MultiFilmBill.billsTwoWorks]]): a double bill is neither film. */
  private lazy val BillsOneWork = Need("a double bill", weighing => !MultiFilmBill.billsTwoWorks(weighing.listing))
  /** A film the listing's title joins another work to by a slash ([[IdentityMeasures.joinsAnotherWork]]): a bill of two
   *  works is neither, as a "+" one is. */
  private lazy val NotJoinedToAnotherWork = NeedOf("the title joins another work to it", (weighing, scored) => !joinedToAnotherWork(weighing, scored))
  private lazy val Uncontradicted = NeedOf("a published fact contradicts it", (_, scored) => !contradicted(scored), (_, scored) => contradiction(scored))
  /** A film the title numbers as ANOTHER instalment than its own ([[IdentityMeasures.OtherInstalment]]). */
  private lazy val NotAnotherInstalment = NeedOf("the title numbers another instalment", (_, scored) => !otherInstalment(scored), (_, scored) => numeral(scored))
  /** The best eligible candidate's probability, the priors lending ([[AcceptanceRule.Weighing.lentToTheBest]]), clears the cut. */
  private lazy val ClearsTheCut = NeedOf("below the rating cut", (weighing, _) => calibration.showsRatings(weighing.lentToTheBest),
    (weighing, _) => cut(weighing.lentToTheBest))
  private lazy val BestEligible = First(_.eligible, "no eligible candidate", bestDenied)

  // ── the rules ───────────────────────────────────────────────────────────────────────────────

  /** The film whose WORK the listing's whole title is, when TMDB ranks it first, it is the only such candidate, and no
   *  other candidate is one the title names — the rest only a director's filmography reached. US venues' "BTS WORLD TOUR
   *  'ARIRANG' IN BUENOS AIRES" (×1,915 with São Paulo) is "…: Live Viewing", not the 2022 Seoul concert film its
   *  director also made; neither pipeline matched them. A work of one word is too many films' title ("It" of "It:
   *  Chapter Two"). The listing may bill that work under a shorter subtitle of its own, the whole title running along
   *  the record's (DE ×140 and ES ×82 "…In Buenos Aires: Live"), when no other candidate bills the work: a banner leading
   *  the title is no work ("The Metropolitan Opera: La Fanciulla del West Encore" is not its 2018 staging). */
  private[identity] lazy val SoleWork = AcceptanceRule("sole-work",
    requires = Nil,
    picks    = One(firstHitsTitlingTheirWork, "no first hit is the title's work", "two first hits are the title's work", weighing => firstHit(weighing.ranked)),
    checks   = List(NoneOf("the title names another film", anotherNamedBesideItsWork)),
    takes    = atItsOwn)

  /** The best eligible candidate, when its probability — the priors lending, never withdrawing
   *  ([[EvidenceWeights.priorsLent]]) — clears the calibration's cut: what the decision's confidence reads
   *  ([[Acceptance.confidenceOf]]), and what the two calibrated rules weigh further. */
  private[identity] lazy val Calibrated = AcceptanceRule("calibrated",
    requires = Nil,
    picks    = BestEligible,
    checks   = List(ClearsTheCut),
    takes    = atWhatThePriorsLend)

  /** [[Calibrated]], when the listing's own facts also favour it over the runner-up — another film the billing could name,
   *  never one it names only by the event it joins (UK "The Assassin + Q&A" beside Q & A, 1990:
   *  [[IdentityMeasures.eventNamed]]), nor one only a sibling's search brought into the family's pool ([[Scored.ownReach]]):
   *  that runner-up comes and goes with the sibling, and the take with it (DE Cineplex Neustadt's bare "Es" [72'], the 2017
   *  film its own search ranks first, refused whenever Caligari's "ES (ULRICH SCHAMONI)" pulled in the 1966 "Es" its title
   *  names too — a flip back and forth as the family's venues arrived, structural fix 2). The closer record likewise: one
   *  the listing's own evidence reached. */
  private[identity] lazy val FavouredCalibrated = Calibrated.copy(name = "favoured-calibrated", checks = Calibrated.checks ++ List(
    NoneOf("its own facts do not favour it over the runner-up", runnerUpItsFactsDoNotBeat),
    NoneOf("its title sits inside a closer record", (weighing, best) => closerThan(best, ownReached(weighing, best)))))

  /** [[Calibrated]] — unless the title names another candidate by the very same pieces and the pooled FACTS
   *  ([[EvidenceWeights.facts]]) fit that one better: four "Camino dla opornych" whose original title "Santiago" names two
   *  films, and whose 113 minutes fit the fourth the search returned, do not take the 93-minute first. A candidate the
   *  title names less specifically ("Mad Max" inside "Mad Max 2: The Road Warrior") or not at all is no such rival, and
   *  namesakes the facts fit alike stay the calibration's to tell apart — its ranking priors and the family's venue count
   *  are measured evidence there (a bare "Resident Evil" at 148 venues). Two films the title names by disjoint pieces
   *  ([[IdentityMeasures.namedApart]]) are not namesakes: only the facts may pick one of "Lalka (Dolly)"'s two. */
  private[identity] lazy val UnrivalledCalibrated = Calibrated.copy(name = "unrivalled-calibrated", checks = Calibrated.checks ++ List(
    NoneOf("its title sits inside a closer record", (weighing, best) => closerThan(best, weighing.eligible)),
    NoneOf("a film the title names alike fits its facts better", namedAlikeAndFittingBetter),
    NoneOf("a festival's bare billing names a newer film alike", newerNamedAlikeByAFestival),
    NoneOf("the title names it only within another candidate's longer title",
      (weighing, best) => Option.when(namedWithinAnother(best, weighing.eligible))(best))))

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

  /** The one candidate whose WORK the listing bills under another subtitle, or publishes alone, by the director it
   *  credits, that no published year or runtime contradicts ([[directorsWorks]]): Multikino's "Cirque du Soleil: Kurios -
   *  Gabinet osobliwości" by Michel Laprise is his "…: KURIOS - Cabinet des curiosités". The pipeline took such hits by
   *  their director on 3,247 listings with no wrong match; two candidates fitting alike (a director's sequels of one
   *  work) are no answer. A one-word work also needs the published year, a word being many films' title. */
  private[identity] lazy val DirectorsWork = AcceptanceRule("directors-work",
    requires = Nil,
    picks    = One(weighing => withoutHollow(directorsWorks(weighing)), "no film of its credited director shares its work",
                 "two films of its director share its work", weighing => credited(weighing.ranked)),
    checks   = Nil,
    takes    = atItsOwn)

  /** The one eligible candidate the listing's title — or the original title its venue publishes — names EXACTLY that
   *  credits the director the listing credits, when no published year or runtime contradicts it ([[directorsTitles]]): a
   *  title and its director name one film together, however the database's search ranks it or however few venues list
   *  it (US "Man of Iron" {Andrzej Wajda}, seventh in TMDB's search behind "Iron Man"). Two such records are no answer. */
  private[identity] lazy val DirectorsTitle = AcceptanceRule("directors-title",
    requires = Nil,
    picks    = One(weighing => withoutHollow(directorsTitles(weighing)), "no film of its exact title by its credited director",
                 "two films of its title by its director", weighing => credited(weighing.ranked)),
    checks   = Nil,
    takes    = atItsOwn)

  /** The one eligible candidate the listing's title names EXACTLY from the year its title dates it (a year off at most,
   *  as a release and a premiere differ), when nothing it publishes contradicts it — a credited director included: US
   *  "Troll (1986)" is the 1986 film, though TMDB ranks "Troll 2" and a 2022 "Troll" above it ([[namedButForItsYear]]:
   *  decorated, as "… Encore (2027)", too). Two such records are no answer. */
  private[identity] lazy val DatedTitle = AcceptanceRule("dated-title",
    requires = Nil,
    picks    = One(datedTitles, "no film of its title from the year its title dates", "two films of its title from that year", undated),
    checks   = Nil,
    takes    = atItsOwn)

  /** The one eligible record billing the listing's work under the listing's OWN house ([[Scored.houseProduction]]) that no
   *  published year or runtime contradicts: Flicks' "NT Live: The Misanthrope" is the National Theatre's 2026 broadcast,
   *  "National Theatre Live: The Misanthrope", though TMDB's search ranks three bare records of Molière's play above it —
   *  a house's banner names its production as a season names one ([[seasonProduction]]) — nor one from another year than
   *  the title dates the production (US "MetOpera: Carmen (2009)" ×513 is Eyre's 2009 staging, not the Met's 2024 record
   *  its house bills). Two such records (a broadcast and its encore) are no answer — nor are two records carrying one
   *  title, however each reads as billed (US "NT Live: Hamlet": the National Theatre's 2010 and 2015 broadcasts) — and
   *  neither is one that another candidate the title names fits at least as well on the listing's own facts: a banner is
   *  learned, not known, and Everyman's "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day", publishing
   *  nothing but its title, is Cameron's film, not TMDB's making-of documentary. */
  private[identity] lazy val HouseProduction = AcceptanceRule("house-production",
    requires = List(Need("no record bills its work under its house", houseRecords(_).nonEmpty)),
    picks    = One(houseRecordsOfItsYear, "its house's record is from another year than its title dates", "two records bill its work under its house",
                 weighing => houseRecords(weighing).map(Acceptance.named).mkString("; ")),
    checks   = List(NoneOf("another record carries its title or fits as well", anotherRecordOfItsTitle)),
    takes    = atItsOwn)

  /** The one SEASON record whose title is the listing's own once its season is taken out ("&" and "and" alike), when the
   *  listing names no season ([[seasonRecords]]). A fact contradicting it, a year it states outside the season's two
   *  ([[inItsSeason]]), or another director refuses it. Its answer for the season the listing's screenings broadcast it
   *  in pre-empts every rule ([[broadcastSeasonRecord]]). */
  private[identity] lazy val SeasonRecord = AcceptanceRule("season-record",
    requires = List(HasCandidate, Need("the title names its season", _.listing.seasonYear.isEmpty)),
    picks    = One(seasonRecords, "no season record carries its title or its work under its house", "two seasons' records carry its title"),
    checks   = List(
      Uncontradicted,
      NeedOf("the year it states is another season's", (weighing, record) => inItsSeason(weighing.listing, record)),
      NeedOf("it credits another director", (_, record) => !record.category("director").exists(AnotherDirector))),
    takes    = atItsOwn)

  /** The one eligible record of the listing's RELAY ([[RelayStage]], [[relayRecords]]): its house's record of its work from
   *  the season its title dates, whatever running time it files, that no year it publishes contradicts ([[airsTheYearItStates]])
   *  and no director it credits denies ([[creditsItsDirector]]). US "MetOpera: Carmen (2009)" [240′] ×514 is the Met's
   *  2009/10 broadcast, TMDB's "Bizet: Carmen" (aired 15 January 2010, 166′), not the Met's 2019 or 2024 Carmen. It
   *  pre-empts every rule, as [[seasonProduction]] does. */
  private[identity] lazy val RelayRecord = AcceptanceRule("relay-record",
    requires = List(HasCandidate, Need("it bills no house's relay of a work in a season", _.listing.relayBilled.isDefined)),
    picks    = One(relayRecords, "no record of its house's relay of its work from its season", "two records of its house's relay of its work from its season",
                 relaySeasons),
    checks   = List(
      NeedOf("a published year contradicts it", (weighing, record) => airsTheYearItStates(weighing.listing, record), (_, record) => contradiction(record)),
      NeedOf("it credits another director", creditsItsDirector)),
    takes    = atItsOwn)

  // ── the vetoes ──────────────────────────────────────────────────────────────────────────────

  /** A film both titles number, differently: PL Cinema City's and UK Odeon's "League of Legends Worlds 26 | Finals in
   *  Cinema" is not TMDB's "League of Legends Worlds25 - Finals in Cinema", its only search hit. A number only one side
   *  carries is weaker — TMDB files a two-part film's halves by their subtitles ("De Gaulle: Liberté" is UK "De Gaulle:
   *  Liberté Part 2") — and stays the rules' own to weigh ([[ImdbSuggested]], [[SoleResult]]). */
  private[identity] lazy val NumbersItsOwnInstalment = Veto("the title numbers another instalment",
    (_, scored) => Option.when(scored.category("numeral").contains("different"))(numeral(scored)))
  /** A film its title names nothing of beside another film of its director the title names, or the listing's year dates
   *  as closely ([[anotherOfItsDirector]]). */
  private lazy val NoOtherFilmOfItsDirectorBeside = Veto("its title names nothing of it, and another film of its director it names or dates as closely stands beside it",
    (weighing, scored) => anotherOfItsDirector(scored, weighing.eligible).map(Acceptance.named))
  /** An UNTAKEN film ([[Scored.untaken]]): it stays a rival, never a take. */
  private lazy val Takeable = Veto("untaken", (_, scored) => scored.untaken)
  private lazy val OnNoReusedPage = Veto("its page reads as reused: only a copied original title names a film of it, and the billing says more", reusedPage)
  private lazy val NotPerformedBeside = Veto("another piece of its billing is a performance the film is not", performedBeside)
  private lazy val NotAnAuthorsNamesake = Veto("its billing credits its work to an author who made no such film", authorsNamesake)
  /** A record whose longer title a billing of a NAME alone merely leads, no fact the venue states agreeing ([[aNameAlone]]):
   *  a performer billed bare is the performer's evening, not a recording of one programme, whichever rule found it. */
  private lazy val NotANameAlone = Veto("its billing is a name alone, which the record's longer title only leads, and no fact agrees",
    (_, scored) => Option.when(aNameAlone(scored))(Acceptance.named(scored)))
  private lazy val NotAStagedShowsOwn = Veto("the record is the staged show's own: a pantomime is an event, no film",
    (_, scored) => Option.when(stagedShowsOwn(scored))(Acceptance.named(scored)))
  private lazy val ReadsAsAFilm = Veto("the billing reads as no film", (_, scored) => Option.when(readsAsNoFilm(scored))(onlyTheEventsWords(scored)))
}
