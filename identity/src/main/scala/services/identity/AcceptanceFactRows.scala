package services.identity

import services.identity.AcceptanceRule.{NoneOf, One}

/** The rows of the acceptance table that take a film by the listing's facts: its sole work, the calibrated rules —
 *  favoured and unrivalled —, a director's work and title, and a dated title ([[AcceptanceRuleTable]] orders them), each
 *  check a question of `factRules`, `directorRules` or `calibratedRules`, or a step of `steps`. */
private[identity] final class AcceptanceFactRows(factRules: AcceptanceFactRules, directorRules: AcceptanceDirectorRules,
                                                 calibratedRules: AcceptanceCalibratedRules, steps: AcceptanceRuleSteps) {
  import factRules.{anotherNamedBesideItsWork, datedTitles, firstHitsTitlingTheirWork, undated}
  import directorRules.{directorsTitles, directorsWorks}
  import calibratedRules.{closerThan, namedAlikeAndFittingBetter, namedWithinAnother, newerNamedAlikeByAFestival, ownReached, runnerUpItsFactsDoNotBeat}
  import steps.{BestEligible, ClearsTheCut, atItsOwn, atWhatThePriorsLend, credited, firstHit, withoutHollow}

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
}
