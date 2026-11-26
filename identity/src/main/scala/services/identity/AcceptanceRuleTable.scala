package services.identity

import services.identity.Acceptance.Refused
import services.identity.AcceptanceRule.{Veto, Weighing}
import services.identity.measures.Scored.Accepted

/** EVERY ACCEPTANCE RULE, in the order they are tried: what a node takes ALONE ([[aloneRules]]), what a cluster takes
 *  on its POOLED evidence ([[pooledRules]]), and the VETOES every rule's take is asked after it ([[Vetoes]]). Before them
 *  all, a node's season production pre-empts every rule, and then its relay's record or its broadcast season's
 *  ([[AcceptanceStageRows.preemptingAlone]]), asked only whether the title numbers another instalment. Each rule is a
 *  row ([[AcceptanceRule]]) of the rows of its kind — `titleRows`, `factRows`, `stageRows` —, each veto a question of
 *  `vetoRules`; every row reads the calibration and the evidence weights off the weighing it is handed;
 *  [[Acceptance]] runs them. */
private[identity] final class AcceptanceRuleTable(titleRows: AcceptanceTitleRows, factRows: AcceptanceFactRows, stageRows: AcceptanceStageRows,
                                                  vetoRules: AcceptanceVetoRules, steps: AcceptanceRuleSteps) {
  import titleRows.{ExactTopHit, ImdbSuggested, SegmentTopHit, SoleResult}
  import factRows.{Calibrated, DatedTitle, DirectorsTitle, DirectorsWork, FavouredCalibrated, SoleWork, UnrivalledCalibrated}
  import stageRows.{HouseProduction, RelayDirector, RelayRecord, SeasonRecord}
  import vetoRules.{aNameAlone, anotherOfItsDirector, authorsNamesake, onlyTheEventsWords, performedBeside, readsAsNoFilm, reusedPage, stagedShowsOwn}
  import steps.NumbersItsOwnInstalment

  /** The rules a node may be taken by on its own evidence, first to accept deciding ([[Acceptance.aloneNamed]]). */
  private[identity] lazy val aloneRules: List[AcceptanceRule] = List(SoleWork, FavouredCalibrated, ExactTopHit, SegmentTopHit,
    SoleResult, ImdbSuggested, DirectorsWork, DirectorsTitle, DatedTitle, HouseProduction, SeasonRecord, RelayRecord, RelayDirector)

  /** The rules a cluster may be taken by on its pooled evidence, first to accept deciding ([[Acceptance.pooledNamed]]). */
  private[identity] lazy val pooledRules: List[AcceptanceRule] = List(UnrivalledCalibrated, ExactTopHit, ImdbSuggested)

  /** What refuses any rule's take, whichever rule took it, asked in this order. */
  private[identity] lazy val Vetoes: List[Veto] = List(NumbersItsOwnInstalment, NoOtherFilmOfItsDirectorBeside, Takeable,
    OnNoReusedPage, NotPerformedBeside, NotAnAuthorsNamesake, NotANameAlone, NotAStagedShowsOwn, ReadsAsAFilm)

  /** The first of `rules` to accept, with its name — a rule switched off for an ablation run ([[RuleAblation]], as
   *  `accept:<rule>`) aside. */
  private[identity] def firstOf(weighing: Weighing, rules: List[AcceptanceRule]): Option[(Accepted, String)] =
    rules.iterator.filterNot(rule => RuleAblation.disables("accept:" + rule.name)).map(rule => rule.accepts(weighing, Vetoes).map(_ -> rule.name))
      .collectFirst { case Right(taken) => taken }

  /** The listing's exact top hit ([[AcceptanceTitleRows.ExactTopHit]]), asked only whether its title numbers another
   *  instalment: what the decision's confidence reads. */
  private[identity] def topHit(weighing: Weighing): Option[Accepted] = ExactTopHit.accepts(weighing, List(NumbersItsOwnInstalment)).toOption
  /** The best eligible candidate the calibration's cut passes ([[AcceptanceFactRows.Calibrated]]). */
  private[identity] def calibrated(weighing: Weighing): AcceptanceRule.Verdict = Calibrated.verdict(weighing)

  // ── the vetoes ──────────────────────────────────────────────────────────────────────────────

  /** A film its title names nothing of beside another film of its director the title names, or the listing's year dates
   *  as closely ([[anotherOfItsDirector]]). */
  private lazy val NoOtherFilmOfItsDirectorBeside = Veto("its title names nothing of it, and another film of its director it names or dates as closely stands beside it",
    (weighing, scored) => anotherOfItsDirector(scored, weighing.eligible).map(Refused.named))

  /** An UNTAKEN film ([[Scored.untaken]]): it stays a rival, never a take. */
  private lazy val Takeable = Veto("untaken", (_, scored) => scored.untaken)

  private lazy val OnNoReusedPage = Veto("its page reads as reused: only a copied original title names a film of it, and the billing says more", reusedPage)

  private lazy val NotPerformedBeside = Veto("another piece of its billing is a performance the film is not", (_, scored) => performedBeside(scored))

  private lazy val NotAnAuthorsNamesake = Veto("its billing credits its work to an author who made no such film", (_, scored) => authorsNamesake(scored))

  /** A record whose longer title a billing of a NAME alone merely leads, no fact the venue states agreeing ([[aNameAlone]]):
   *  a performer billed bare is the performer's evening, not a recording of one programme, whichever rule found it. */
  private lazy val NotANameAlone = Veto("its billing is a name alone, which the record's longer title only leads, and no fact agrees",
    (_, scored) => Option.when(aNameAlone(scored))(Refused.named(scored)))

  private lazy val NotAStagedShowsOwn = Veto("the record is the staged show's own: a pantomime is an event, no film",
    (_, scored) => Option.when(stagedShowsOwn(scored))(Refused.named(scored)))

  private lazy val ReadsAsAFilm = Veto("the billing reads as no film", (_, scored) => Option.when(readsAsNoFilm(scored))(onlyTheEventsWords(scored)))
}
