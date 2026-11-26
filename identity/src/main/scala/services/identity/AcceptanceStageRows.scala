package services.identity

import services.identity.Acceptance.Refused
import services.identity.AcceptanceRule.{Need, NeedOf, NoneOf, One, Weighing}
import services.identity.measures.Scored
import services.identity.measures.Scored.Accepted

/** The rows of the acceptance table that take a stage relay's film: its house's production, its season's record, its
 *  relay's record and its relay's director ([[AcceptanceRuleTable]] orders them), each check a question of `stageRules`
 *  or a step of `steps`; and what PRE-EMPTS every rule ([[seasonProduction]], [[preemptingAlone]], [[preemptingPooled]]),
 *  the season production weighed with the evidence `weights`. */
private[identity] final class AcceptanceStageRows(stageRules: AcceptanceStageRules, factRules: AcceptanceFactRules, steps: AcceptanceRuleSteps,
                                                  weights: EvidenceWeights) {
  import stageRules.{AnotherDirector, airsTheYearItStates, anotherRecordOfItsTitle, creditsItsDirector, houseRecords,
    houseRecordsOfItsYear, inItsSeason, relayDirectorsRecords, relayRecords, relaySeasons, seasonRecords}
  import factRules.contradiction
  import steps.{BillsOneWork, HasCandidate, NotJoinedToAnotherWork, NumbersItsOwnInstalment, Uncontradicted, atItsOwn, atOwn, credited}
  import weights.own

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
                 weighing => houseRecords(weighing).map(Refused.named).mkString("; ")),
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

  /** The one record of the listing's HOUSE crediting the director it credits, broadcast in the season it screens in and
   *  aired by then ([[relayDirectorsRecords]]): a relay and its credited director name the PRODUCTION, the filmed staging,
   *  not a film of the play, whatever language the venue bills the play's title in. PL Multikino's "NT Live: Mizantrop"
   *  {Indhu Rubasingham} ×15, screening 2 December 2026, is the National Theatre's "National Theatre Live: The
   *  Misanthrope" (aired 22 September 2026) — not TMDB's Polish "Mizantrop" (Szifron's "To Catch a Killer"), nor her
   *  "National Theatre Live: The Jungle Book" (April 2027, not yet aired) or "Bacchae" (another season's). Tried after
   *  every rule reading the work: a record billing its work under its house is [[HouseProduction]]'s, taken or refused.
   *  Two such records (two of the director's relays aired that season) are no answer. */
  private[identity] lazy val RelayDirector = AcceptanceRule("relay-director",
    requires = List(HasCandidate, Need("it credits no director", _.listing.directors.nonEmpty),
      Need("it bills no relay's banner", weighing => services.titles.RelayBanner.billed(weighing.listing.ownTitles)),
      Need("a record bills its work under its house", houseRecords(_).isEmpty), BillsOneWork),
    picks    = One(relayDirectorsRecords, "no record of its house by its director aired in the season it screens in",
                 "two records of its house by its director aired in the season it screens in", weighing => credited(weighing.ranked)),
    checks   = List(NotJoinedToAnotherWork),
    takes    = atItsOwn)

  // ── what pre-empts the rules ────────────────────────────────────────────────────────────────

  /** The eligible candidate whose record names the listing's SEASON PRODUCTION — the season and
   *  the work its title names. `None`: no candidate does, or the rule is switched off for an ablation run
   *  ([[RuleAblation]], `accept:season-production`). `Some(Some(x))`: `x` is the listing's
   *  film on that identity, whatever the calibrated probability (the fitted weights do not read a
   *  season yet): a season names its production as a published year names a film, and the
   *  namesakes it rules out are already denied (`ListingConstraints.seasonsApart`).
   *  `Some(None)`: two records do (two houses' stagings of one work in one season) and the
   *  listing's own facts favour neither — ambiguity, so nothing is taken, on the season or on
   *  the database's ranking. The confidence stays the calibrated probability. */
  def seasonProduction(ranked: Seq[Scored]): Option[Option[Accepted]] =
    ranked.filter(scored => !scored.denied && scored.seasonProduction).sortBy(scored => (-own(scored), scored.candidate.tmdbId)) match {
      case Seq()       => None
      case Seq(one)    => Some(Some(atOwn(one)))
      case first +: second +: _ => Some(Option.when(own(first) > own(second))(atOwn(first)))
    }

  /** The relay's record ([[RelayRecord]]), asked only whether the title numbers another instalment. */
  private def relayRecord(weighing: Weighing): Option[Accepted] = RelayRecord.accepts(weighing, List(NumbersItsOwnInstalment)).toOption
  /** The season record ([[SeasonRecord]]), asked only whether the title numbers another instalment. */
  private def seasonRecord(weighing: Weighing): Option[Accepted] = SeasonRecord.accepts(weighing, List(NumbersItsOwnInstalment)).toOption
  /** [[seasonRecord]]'s answer when it is a record of the season the listing's screenings broadcast it in: it pre-empts
   *  every rule, as [[seasonProduction]] does a season the title names — a house's other stagings of the work (the Met's
   *  2014 "Macbeth", the Royal Ballet's 2013 "Romeo and Juliet") are other broadcasts than the one the venue screens. */
  private def broadcastSeasonRecord(weighing: Weighing): Option[Accepted] = seasonRecord(weighing).filter { case (record, _) =>
    record.listing.dating.screenedSeason.exists(record.candidate.film.season.contains)
  }
  /** What pre-empts the rules a node is taken by alone, after [[seasonProduction]]: the relay's record ([[relayRecord]]) or
   *  the season record its screenings' season names ([[broadcastSeasonRecord]]), with the rule naming it — unless that
   *  rule is switched off for an ablation run. */
  private[identity] def preemptingAlone(weighing: Weighing): Option[(Accepted, String)] =
    relayRecord(weighing).map(_ -> "relay-record").orElse(broadcastSeasonRecord(weighing).map(_ -> "season-record"))
      .filterNot { case (_, rule) => ablated(rule) }
  /** What pre-empts the pooled rules: a relay's broadcast-season record pre-empts them as it does the alone ones (a
   *  cluster of "MET: Macbeth" spellings took the Met's 2014 staging by a pooled rule after the alone path ruled it another
   *  broadcast). Never stage-production pooled: one member's stated year is not the cluster's broadcast — it took UK
   *  Aldeburgh's "Royal Ballet: The Nutcracker" [2025] as the 2025/26 record, and split Savoy Worksop's "MET:Manon" off
   *  the Met's. */
  private[identity] def preemptingPooled(weighing: Weighing): Option[(Accepted, String)] =
    broadcastSeasonRecord(weighing).map(_ -> "season-record").filterNot(_ => ablated("season-record"))
  /** [[seasonProduction]], unless it is switched off for an ablation run. */
  private[identity] def seasonProductionTaken(ranked: Seq[Scored]): Option[Option[Accepted]] =
    seasonProduction(ranked).filterNot(_ => ablated("season-production"))

  private def ablated(rule: String): Boolean = RuleAblation.disables("accept:" + rule)
}
