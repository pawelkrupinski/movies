package services.identity

import services.identity.AcceptanceRule.Weighing
import services.identity.Scored.Accepted
import services.titles.YearWindow

/** What the acceptance rules of a stage relay ask: the house's production, the relay's record ([[RelayStage]]), and the
 *  record of the season it is broadcast in ([[AcceptanceRuleTable]]).
 *  Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceStageRules { this: Acceptance =>
  import weights.own

  /** The eligible records billing the listing's work under the listing's OWN house ([[Scored.houseProduction]]) that no
   *  published year or runtime contradicts. */
  private[identity] def houseRecords(weighing: Weighing): Seq[Scored] =
    weighing.eligible.filter(candidate => candidate.houseProduction && !contradicted(candidate))
  /** [[houseRecords]] from the year the title dates the production, if it dates one: [[AcceptanceRuleTable.HouseProduction]]'s
   *  to pick one of. */
  private[identity] def houseRecordsOfItsYear(weighing: Weighing): Seq[Scored] =
    houseRecords(weighing).filter(_.number("titleYear.delta").forall(FactRelations.nearDelta))
  /** Another record carrying the house record's title, or one the title names that fits at least as well on the
   *  listing's own facts. */
  private[identity] def anotherRecordOfItsTitle(weighing: Weighing, house: Scored): Option[Scored] =
    weighing.eligible.find(other => (other ne house) &&
      (IdentityMeasures.key(other.candidate.film.title) == IdentityMeasures.key(house.candidate.film.title) ||
        (other.titleNamesIt && own(other) >= own(house))))

  /** The eligible records of the listing's RELAY ([[RelayStage]]): its house's records of its work from the season its title
   *  dates — DE "Royal Ballet & Opera im Kino: Manon" [2026] {Kenneth MacMillan} ×59 is "Royal Ballet & Opera 2026/27:
   *  Manon" (October 2026), not the Met's 2026/27 Manon (April 2027), which "MET Opera Live im Kino: Manon" [2027] ×4 is;
   *  DE "RBO live 2027: Tosca" is the RBO's 2026/27 record, not the ROH's 2011 one IMDb suggests — [[AcceptanceRuleTable.RelayRecord]]'s
   *  to pick one of. A record of its screenings' season airing on a day it screens is the broadcast it screens, whatever
   *  older year its title dates the staging by: UK Ilkley's "The Royal Ballet: The Nutcracker (2025)", screening 1
   *  December 2026, is the 2026/27 relay aired 29 November, which 112 Flicks venues bill "RBO Cinema Season 2026-27: The
   *  Nutcracker (2025)". Of a year's two seasons each holding a record of it, the one dated that very year (UK
   *  Gorleston's "RBO: THE NUTCRACKER 2026" is the 2026/27 record, not the 2025/26 one). */
  private[identity] def relayRecords(weighing: Weighing): Seq[Scored] = {
    val relay   = relayOf(weighing)
    val records = weighing.eligible.filter(scored => RelayStage.recordOf(relay, scored.candidate.film))
    val aired   = records.filter(scored => relay.airedOn(scored.candidate.film))
    if (aired.nonEmpty) aired
    else Some(records.filter(scored => relay.year.exists(scored.candidate.film.year.contains))).filter(_.sizeIs == 1).getOrElse(records)
  }
  /** The relay the listing bills: asked only by the rule requiring one. */
  private def relayOf(weighing: Weighing): RelayStage.Billed = weighing.listing.relayBilled.get
  private[identity] def relaySeasons(weighing: Weighing): String = {
    val relay = relayOf(weighing)
    s"works ${relay.works.toSeq.sorted.mkString(",")}, seasons ${relay.seasons.toSeq.sorted.mkString(",")}"
  }
  /** No year the listing publishes contradicts the relay's record — but a page kept from a season too far back dates
   *  its staging, not the relay it airs on its day ([[MeasuredListing.airDays]]). */
  private[identity] def airsTheYearItStates(listing: MeasuredListing, record: Scored): Boolean =
    !record.number("year.distance").exists(_ > YearWindow.PublishedAdjacency) ||
      listing.billsAStaleSeason && listing.relayBilled.exists(_.airedOn(record.candidate.film))
  /** The relay's record credits no other director than the listing — or the listing's credit is another work's
   *  ([[creditsAnotherWorksRecord]]). */
  private[identity] def creditsItsDirector(weighing: Weighing, record: Scored): Boolean =
    !record.category("director").exists(AnotherDirector) || creditsAnotherWorksRecord(weighing.ranked, relayOf(weighing))
  /** A director relation that says the record is another director's, in either script. */
  private[identity] val AnotherDirector = Set("different", "different_script")

  /** Does the relay credit the director of its house's record of ANOTHER work — a credit the venue copied from another
   *  relay's page, which says nothing of which production of its own work it screens? US Showcase's "The Metropolitan
   *  Opera: Samson et Dalila ENCORE (2026)" ×7 credits Louisa Proske, who directs the Met's 2026/27 "Macbeth" in its
   *  pool, not its Samson (Darko Tresnjak). A credit naming a director of none of its house's records, or of a record of
   *  its own work, still denies. */
  private def creditsAnotherWorksRecord(ranked: Seq[Scored], relay: RelayStage.Billed): Boolean = ranked.exists { scored =>
    val film = scored.candidate.film
    scored.category("director").contains("same_person") && film.stageWorkIds.nonEmpty && !film.stageWorkIds.exists(relay.works) && relay.itsHouse(film)
  }

  /** The relay's record ([[AcceptanceRuleTable.RelayRecord]]), asked only whether the title numbers another instalment. */
  private[identity] def relayRecord(weighing: Weighing): Option[Accepted] = RelayRecord.accepts(weighing, List(NumbersItsOwnInstalment)).toOption
  /** The season record ([[AcceptanceRuleTable.SeasonRecord]]), asked only whether the title numbers another instalment. */
  private[identity] def seasonRecord(weighing: Weighing): Option[Accepted] = SeasonRecord.accepts(weighing, List(NumbersItsOwnInstalment)).toOption
  /** [[seasonRecord]]'s answer when it is a record of the season the listing's screenings broadcast it in: it pre-empts
   *  every rule, as [[seasonProduction]] does a season the title names — a house's other stagings of the work (the Met's
   *  2014 "Macbeth", the Royal Ballet's 2013 "Romeo and Juliet") are other broadcasts than the one the venue screens. */
  private[identity] def broadcastSeasonRecord(weighing: Weighing): Option[Accepted] = seasonRecord(weighing).filter { case (record, _) =>
    record.listing.dating.screenedSeason.exists(record.candidate.film.season.contains)
  }
  /** The relay's record ([[relayRecord]]) or the season record its screenings' season names ([[broadcastSeasonRecord]]),
   *  with the rule naming it: what pre-empts the rules after [[seasonProduction]]. */
  private[identity] def relayOrBroadcastSeasonRecord(weighing: Weighing): Option[(Accepted, String)] =
    relayRecord(weighing).map(_ -> "relay-record").orElse(broadcastSeasonRecord(weighing).map(_ -> "season-record"))

  /** The SEASON records whose title is the listing's own once its season is taken out ("&" and "and" alike): UK "Royal
   *  Ballet and Opera: Romeo and Juliet" {Kenneth MacMillan} ×127, screening May–June 2027, is "Royal Ballet & Opera
   *  2026/27: Romeo and Juliet" — the cut alone denied it, the season marker reading as a title the listing does not
   *  carry. Two seasons of the title ("…: Tosca", 2025/26 and 2026/27) are told apart by the season the listing's
   *  screenings broadcast it in ([[ofItsBroadcastSeason]]), and are no answer without one. So is the record of the very
   *  season the listing's screenings broadcast it in that names its stage work under its house
   *  ([[StageRelays.bannerNamesItsHouse]]), however the venue shortens the banner: prod UK 2026-10-13 "MET: Macbeth" (17
   *  October 2026), "RBO: Tosca 27" and "Macbeth (Met Opera)" are the Met's and RBO's 2026/27 records. */
  private[identity] def seasonRecords(weighing: Weighing): Seq[Scored] = {
    def plain(title: String) = IdentityMeasures.key(title.replace("&", " and "))
    val listing = weighing.listing
    val own     = (Seq(listing.title) ++ listing.rawTitle).map(plain).toSet
    ofItsBroadcastSeason(weighing.ranked.filter(scored => (isEligible(scored) || scored.deniedByCutOnly) && scored.candidate.film.season.isDefined &&
      (scored.candidate.film.titles.exists(t => own(plain(IdentityMeasures.withoutSeasons(t)))) || StageRelays.screenedSeasonRecord(listing, scored.candidate.film))))
  }
  /** A season spans two years: a year the listing states outside them is another season's broadcast (UK Aldeburgh's "The
   *  Royal Ballet: Romeo & Juliet" [2025] is no "The Royal Ballet: Romeo and Juliet (2021/22)") — but for the record of the
   *  season its screenings broadcast it in, whose year it predates: that year dates the staging's premiere (Aldeburgh's,
   *  screening on 25 May 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet", aired that day). */
  private[identity] def inItsSeason(listing: MeasuredListing, record: Scored): Boolean =
    listing.year.forall(year => record.candidate.film.season.forall(season => year == season || year == season + 1)) ||
      StageRelays.screenedSeasonRecord(listing, record.candidate.film)

  /** Of several season records of one stage work, the one season the listing's screenings broadcast it in
   *  ([[MeasuredListing.screenedSeason]]) when exactly that season's records are among them; all of them
   *  otherwise. A relay airs on its house's published dates: UK "Royal Ballet & Opera: Swan Lake" (recording
   *  38342304766, ~30 venues billing no season or year), screening on 22 April 2027, is the 2026/27 record, not the
   *  house's 2024/25 one. */
  private[identity] def ofItsBroadcastSeason(records: Seq[Scored]): Seq[Scored] =
    records.headOption.flatMap(_.listing.dating.screenedSeason).filter(_ => records.sizeIs > 1)
      .map(season => records.filter(record => record.candidate.film.season.contains(season)))
      .filter(_.nonEmpty).getOrElse(records)
}
