package services.identity

import services.identity.Scored.Accepted
import services.resolution.YearWindow

/** The acceptance rules of a stage relay: the house's production, the relay's record ([[RelayStage]]), and the record of
 *  the season it is broadcast in.
 *  Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceStageRules { this: Acceptance =>
  import weights.own
  import Acceptance.Refused

  /** The one eligible record billing the listing's work under the listing's OWN house
   *  ([[Scored.houseProduction]]) that no published year or runtime contradicts: Flicks' "NT Live:
   *  The Misanthrope" is the National Theatre's 2026 broadcast, "National Theatre Live: The
   *  Misanthrope", though TMDB's search ranks three bare records of Molière's play above it — a
   *  house's banner names its production as a season names one ([[seasonProduction]]). Two such
   *  records (a broadcast and its encore) are no answer — nor are two records carrying one title,
   *  however each reads as billed (US "NT Live: Hamlet": the National Theatre's 2010 and 2015
   *  broadcasts) — and neither is one that another candidate
   *  the title names fits at least as well on the listing's own facts: a banner is learned, not
   *  known, and Everyman's "Cellar Door x ThoughtBubble presents: Terminator 2: Judgment Day",
   *  publishing nothing but its title, is Cameron's film, not TMDB's making-of documentary. */
  private[identity] def houseProductionWhy(ranked: Seq[Scored]): Verdict = {
    val eligible = eligibleOf(ranked)
    for {
      // nor one from another year than the title dates the production: US "MetOpera: Carmen (2009)" ×513 is Eyre's
      // 2009 staging, not the Met's 2024 record its house bills
      house <- { val billed = eligible.filter(candidate => candidate.houseProduction && !contradicted(candidate))
                 one(billed.filter(_.number("titleYear.delta").forall(FactRelations.nearDelta)),
                   if (billed.isEmpty) "no record bills its work under its house" else "its house's record is from another year than its title dates",
                   "two records bill its work under its house", billed.map(Acceptance.named).mkString("; ")) }
      _     <- noneOf(eligible.find(other => (other ne house) &&
                 (IdentityMeasures.key(other.candidate.film.title) == IdentityMeasures.key(house.candidate.film.title) ||
                   (other.titleNamesIt && own(other) >= own(house)))), "another record carries its title or fits as well", house)
    } yield atOwn(house)
  }

  /** The one eligible record of the listing's RELAY ([[RelayStage]]): its house's record of its work from the season its
   *  title dates, whatever running time it files, that no year it publishes contradicts and no director it credits
   *  denies. US "MetOpera: Carmen (2009)" [240′] ×514 is the Met's 2009/10 broadcast, TMDB's "Bizet: Carmen" (aired 15
   *  January 2010, 166′), not the Met's 2019 or 2024 Carmen; DE "Royal Ballet & Opera im Kino: Manon" [2026] {Kenneth
   *  MacMillan} ×59 is "Royal Ballet & Opera 2026/27: Manon" (October 2026), not the Met's 2026/27 Manon (April 2027), which
   *  "MET Opera Live im Kino: Manon" [2027] ×4 is; DE "RBO live 2027: Tosca" is the RBO's 2026/27 record, not the ROH's
   *  2011 one IMDb suggests. It pre-empts every rule, as [[seasonProduction]] does. */
  private[identity] def relayRecord(ranked: Seq[Scored]): Option[Accepted] = ownInstalment(relayRecordWhy(ranked)).toOption
  private[identity] def relayRecordWhy(ranked: Seq[Scored]): Verdict =
    for {
      any    <- firstCandidate(ranked)
      relay  <- any.listing.relayBilled.toRight(Refused("it bills no house's relay of a work in a season"))
      // a year's two seasons each holding a record of it: the one dated that very year (UK Gorleston's "RBO: THE NUTCRACKER
      // 2026" is the 2026/27 record, not the 2025/26 one)
      records = eligibleOf(ranked).filter(scored => RelayStage.recordOf(relay, scored.candidate.film))
      // a record of its screenings' season airing on a day it screens is the broadcast it screens, whatever older year its
      // title dates the staging by: UK Ilkley's "The Royal Ballet: The Nutcracker (2025)", screening 1 December 2026, is the
      // 2026/27 relay aired 29 November, which 112 Flicks venues bill "RBO Cinema Season 2026-27: The Nutcracker (2025)"
      aired   = records.filter(scored => relay.airedOn(scored.candidate.film))
      record <- one(if (aired.nonEmpty) aired
                    else Some(records.filter(scored => relay.year.exists(scored.candidate.film.year.contains))).filter(_.sizeIs == 1).getOrElse(records),
                  "no record of its house's relay of its work from its season", "two records of its house's relay of its work from its season",
                  s"works ${relay.works.toSeq.sorted.mkString(",")}, seasons ${relay.seasons.toSeq.sorted.mkString(",")}")
      _      <- needOf(!record.number("year.distance").exists(_ > YearWindow.PublishedAdjacency), "a published year contradicts it", record, contradiction(record))
      _      <- needOf(!record.category("director").exists(Set("different", "different_script")) || creditsAnotherWorksRecord(ranked, relay),
                  "it credits another director", record)
    } yield atOwn(record)

  /** Does the relay credit the director of its house's record of ANOTHER work — a credit the venue copied from another
   *  relay's page, which says nothing of which production of its own work it screens? US Showcase's "The Metropolitan
   *  Opera: Samson et Dalila ENCORE (2026)" ×7 credits Louisa Proske, who directs the Met's 2026/27 "Macbeth" in its
   *  pool, not its Samson (Darko Tresnjak). A credit naming a director of none of its house's records, or of a record of
   *  its own work, still denies. */
  private def creditsAnotherWorksRecord(ranked: Seq[Scored], relay: RelayStage.Billed): Boolean = ranked.exists { scored =>
    val film = scored.candidate.film
    scored.category("director").contains("same_person") && film.stageWorkIds.nonEmpty && !film.stageWorkIds.exists(relay.works) && relay.itsHouse(film)
  }

  /** The one SEASON record whose title is the listing's own once its season is taken out ("&" and "and" alike), when
   *  the listing names no season: UK "Royal Ballet and Opera: Romeo and Juliet" {Kenneth MacMillan} ×127, screening
   *  May–June 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet" — the cut alone denied it, the season marker
   *  reading as a title the listing does not carry. Two seasons of the title ("…: Tosca", 2025/26 and 2026/27) are told
   *  apart by the season the listing's screenings broadcast it in ([[ofItsBroadcastSeason]]), and are no answer without
   *  one; a fact contradicting it, or another director, refuses it.
   *
   *  So is the record of the very season the listing's screenings broadcast it in that names its stage work under its
   *  house ([[StageRelays.bannerNamesItsHouse]]), however the venue shortens the banner: prod UK 2026-10-13 "MET:
   *  Macbeth" (17 October 2026), "RBO: Tosca 27" and "Macbeth (Met Opera)" are the Met's and RBO's 2026/27 records. */
  private[identity] def seasonRecord(ranked: Seq[Scored]): Option[Accepted] = ownInstalment(seasonRecordWhy(ranked)).toOption
  /** [[seasonRecord]]'s answer when it is a record of the season the listing's screenings broadcast it in: it pre-empts
   *  every rule, as [[seasonProduction]] does a season the title names — a house's other stagings of the work (the Met's
   *  2014 "Macbeth", the Royal Ballet's 2013 "Romeo and Juliet") are other broadcasts than the one the venue screens. */
  private[identity] def broadcastSeasonRecord(ranked: Seq[Scored]): Option[Accepted] = seasonRecord(ranked).filter { case (record, _) =>
    record.listing.screenedSeason.exists(record.candidate.film.season.contains)
  }
  /** The relay's record ([[relayRecord]]) or the season record its screenings' season names ([[broadcastSeasonRecord]]),
   *  with the rule naming it: what pre-empts the rules after [[seasonProduction]]. */
  private[identity] def relayOrBroadcastSeasonRecord(ranked: Seq[Scored]): Option[(Accepted, String)] =
    relayRecord(ranked).map(_ -> "relay-record").orElse(broadcastSeasonRecord(ranked).map(_ -> "season-record"))
  private[identity] def seasonRecordWhy(ranked: Seq[Scored]): Verdict = {
    def plain(title: String) = IdentityMeasures.key(title.replace("&", " and "))
    for {
      any    <- firstCandidate(ranked)
      listing = any.listing
      _      <- need(listing.seasonYear.isEmpty, "the title names its season")
      own     = (Seq(listing.title) ++ listing.rawTitle).map(plain).toSet
      record <- one(ofItsBroadcastSeason(ranked.filter(scored => (isEligible(scored) || scored.deniedByCutOnly) &&
                  scored.candidate.film.season.isDefined &&
                  (scored.candidate.film.titles.exists(t => own(plain(IdentityMeasures.withoutSeasons(t)))) || StageRelays.screenedSeasonRecord(listing, scored.candidate.film)))),
                  "no season record carries its title or its work under its house", "two seasons' records carry its title")
      _      <- needOf(!contradicted(record), "a published fact contradicts it", record, contradiction(record))
      // a season spans two years: a year the listing states outside them is another season's broadcast (UK Aldeburgh's
      // "The Royal Ballet: Romeo & Juliet" [2025] is no "The Royal Ballet: Romeo and Juliet (2021/22)") — but for the
      // record of the season its screenings broadcast it in, whose year it predates: that year dates the staging's premiere
      // (Aldeburgh's, screening on 25 May 2027, is "Royal Ballet & Opera 2026/27: Romeo and Juliet", aired that day)
      _      <- needOf(listing.year.forall(year => record.candidate.film.season.forall(season => year == season || year == season + 1)) ||
                  StageRelays.screenedSeasonRecord(listing, record.candidate.film), "the year it states is another season's", record)
      _      <- needOf(!record.category("director").exists(Set("different", "different_script")), "it credits another director", record)
    } yield atOwn(record)
  }

  /** Of several season records of one stage work, the one season the listing's screenings broadcast it in
   *  ([[IdentityMeasures.Listing.screenedSeason]]) when exactly that season's records are among them; all of them
   *  otherwise. A relay airs on its house's published dates: UK "Royal Ballet & Opera: Swan Lake" (recording
   *  38342304766, ~30 venues billing no season or year), screening on 22 April 2027, is the 2026/27 record, not the
   *  house's 2024/25 one. */
  private[identity] def ofItsBroadcastSeason(records: Seq[Scored]): Seq[Scored] =
    records.headOption.flatMap(_.listing.screenedSeason).filter(_ => records.sizeIs > 1)
      .map(season => records.filter(record => record.candidate.film.season.contains(season)))
      .filter(_.nonEmpty).getOrElse(records)
}
