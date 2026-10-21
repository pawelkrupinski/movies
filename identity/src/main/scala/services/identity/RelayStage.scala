package services.identity

import services.identity.IdentityMeasures.{Film, Listing}

/** A stage house's RELAY matched as one: the HOUSE its billing names in particular ([[StageRelays.Relay.houses]]), the
 *  stage WORK ([[StageWorks]], in any language) and the SEASON the performance it screens falls in. A record of that
 *  house, of that work, from that season is the relay's own whatever else it files — a running time TMDB holds for a
 *  broadcast is a placeholder or the stage's, and its title is the house's banner on the work, so the learned vetoes on
 *  a running-time gap or a mere title overlap say nothing of it ([[CandidateScoring.evidenceDenial]]). Which record
 *  that is, [[Acceptance]]'s `relay-record` rule takes: exactly one, or none.
 *
 *  The season, read off the billing:
 *  - the season its title names ("Met Opera 2026-27: Così fan tutte");
 *  - else the year its title brackets, the performance's ("MetOpera: Carmen (2009)", the 2009/10 broadcast aired January
 *    2010): that season or the one before. Never an ENCORE's year, which dates the encore, not the performance ("The
 *    Metropolitan Opera: Samson et Dalila ENCORE (2026)");
 *  - else nothing: a relay billing no date is the season rules' ([[AcceptanceStageRules]]).
 *  A banner naming no house in particular ("OPERA 2026/2027 - CARMEN", "Ballet - Notre-Dame de Paris") names no record
 *  by it, nor does a house TMDB holds no record of (the Opéra national de Paris' "La fanciulla del West" is not the Met's). */
private[identity] object RelayStage {

  /** A relay as its billing reads: the houses it names in particular, its stage works, and the seasons its performance
   *  can fall in (a season by the year it opens). */
  final case class Billed(houses: Seq[Seq[String]], works: Set[String], seasons: Set[Int])

  /** The relay `l` bills, when it names a house in particular, a stage work and a season. */
  def of(l: Listing): Option[Billed] =
    if (!l.billsAStageHouse) None
    else {
      val houses = l.relay.houses.filter(house => StageRelays.namesAHouseInParticular(house))
      val works  = l.billedStageWorks
      val seasons = l.seasonYear.map(Set(_)).orElse(
        Option.when(!billsAnEncore(l))(l.titleYear).flatten.map(year => Set(year - 1, year))).getOrElse(Set.empty)
      Option.when(houses.nonEmpty && works.nonEmpty && seasons.nonEmpty)(Billed(houses, works, seasons))
    }

  /** Is `f` the record of the relay `l` bills ([[of]]): its titles name the relay's house ([[StageRelays.namesOneHouse]])
   *  and its work, and it is dated in one of the relay's seasons — the season its title names, else the season its air day
   *  falls in, else either season its year is in? */
  def recordOf(l: Listing, f: Film): Boolean = l.relayBilled.exists(relay => recordOf(relay, f))
  def recordOf(relay: Billed, f: Film): Boolean =
    f.stageWorkIds.exists(relay.works) &&
      relay.houses.exists(house => f.houseNames.exists(StageRelays.namesOneHouse(house, _))) &&
      seasonsOf(f).exists(relay.seasons)

  /** The seasons a record can be of: the one its title names, else the one its air day falls in, else either its year is
   *  in. */
  private def seasonsOf(f: Film): Set[Int] =
    f.season.map(Set(_))
      .orElse(f.released.map(day => Set(if (day.getMonthValue >= ScreeningDays.SeasonOpens) day.getYear else day.getYear - 1)))
      .orElse(f.year.map(year => Set(year - 1, year))).getOrElse(Set.empty)

  private val Encore = """(?i)(?<![\p{L}])encore(?![\p{L}])""".r
  /** Does the billing bill an ENCORE ("… ENCORE (2026)", "(2026 ENCORE)")? Its year is the encore's. */
  private def billsAnEncore(l: Listing): Boolean = l.ownTitles.exists(Encore.findFirstIn(_).isDefined)
}
