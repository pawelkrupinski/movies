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

  /** A relay as its billing reads: the houses it names in particular (none: a banner of the art form alone, "OPERA-
   *  PARSIFAL"), its stage works, the seasons its performance can fall in (a season by the year it opens), which records
   *  name its house ([[StageRelays.bannerNamesItsHouse]]), and the year it dates them by, when no season. */
  final case class Billed(houses: Seq[Seq[String]], works: Set[String], seasons: Set[Int], itsHouse: Film => Boolean, year: Option[Int]) {
    /** Does it name a house in particular — what lifts the learned vetoes off its record? */
    def particular: Boolean = houses.nonEmpty
  }

  /** The relay `l` bills, when it names a stage house, a stage work and a season:
   *  - the season its title names;
   *  - else the year its title brackets, the performance's ("MetOpera: Carmen (2009)", aired January 2010; "Royal Ballet:
   *    The Nutcracker (2021)", its 2021 recording): that season or the one before;
   *  - else the year its field or its banner states ("Royal Ballet & Opera im Kino: Manon" [2026], "RBO live 2027:
   *    Tosca"), unless it is before the season its screenings fall in, when it dates the staging's premiere (UK
   *    Aldeburgh's "Royal Ballet: The Nutcracker" [2025], screening in 2026/27) — that season or the one before.
   *  Never an ENCORE's year, which dates the encore ("The Metropolitan Opera: Samson et Dalila ENCORE (2026)"). */
  def of(listing: Listing): Option[Billed] = {
    val l = camelSplit(listing)
    if (!l.billsAStageHouse) None
    else {
      val works   = l.billedStageWorks
      val stated  = l.year.orElse(StageRelays.bannerYear(l)).filter(year => l.screeningsSeason.forall(year >= _))
      val dated   = if (billsAnEncore(l)) None else l.titleYear.orElse(stated)
      val seasons = l.seasonYear.map(Set(_)).orElse(dated.map(year => Set(year - 1, year))).getOrElse(Set.empty)
      Option.when(works.nonEmpty && seasons.nonEmpty)(
        Billed(l.relay.houses.filter(StageRelays.namesAHouseInParticular), works, seasons, StageRelays.bannerNamesItsHouse(l),
          dated.filter(_ => l.seasonYear.isEmpty)))
    }
  }

  /** Where a lower-case letter runs into an upper-case one: the seam of a camel-cased word. */
  private val CamelSeam = """(?<=\p{Ll})(?=\p{Lu})""".r
  /** The listing with its titles' camel-cased words read as their parts: US Flicks' "MetOpera" is the Met's "Met Opera",
   *  a house and its art. Read for the relay alone: the search and every other rule read the billing as billed. */
  private def camelSplit(l: Listing): Listing = {
    def split(title: String) = CamelSeam.replaceAllIn(title, " ")
    if (l.ownTitles.forall(title => split(title) == title)) l else l.copy(title = split(l.title), rawTitle = l.rawTitle.map(split))
  }

  /** Is `f` the record of the relay `l` bills ([[of]]), naming a house in particular: what no learned veto denies, and no
   *  sibling's denial withdraws? */
  def particularRecordOf(l: Listing, f: Film): Boolean = l.relayBilled.exists(relay => relay.particular && recordOf(relay, f))
  /** Is `f` the relay's record: its titles name the relay's house ([[StageRelays.bannerNamesItsHouse]]) and its work, and
   *  it is dated in one of the relay's seasons — the season its title names, else the season its air day falls in, else
   *  either season its year is in? */
  def recordOf(relay: Billed, f: Film): Boolean =
    f.stageWorkIds.exists(relay.works) && relay.itsHouse(f) && seasonsOf(f).exists(relay.seasons)

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
