package services.identity.measures

import services.identity.measures.IdentityMeasures.*

/**
 * The SEASON a stage house's relay airs in, read off a listing's screenings and billing for the identity measures
 * ([[IdentityMeasures]]): the season record a bare or banner-billed work names ([[StageRelays.stageWorks]]), the year a
 * performance or a banner gives, the air day a screening falls on. How a relay is billed is [[StageRelays]]'.
 */
object StageSeasonRecords {
  /** Do the season rules read the season `l`'s screenings fall in ([[SearchMeasures.stageWorkQueries]], [[RelayStage.of]],
   *  [[bareWorksSeasonRecord]])? Wherever its billing names a stage house or a stage work. That season is then part of the
   *  evidence its node is keyed by ([[Evidence.key]]): UK Riverside's "Romeo and Juliet (2027)", screening on 25 May 2027,
   *  searches "Romeo and Juliet 2026" and a billing alike screening in October 2027 does not — one node held both, searched
   *  as whichever listing headed it. */
  private[identity] def readsItsScreeningsSeason(l: MeasuredListing): Boolean = l.billsAStageHouse || l.billedStageWorks.nonEmpty

  /** Is `f` the season record of the work `l`'s whole title bills bare ([[barelyTimed]]), of the very season its
   *  screenings fall in ([[MeasuredListing.screenedSeason]])? A house relays the work that season, and a bare billing of it then
   *  names the relay as much as any film of the title: UK Theatr Gwaun Fishguard's "Romeo and Juliet", screening on 25 May
   *  2027 beside the "Royal Ballet & Opera 2026/27: Romeo and Juliet" its own search finds, is no proof of Zeffirelli's. */
  def bareWorksSeasonRecord(l: MeasuredListing, f: Film): Boolean =
    barelyTimed(l) && l.dating.screenedSeason.exists(f.season.contains) && StageRelays.stageWorks(f).exists(l.billedStageWorks)

  /** Is the listing's whole title a stage work, stating no year and no credit — a running time at most, which
   *  [[MeasuredListing.broadcastSeason]] reads as naming its staging? A running time names no staging by itself: UK Empire Theatre
   *  Halstead's bare "Romeo and Juliet" [170′], screening on 25 May 2027, is the RBO 2026/27 relay aired that day, which
   *  no search of the work's name alone reaches — the broadcast join ([[agreement.Broadcast]]) tells it from a film by
   *  its day. A credit does name one (Jacquot's "Tosca"), and a title going on past the work is a film's ("Lord Of The
   *  Rings: The Fellowship Of The Ring", the musical's name only its head). */
  private[identity] def barelyTimed(l: MeasuredListing): Boolean =
    l.statedYear.isEmpty && l.directors.isEmpty && l.ownTitles.exists(title => StageWorks.resolver.named(key(title)).nonEmpty)

  private val BannerYear = """(?<![\p{N}])(20\d{2})(?![\p{N}/–—-])""".r
  /** A year a text gives a PERFORMANCE: "the indelible 2010 Met performance", "its 2019 performance" — the year, at most
   *  two words before "performance". */
  private val PerformanceYear = """(?i)(?<![\p{N}])((?:19|20)\d{2})(?![\p{N}])(?:\s+[\p{L}’'.-]+){0,2}\s+performance(?![\p{L}])""".r
  /** The one year `text` (a venue's synopsis) gives the performance it screens, if it gives exactly one: what dates an
   *  encore's relay ([[RelayStage.of]]). */
  def performanceYear(text: String): Option[Int] =
    Some(PerformanceYear.findAllMatchIn(text).map(_.group(1).toInt).toSeq.distinct).collect { case Seq(one) => one }

  /** The one year a relay's banner dates its broadcast by — a piece of its title naming no stage work, beside one that
   *  does: DE "RBO live 2027: Tosca". A year beside the work is the work's own ("Macbeth 2026" is [[SearchMeasures.stageWorkQueries]]'). */
  def bannerYear(l: MeasuredListing): Option[Int] = {
    val (works, banners) = l.ownTitles.flatMap(t => PieceBreak.split(withoutSeasons(t)).toSeq)
      .partition(piece => StageWorks.resolver.named(key(BannerYear.replaceAllIn(piece, ""))).nonEmpty)
    Option.when(works.nonEmpty)(banners.flatMap(BannerYear.findAllMatchIn).map(_.group(1).toInt).distinct).collect { case Seq(one) => one }
  }

  /** Is `f` the house's record of the very season `l`'s screenings broadcast its work in ([[MeasuredListing.screenedSeason]]): a
   *  season record naming a stage work the listing bills, under the listing's house ([[StageRelays.bannerNamesItsHouse]])? A relay
   *  airs on its house's published dates, so a venue screening it then screens that season's broadcast. */
  def screenedSeasonRecord(l: MeasuredListing, f: Film): Boolean =
    l.dating.screenedSeason.exists(f.season.contains) &&
      StageRelays.stageWorks(f).exists(l.billedStageWorks) && StageRelays.bannerNamesItsHouse(l, f) && airsOnItsDay(l, f)

  /** Does a listing whose year is more than a season before its screened season ([[MeasuredListing.airDays]]) screen on the
   *  record's air day — its release date, give or take [[AirDayWindow]] days? A nearer year passes with no test. */
  private def airsOnItsDay(l: MeasuredListing, f: Film): Boolean =
    l.dating.airDays.isEmpty || f.released.exists(aired => l.dating.airDays.exists(day => math.abs(java.time.temporal.ChronoUnit.DAYS.between(aired, day)) <= AirDayWindow))
  /** How many days from a relay's release date a screening still airs it: a venue's own date, a time zone apart. */
  val AirDayWindow = 3
}
