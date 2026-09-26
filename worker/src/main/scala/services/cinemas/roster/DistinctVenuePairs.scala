package services.cinemas.roster

import models.{BakerStreetCinemaAbergavenny, Cinema, ColiseumCinemaBrecon, FloraCinemaHelston, KinoMikro, MerlinWellesleyWellington,
  MikroBronowice, OdeonCinemaChelmsford, OdeonCinemaColchester, OdeonCinemaLlanelli, OdeonCinemaNewark, OdeonCinemaSwadlincote,
  RitzBurnhamOnSea, RoyalStIvesCinema, TheAvenueCinemaMinehead, TivoliTiverton, WTWLighthouseNewquay, WTWWhiteRiverCinema,
  WestwayCinemaFrome}
import play.api.Logging

/**
 * Pairs of roster venues that LOOK like one screen listed twice — by name (the offline roster
 * audit, `CinemaRosterAuditSpec`) or by programme (`services.metrics.DuplicateVenueCensus`) —
 * and are genuinely two. One list for both checks, so a pair cleared by one is cleared by the
 * other and the reason is written down once.
 *
 * The programme matches below were each checked against prod (2026-09-23): the two venues are
 * in different towns (in the second block, different CITIES — the census's cross-city scope),
 * and their showtimes book through two different per-venue ticketing ids, so the upstream
 * holds two listings that a small chain (or an operator running two screens) happens to
 * programme alike. Since 2026-09-26 the census clears such a pair itself when the booking links
 * differ, so a new entry is needed only when a venue carries none. Generated-roster venues
 * (Germany, Spain, the US) have no case object, so they are named by the display name they are
 * stored under — and a name a roster regeneration dropped is logged and skipped, never a failed
 * load.
 */
object DistinctVenuePairs extends Logging {
  private val cased: Set[Set[Cinema]] = Set(
    Set(KinoMikro, MikroBronowice),   // Kino Mikro (Juliusza Lea) and its second screen in Bronowice
    // Merlin Cinemas, Helston and St Ives: admit-one merlinhelston vs merlinstives
    Set(FloraCinemaHelston, RoyalStIvesCinema),
    // WTW Cinemas, Newquay and St Austell: wtwcinemas.co.uk/newquay vs /st-austell booking
    Set(WTWLighthouseNewquay, WTWWhiteRiverCinema),
    // Odeon's template schedule in two Essex towns: showtime ids 498-* vs 515-*, own screens
    Set(OdeonCinemaChelmsford, OdeonCinemaColchester),
    // S&B Cinemas' three Somerset towns: sandbcinemas.co.uk/<venue>/booknow per venue
    Set(RitzBurnhamOnSea, TheAvenueCinemaMinehead),
    Set(RitzBurnhamOnSea, WestwayCinemaFrome),
    Set(TheAvenueCinemaMinehead, WestwayCinemaFrome),

    // ── Two cities, 95%+ alike (the census's cross-city scope) ──
    // Abergavenny and Brecon: internet-ticketing sites BAKABE vs COLBRE, own perfcodes
    Set(BakerStreetCinemaAbergavenny, ColiseumCinemaBrecon),
    // Merlin Cinemas, Wellington and Tiverton: admit-one merlinwellington vs merlintiverton
    Set(MerlinWellesleyWellington, TivoliTiverton),
    // Odeon's template schedule: showtime ids 760-* (Llanelli), 757-* (Newark), 759-* (Swadlincote)
    Set(OdeonCinemaLlanelli, OdeonCinemaNewark),
    Set(OdeonCinemaLlanelli, OdeonCinemaSwadlincote),
  )

  /** Generated-roster pairs (Germany, Spain, the US), by the display name each is stored under. */
  private val byName: Seq[(String, String)] = Seq(
    // Cineplex on Lake Constance and in the Hegau: Filmstarts theatres A0313 vs A0323
    ("Cineplex Friedrichshafen", "Cineplex Singen"),
    // Cines Victoria, Extremadura: theatres E0372 (Don Benito) vs E0383 (Mérida)
    ("Cines Victoria Don Benito", "Cines Victoria Mérida"),
    // Atlantic Beach and Emerald Isle, NC: formovietickets chains atlanticstation vs emeraldplantaion
    ("Atlantic Station Cinema", "Emerald Plantation Cinemas"),
    // Jordan's Furniture's two IMAX domes, Natick and Reading, sharing one release slate
    ("IMAX 3D Natick (Jordan's)", "IMAX 3D Reading (Jordan's)"),
    // St Helens and Gresham, OR: formovietickets chains columbiatheatre vs mthood
    ("Columbia St Helens", "Mt Hood Theatre Gresham"),
    // State Theatres in Boscobel and Dodgeville, WI: booking sites 24552 vs 80756
    ("Blaine Theatre Boscobel", "Dodge Theatre"),
    // Wunderland's North Portland (Avalon) and Beaverton houses, one second-run slate
    ("Avalon Theatre Portland", "Beaverton Wunderland"),
    // One Illinois/Iowa operator's small towns: internet-ticketing FOXFOR, MAJCAN, TAYTAY
    ("Fox Theatre Fort Madison", "Majestic Theatre of Canton"),
    ("Fox Theatre Fort Madison", "Taylorville Cinema"),
    ("Majestic Theatre of Canton", "Taylorville Cinema"),
    // Three Rau's Entertainment, Shenandoah and Le Mars IA: ticket sites 00001-00002 vs 00001-00003
    ("Legacy Theatre Shenandoah", "Royal 3 Cinema Le Mars"),
    // RMC Stadium, Jacksonville and Waterloo IL: formovietickets rtn 39924 vs 14446
    ("RMC Jacksonville", "RMC Waterloo Cinema"),
    // Two small-town twins 500 miles apart, East Jamestown TN and Laurinburg NC: Flicks venues
    // castle-twin-jamestown vs cinema-laurinburg, 21 vs 22 showtimes of the same three wide
    // releases on different day calendars (checked 2026-09-25). Listed by hand because Laurinburg
    // carries no booking links, so the census cannot tell the two apart by them.
    ("Castle Twin Jamestown", "Cinema Laurinburg"),
  )

  /** Names in [[byName]] the roster no longer holds — a regeneration renamed or dropped the
   *  venue. Their pairs are left out rather than failing the worker's load; the census may
   *  then count that pair again, and `DuplicateVenueCensusSpec` fails until the entry is fixed. */
  private val resolved: (Set[Set[Cinema]], Seq[String]) = resolve(byName, Cinema.byDisplayName.get)
  val unresolved: Seq[String] = resolved._2
  if (unresolved.nonEmpty)
    logger.warn(s"DistinctVenuePairs: ${unresolved.size} venue name(s) on no roster, their pairs ignored: ${unresolved.mkString(", ")}")

  val all: Set[Set[Cinema]] = cased ++ resolved._1

  def contains(a: Cinema, b: Cinema): Boolean =
    all(Set(a, b))

  /** Each named pair whose venues `lookup` finds, and every name it does not. */
  private[services] def resolve(named: Seq[(String, String)], lookup: String => Option[Cinema]): (Set[Set[Cinema]], Seq[String]) = {
    val pairs   = named.flatMap { case (a, b) => for (x <- lookup(a); y <- lookup(b)) yield Set(x, y) }.toSet
    val missing = named.flatMap { case (a, b) => Seq(a, b) }.distinct.filter(lookup(_).isEmpty)
    (pairs, missing)
  }
}
