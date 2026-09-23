package services.cinemas.roster

import models.{BakerStreetCinemaAbergavenny, Cinema, ColiseumCinemaBrecon, FloraCinemaHelston, KinoMikro, MerlinWellesleyWellington,
  MikroBronowice, OdeonCinemaChelmsford, OdeonCinemaColchester, OdeonCinemaLlanelli, OdeonCinemaNewark, OdeonCinemaSwadlincote,
  RitzBurnhamOnSea, RoyalStIvesCinema, TheAvenueCinemaMinehead, TivoliTiverton, WTWLighthouseNewquay, WTWWhiteRiverCinema,
  WestwayCinemaFrome}

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
 * programme alike. Generated-roster venues (Germany, Spain, the US) have no case
 * object, so they are named by the display name they are stored under.
 */
object DistinctVenuePairs {
  val all: Set[Set[Cinema]] = Set(
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
    // Cineplex on Lake Constance and in the Hegau: Filmstarts theatres A0313 vs A0323
    named("Cineplex Friedrichshafen", "Cineplex Singen"),
    // Cines Victoria, Extremadura: theatres E0372 (Don Benito) vs E0383 (Mérida)
    named("Cines Victoria Don Benito", "Cines Victoria Mérida"),
    // Atlantic Beach and Emerald Isle, NC: formovietickets chains atlanticstation vs emeraldplantaion
    named("Atlantic Station Cinema", "Emerald Plantation Cinemas"),
    // Jordan's Furniture's two IMAX domes, Natick and Reading, sharing one release slate
    named("IMAX 3D Natick (Jordan's)", "IMAX 3D Reading (Jordan's)"),
    // St Helens and Gresham, OR: formovietickets chains columbiatheatre vs mthood
    named("Columbia St Helens", "Mt Hood Theatre Gresham"),
    // State Theatres in Boscobel and Dodgeville, WI: booking sites 24552 vs 80756
    named("Blaine Theatre Boscobel", "Dodge Theatre"),
    // Wunderland's North Portland (Avalon) and Beaverton houses, one second-run slate
    named("Avalon Theatre Portland", "Beaverton Wunderland"),

    // ── Two cities, 95%+ alike (the census's cross-city scope) ──
    // Abergavenny and Brecon: internet-ticketing sites BAKABE vs COLBRE, own perfcodes
    Set(BakerStreetCinemaAbergavenny, ColiseumCinemaBrecon),
    // Merlin Cinemas, Wellington and Tiverton: admit-one merlinwellington vs merlintiverton
    Set(MerlinWellesleyWellington, TivoliTiverton),
    // Odeon's template schedule: showtime ids 760-* (Llanelli), 757-* (Newark), 759-* (Swadlincote)
    Set(OdeonCinemaLlanelli, OdeonCinemaNewark),
    Set(OdeonCinemaLlanelli, OdeonCinemaSwadlincote),
    // One Illinois/Iowa operator's small towns: internet-ticketing FOXFOR, MAJCAN, TAYTAY
    named("Fox Theatre Fort Madison", "Majestic Theatre of Canton"),
    named("Fox Theatre Fort Madison", "Taylorville Cinema"),
    named("Majestic Theatre of Canton", "Taylorville Cinema"),
    // Three Rau's Entertainment, Shenandoah and Le Mars IA: ticket sites 00001-00002 vs 00001-00003
    named("Legacy Theatre Shenandoah", "Royal 3 Cinema Le Mars"),
    // RMC Stadium, Jacksonville and Waterloo IL: formovietickets rtn 39924 vs 14446
    named("RMC Jacksonville", "RMC Waterloo Cinema"),
  )

  def contains(a: Cinema, b: Cinema): Boolean = all(Set(a, b))

  /** A generated-roster pair by stored display name — throws at load if either is gone, so a
   *  roster regeneration that renames one fails the build (`DuplicateVenueCensusSpec`) rather
   *  than silently dropping the entry. */
  private def named(a: String, b: String): Set[Cinema] = Set(Cinema.byDisplayName(a), Cinema.byDisplayName(b))
}
