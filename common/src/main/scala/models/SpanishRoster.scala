package models

/** Materialises the generated Spanish roster into [[RosterPlace]]s +
 *  `SpanishCinema` venues, once. Exposes the places for `City.spanishCities`,
 *  the by-PROVINCE grouping for `Cinema.byCity` (the uptime page's level, as the
 *  US's is the state), and each cinema's SensaCine `theaterId` for the scrape
 *  catalog — the Spanish counterpart of [[GermanRoster]] and [[UsRoster]]. */
object SpanishRoster {
  /** Display names already spoken for — every hand-declared, German and US venue's — which a
   *  Spanish venue may not reuse ([[GeneratedRoster.wireName]]). The generator already refuses to
   *  emit a collision WITHIN Spain (`data/spain/scripts/generate_roster.py`); this catches the
   *  cross-country case. Reads the rosters' `byCity`, the names actually built. */
  // Lazy, once per roster, as `GermanRoster.claimedElsewhere` is.
  private lazy val claimedElsewhere: Set[String] =
    Cinema.displayNamesClaimedBy(GermanRoster.byCity, UsRoster.byCity)

  /** "Sitges y alrededores", which reads right both alone and after "en". */
  val style: RosterStyle = RosterStyle("y alrededores")

  private val built: Seq[(RosterPlace, Seq[(SpanishCinema, Option[String], Option[String])])] =
    SpanishRosterData.pages.map { case (slug, qualified, name, province, lat, lon, zone, multiTown, towns, cinemas) =>
      // Qualified with the PROVINCE, as they were when a page was one, so a re-cluster
      // never changes a venue's wire key.
      val venues = cinemas.map { case (disp, pill, theaterId, ocineServer) =>
        val opening = Opening.generated(SpanishRosterData.seasonal(disp), SeasonWindow.Spain)
        (new SpanishCinema(GeneratedRoster.wireName(claimedElsewhere, disp, s"$disp $province"), pill, opening),
         theaterId, ocineServer)
      }
      (RosterPlace(slug, qualified, name, province, lat, lon, TimeZones.named(zone), multiTown, towns,
                   venues.map(_._1)), venues)
    }

  val places: Seq[RosterPlace] = built.map(_._1)
  /** Venues by province, provinces in roster order — the grouping `Cinema.byCity`
   *  reports Spain at (the uptime page), one level above the pages. */
  val byCity: Seq[(String, Seq[Cinema])] = GeneratedRoster.venuesByRegionInOrder(places)

  /** Each SensaCine-listed venue's `theaterId`. The Ocine venues SensaCine does
   *  not list (`data/spain/ocine.json`'s `unlisted`) have none, and are absent. */
  val theaterIdByCinema: Map[Cinema, String] =
    built.flatMap(_._2).collect { case (c, Some(theaterId), _) => (c: Cinema) -> theaterId }.toMap

  /** Each Ocine venue's ticketing-server slug (`tickets.ocine<slug>.es`), the
   *  scrape primary for every venue named here — SensaCine carries no programme
   *  for most of them, and does not list some at all. From `data/spain/ocine.json`,
   *  which says which venues are (not) on it and why. */
  val ocineServerByCinema: Map[Cinema, String] =
    built.flatMap(_._2).collect { case (c, _, Some(slug)) => (c: Cinema) -> slug }.toMap
}
