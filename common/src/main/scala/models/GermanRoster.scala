package models

/** Materialises the generated German roster (`GermanRosterData`: 436 town and cluster pages,
 *  1,515 cinemas) into [[RosterPlace]]s + `GermanCinema` venues, once. Exposes the places for
 *  `City.germanPages`, the by-BUNDESLAND grouping for `Cinema.byCity` (the uptime page's level, as
 *  Spain's is the province), each cinema's venue key (its Filmstarts `theaterId`, or the feed that
 *  found it) for the scrape catalog, and its kinoprogramm.com fallback. */
object GermanRoster {
  /** Display names already spoken for by a hand-declared source, which a German venue may not
   *  reuse ([[GeneratedRoster.wireName]]). The roster is generated, so the guard has to live here
   *  rather than in the data: regenerating `GermanRosterData` from Filmstarts would otherwise
   *  reintroduce any collision that got hand-fixed there. Two exist today — a German "Studio"
   *  against Opole's `KinoStudio`, and a German "Cineworld" against the `CineworldChain` detail
   *  source — and both once silently rebound a Polish/UK venue's showtimes to a German one. */
  // Lazy, once per roster: as a `def` it was rebuilt for every venue the roster builds — the
  // set of every other country's names, thousands of times over — 8.7% of worker-uk's CPU in
  // its first ten boot minutes (JFR 2026-10-02).
  private lazy val claimedElsewhere: Set[String] = Cinema.displayNamesClaimedBy()

  /** "Bayreuth und Umgebung", which reads right both alone and after "in". */
  val style: RosterStyle = RosterStyle("und Umgebung")

  private val built: Seq[(RosterPlace, Seq[(GermanCinema, String, Option[String])])] =
    GermanRosterData.pages.map { case (slug, qualified, name, bundesland, lat, lon, multiTown, towns, cinemas) =>
      // Qualified with the venue's own TOWN, so a re-cluster never changes a wire key.
      val venues = cinemas.map { case (disp, pill, tid, kinoprogramm, town) =>
        val opening = Opening.generated(GermanRosterData.seasonal(disp), SeasonWindow.Germany)
        (new GermanCinema(GeneratedRoster.wireName(claimedElsewhere, disp, s"$disp $town"), pill, opening),
         tid, kinoprogramm)
      }
      (RosterPlace(slug, qualified, name, bundesland, lat, lon, TimeZones.Germany, multiTown, towns, venues.map(_._1)),
       venues)
    }

  val places: Seq[RosterPlace] = built.map(_._1)
  /** Venues by Bundesland, in roster order — the grouping `Cinema.byCity` reports
   *  Germany at (the uptime page), one level above the pages. */
  val byCity: Seq[(String, Seq[Cinema])] = GeneratedRoster.venuesByRegionInOrder(places)
  /** Each venue's key: its Filmstarts `theaterId`, or — for a venue Filmstarts does not
   *  list — the feed that found it ("cineamo:997"), which its sources are filed under. */
  val venueKeyByCinema: Map[Cinema, String] = built.flatMap(_._2).map { case (c, key, _) => c -> key }.toMap
  /** Each Filmstarts-listed venue's `theaterId`. */
  val theaterIdByCinema: Map[Cinema, String] = venueKeyByCinema.filterNot(_._2.contains(':'))
  /** Each venue's kinoprogramm.com page path, its showtime fallback behind Filmstarts;
   *  absent for a venue `harvest_kinoprogramm.py` could not match with confidence. */
  val kinoprogrammPathByCinema: Map[Cinema, String] =
    built.flatMap(_._2).collect { case (c, _, Some(path)) => c -> path }.toMap
}
