package models

/** One addressable German page — Poland's and Spain's shape since 2026-10: a major
 *  city (a municipality of 100,000+) with only its own venues, a town with three or
 *  more, or a cluster of small towns a short drive apart named after the biggest
 *  ("Bayreuth und Umgebung"). Which page holds which venue is data, built by
 *  `data/germany/scripts/build_pages.py` with the shared rule
 *  (`data/scripts/town_pages.py`).
 *
 *  Plain data rather than a [[City]], for the reason [[SpanishPlace]] is: the slug a
 *  page ends up addressable under depends on what every OTHER country has claimed,
 *  and `City` is the only place that sees all of them. See `City.germanPages`. */
final case class GermanPlace(
  /** The slug this page wants — its own name, umlauts spelt out (`muenster`). */
  preferredSlug: String,
  /** The slug qualified with the Bundesland (`neustadt-hessen`) — what the page
   *  answers at where another country already serves or wants [[preferredSlug]]. */
  qualifiedSlug: String,
  /** The town the page is named after. */
  name:          String,
  /** The Bundesland of [[name]]'s town — what the picker groups the page under. */
  bundesland:    String,
  lat:           Double,
  lon:           Double,
  /** Several towns rather than one — the page is then "<name> und Umgebung". */
  multiTown:     Boolean,
  /** The page's towns, most venues first, [[name]] leading — the place names a
   *  visitor searches by; see [[City.coveredPlaces]]. */
  towns:         Seq[String],
  cinemas:       Seq[Cinema],
)

/** Materialises the generated German roster into [[GermanPlace]]s + `GermanCinema`
 *  venues, once. Exposes the places for `City.germanPages`, the by-BUNDESLAND
 *  grouping for `Cinema.byCity` (the uptime page's level, as Spain's is the
 *  province), each cinema's venue key (its Filmstarts `theaterId`, or the feed that
 *  found it) for the scrape catalog, and its kinoprogramm.com fallback. */
object GermanRoster {
  /** Display names already spoken for by a hand-declared source, which a generated German
   *  venue may NOT reuse.
   *
   *  `displayName` is the wire key every per-cinema slot is stored under (`movie_slots`,
   *  `screenings`, the embedded `sourceData` map), and `Source.byDisplayName` is a plain
   *  `toMap` — so two venues sharing a name silently collapse to whichever is built LAST,
   *  and the loser's stored slots read back as the WINNER. The roster is generated, so the
   *  guard has to live here rather than in the data: regenerating `GermanRosterData` from
   *  Filmstarts would otherwise reintroduce any collision that got hand-fixed there.
   *
   *  Two exist today — a German "Studio" against Opole's `KinoStudio`, and a German
   *  "Cineworld" against the `CineworldChain` detail source — and both silently rebound a
   *  Polish/UK venue's showtimes to a German one. `SourceWireKeySpec` fails on any third. */
  // Lazy, once per roster: as a `def` it was rebuilt for every venue the roster builds — the
  // set of every other country's names, thousands of times over — 8.7% of worker-uk's CPU in
  // its first ten boot minutes (JFR 2026-10-02).
  private lazy val claimedElsewhere: Set[String] = Cinema.displayNamesClaimedBy()

  private val built: Seq[(GermanPlace, Seq[(GermanCinema, String, Option[String])])] =
    GermanRosterData.pages.map { case (slug, qualified, name, bundesland, lat, lon, multiTown, towns, cinemas) =>
      // Qualify only the venues that would collide, so the roster's names — and the wire
      // keys of every already-stored German slot — stay exactly as they are otherwise.
      // Qualified with the venue's own TOWN, so a re-cluster never changes a wire key.
      val venues = cinemas.map { case (disp, pill, tid, kinoprogramm, town) =>
        val unique = if (claimedElsewhere.contains(disp)) s"$disp $town" else disp
        (new GermanCinema(unique, pill, Opening.generated(GermanRosterData.seasonal(disp), SeasonWindow.Germany)), tid, kinoprogramm)
      }
      (GermanPlace(slug, qualified, name, bundesland, lat, lon, multiTown, towns, venues.map(_._1)), venues)
    }

  val places: Seq[GermanPlace] = built.map(_._1)
  /** Venues by Bundesland, in roster order — the grouping `Cinema.byCity` reports
   *  Germany at (the uptime page), one level above the pages. */
  val byCity: Seq[(String, Seq[Cinema])] = Cinema.venuesByGroupInOrder(places)(_.bundesland)(_.cinemas)
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
