package models

import java.time.ZoneId

/** One addressable Spanish page — Poland's shape since 2026-10: a major city (the
 *  43 municipalities of 150,000+) with only its own venues, a town with three or
 *  more, or a cluster of small towns a short drive apart named after the biggest
 *  ("Sitges y alrededores"). Which page holds which venue is data, built by
 *  `data/spain/scripts/build_pages.py` with the rule Poland's pages share
 *  (`data/scripts/town_pages.py`).
 *
 *  Deliberately plain data rather than a [[City]], for the reason [[UsPlace]]
 *  is: the slug a page ends up addressable under depends on what every OTHER
 *  country has already claimed, and `City` is the only place that can see all
 *  those lists at once. It builds the `SpanishPage` objects from these; see
 *  `City.spanishCities`. */
final case class SpanishPlace(
  /** The slug this page wants — its own folded name. `City` uses
   *  [[qualifiedSlug]] instead only on a collision. */
  preferredSlug: String,
  /** The slug qualified with the autonomous community (`toledo-castilla-la-mancha`)
   *  — what the page answers at where another country already serves
   *  [[preferredSlug]], the way a state qualifies a US page's. */
  qualifiedSlug: String,
  /** The town the page is named after. */
  name:          String,
  /** The province of [[name]]'s town — what the picker groups the page under. */
  province:      String,
  lat:           Double,
  lon:           Double,
  /** The page's own zone. A CONSTRUCTOR value rather than the constant
   *  `GermanPage` can afford, because Spain has TWO: the Canary Islands run an
   *  hour behind the peninsula on `Atlantic/Canary`, and a national default would
   *  move their "today" boundary — the same trap `UsPage` documents for six US
   *  zones. */
  zoneId:        ZoneId,
  /** Several towns rather than one — the page is then "<name> y alrededores". */
  multiTown:     Boolean,
  /** The page's towns (municipalities), most venues first, [[name]] leading — the
   *  place names a Spanish visitor searches by; see [[City.coveredPlaces]]. */
  towns:         Seq[String],
  cinemas:       Seq[Cinema],
)

/** Materialises the generated Spanish roster into [[SpanishPlace]]s +
 *  `SpanishCinema` venues, once. Exposes the places for `City.spanishCities`,
 *  the by-PROVINCE grouping for `Cinema.byCity` (the uptime page's level, as the
 *  US's is the state), and each cinema's SensaCine `theaterId` for the scrape
 *  catalog — the Spanish counterpart of [[GermanRoster]] and [[UsRoster]]. */
object SpanishRoster {
  /** Display names already spoken for, which a generated Spanish venue may NOT
   *  reuse.
   *
   *  The same hazard [[GermanRoster.claimedElsewhere]] guards, now looking at
   *  five countries instead of three: `displayName` is the wire key every
   *  per-cinema slot is stored under and `Source.byDisplayName` is a plain
   *  `toMap`, so a Spanish venue colliding with a Polish, UK, German **or US**
   *  one silently rebinds the loser's stored showtimes to the winner. The
   *  generator already refuses to emit a collision WITHIN Spain
   *  (`data/spain/scripts/generate_roster.py`); this catches the cross-country
   *  case, which the generator cannot see. `SourceWireKeySpec` fails on any that
   *  slips through.
   *
   *  Reads the rosters' `byCity` rather than their raw data, so the comparison is
   *  against the names actually built — the German and US venues are themselves
   *  qualified on collision, and it is the qualified name that ends up on the
   *  wire. */
  // Lazy, once per roster, as `GermanRoster.claimedElsewhere` is.
  private lazy val claimedElsewhere: Set[String] =
    Cinema.displayNamesClaimedBy(GermanRoster.byCity, UsRoster.byCity)

  private val built: Seq[(SpanishPlace, Seq[(SpanishCinema, Option[String], Option[String])])] =
    SpanishRosterData.pages.map { case (slug, qualified, name, province, lat, lon, zone, multiTown, towns, cinemas) =>
      // Qualify only the venues that would collide, so the roster's names — and the
      // wire keys of every already-stored Spanish slot — stay exactly as they are
      // otherwise. Qualified with the PROVINCE, as they were when a page was one, so
      // a re-cluster never changes a venue's wire key.
      val venues = cinemas.map { case (disp, pill, theaterId, ocineServer) =>
        val unique = if (claimedElsewhere.contains(disp)) s"$disp $province" else disp
        (new SpanishCinema(unique, pill, Opening.generated(SpanishRosterData.seasonal(disp), SeasonWindow.Spain)), theaterId, ocineServer)
      }
      (SpanishPlace(slug, qualified, name, province, lat, lon, TimeZones.named(zone), multiTown, towns,
                    venues.map(_._1)), venues)
    }

  val places: Seq[SpanishPlace] = built.map(_._1)
  /** Venues by province, provinces in roster order — the grouping `Cinema.byCity`
   *  reports Spain at (the uptime page), one level above the pages. */
  val byCity: Seq[(String, Seq[Cinema])] = Cinema.venuesByGroupInOrder(places)(_.province)(_.cinemas)

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
