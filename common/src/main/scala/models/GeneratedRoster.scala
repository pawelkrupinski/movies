package models

import java.time.ZoneId

/** One addressable page of a generated roster — Germany's, the United States' or Spain's, all
 *  Poland's shape since 2026-10: a major city with only its own venues, a town with three or
 *  more, or a cluster of small towns a short drive apart named after the biggest ("Bayreuth und
 *  Umgebung", "Bend area", "Sitges y alrededores"). Which page holds which venue is data, built by
 *  each country's `data/<country>/scripts/build_pages.py` with the shared rule
 *  (`data/scripts/town_pages.py`); a US cluster may cross a state line.
 *
 *  Plain data rather than a [[City]]: the slug a page ends up addressable under depends on what
 *  every OTHER country has claimed, and `City` is the only place that sees all of them. It builds
 *  the [[RosterPage]]s from these.
 *
 *  @param preferredSlug the slug this page wants — its own folded name (`muenster`, `bend`), or
 *                       already qualified where another page of its country has or had the name
 *                       (`springfield-missouri`)
 *  @param qualifiedSlug the slug qualified with the page's region (`neustadt-hessen`,
 *                       `birmingham-alabama`, `toledo-castilla-la-mancha`) — what it answered at
 *                       while another country held [[preferredSlug]]
 *  @param name          the town the page is named after
 *  @param region        what the picker groups the page under: [[name]]'s Bundesland, state or
 *                       province
 *  @param zoneId        the page's clock. Per place, not per country: the US spans six zones and
 *                       a page keeps the one most of its venues keep; Spain's Canary Islands run
 *                       an hour behind the peninsula; Germany has one
 *  @param multiTown     several towns rather than one — the page is then "<name> <cluster suffix>"
 *  @param towns         the places a visitor searches the page by, most venues first, [[name]]
 *                       leading; see [[City.coveredPlaces]] */
final case class RosterPlace(
  preferredSlug: String,
  qualifiedSlug: String,
  name:          String,
  region:        String,
  lat:           Double,
  lon:           Double,
  zoneId:        ZoneId,
  multiTown:     Boolean,
  towns:         Seq[String],
  cinemas:       Seq[Cinema],
)

/** How a generated roster's cluster pages read in the country's language.
 *
 *  @param clusterSuffix      what follows a cluster's name: "und Umgebung", "area", "y alrededores" —
 *                            words that read right both alone and after the grammar's preposition
 *  @param clusterPreposition the locative a cluster takes instead of the grammar's, where the
 *                            language puts an article before it ("in the Bend area") */
final case class RosterStyle(clusterSuffix: String, clusterPreposition: Option[String] = None)

/** What the three generated rosters ([[GermanRoster]], [[UsRoster]], [[SpanishRoster]]) share. */
private[models] object GeneratedRoster {
  /** A roster venue's wire name. `displayName` is the key every per-cinema slot is stored under
   *  (`movie_slots`, `screenings`, the embedded `sourceData` map), and `Source.byDisplayName` is a
   *  plain `toMap` — so a venue reusing a name `claimed` by a country built before it would
   *  silently rebind the loser's stored showtimes to the winner. Such a venue takes `qualified`
   *  instead; every other keeps its name, so no stored slot's key moves. `SourceWireKeySpec` fails
   *  on any collision that slips through. */
  def wireName(claimed: Set[String], displayName: String, qualified: => String): String =
    if (claimed.contains(displayName)) qualified else displayName

  /** The roster's venues under each page's region, regions in the order their first page comes —
   *  the `Cinema.byCity` level one above the pages (the uptime page's). */
  def venuesByRegionInOrder(places: Seq[RosterPlace]): Seq[(String, Seq[Cinema])] = {
    val byRegion = places.groupBy(_.region)
    places.map(_.region).distinct.map(region => region -> byRegion(region).flatMap(_.cinemas))
  }
}
