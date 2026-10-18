package models

import java.time.ZoneId

/** Materialises the generated US roster into [[RosterPlace]]s + `UsCinema` venues,
 *  once. Exposes the places for `City.usCities`, the by-STATE grouping for
 *  `Cinema.byCity` (the uptime page's level, as Germany's is the Bundesland), each
 *  cinema's Flicks slug for the scrape catalog, and the venues on their own clock
 *  — the US counterpart of [[GermanRoster]]. */
object UsRoster {
  /** Display names already spoken for — Poland's, the UK's and Germany's — which a US venue may
   *  not reuse ([[GeneratedRoster.wireName]]). The generator already de-duplicates WITHIN the US
   *  roster (it qualifies a repeated title with its town, then its state, and refuses to emit an
   *  unresolved collision); this catches the cross-country case, which the generator cannot see.
   *  Reads `GermanRoster.byCity` rather than `GermanRosterData`, so the comparison is against the
   *  names actually built: it is a German venue's qualified name that ends up on the wire. */
  // Lazy, once per roster, as `GermanRoster.claimedElsewhere` is.
  private lazy val claimedElsewhere: Set[String] = Cinema.displayNamesClaimedBy(GermanRoster.byCity)

  /** One venue of the generated roster, with everything the places and the
   *  scrape catalog read off it. */
  private final case class Venue(cinema: UsCinema, flicksSlug: String, state: String, town: String,
                                zone: Option[ZoneId])

  /** The venues whose own clock is not their page's, by roster name (see `generate_roster.py`). */
  private val ownZones: Map[String, ZoneId] =
    UsRosterData.venueZones.map { case (disp, zone) => disp -> TimeZones.named(zone) }.toMap

  /** "Bend area", "in the Bend area" — an area takes the article a town does not. */
  val style: RosterStyle = RosterStyle("area", clusterPreposition = Some("in the"))

  private val built: Seq[(RosterPlace, Seq[Venue])] =
    UsRosterData.pages.map { case (slug, qualified, name, state, lat, lon, zone, multiTown, towns, cinemas) =>
      // Qualified with the venue's own STATE, so a re-cluster never moves a wire key.
      val venues = cinemas.map { case (disp, pill, flicksSlug, venueState, town) =>
        val opening = Opening.generated(UsRosterData.seasonal(disp), SeasonWindow.UnitedStates)
        Venue(new UsCinema(GeneratedRoster.wireName(claimedElsewhere, disp, s"$disp ($venueState)"), pill, opening),
              flicksSlug, venueState, town, ownZones.get(disp))
      }
      // Its towns, then the towns its venues give as their own — New York's boroughs, Los Angeles' Hollywood.
      val places = (name +: (towns ++ TownRanking.ranked(venues.map(_.town)))).distinct
      (RosterPlace(slug, qualified, name, state, lat, lon, TimeZones.named(zone), multiTown, places,
                   venues.map(_.cinema)), venues)
    }

  private val venues: Seq[Venue] = built.flatMap(_._2)

  /** Every US page, major cities first (biggest first), then the rest by slug. */
  val places: Seq[RosterPlace] = built.map(_._1)

  /** Every US venue under its STATE's name — one section per state on the
   *  uptime page, and the US half of `Cinema.byCity`. Not per page: 1,995
   *  sections of two cinemas each is not a page anybody reads, and a page may
   *  cross a state line where a venue never does. */
  val byCity: Seq[(String, Seq[Cinema])] =
    venues.groupBy(_.state).toSeq.sortBy(_._1).map { case (state, vs) => state -> vs.map(v => v.cinema: Cinema) }

  /** Each US venue's Flicks `/cinema/<slug>/` id — what the scrape catalog binds
   *  to a `FlicksClient` on the `FlicksMarket.UnitedStates` market, and what a
   *  chain-primary venue keeps as its FALLBACK slug. */
  val flicksSlugByCinema: Map[Cinema, String] = venues.map(v => (v.cinema: Cinema) -> v.flicksSlug).toMap

  /** US venues by display name. The display name is the wire key every stored slot
   *  uses and the only stable handle these venues have — they are built at runtime
   *  from the generated `UsRosterData`, so unlike the Polish and UK rosters there is no
   *  case object to refer to one by. `services.cinemas.us.UsChainVenues` keys its
   *  chain maps this way for exactly that reason; this is the lookup back. */
  val byDisplayName: Map[String, Cinema] = venues.map(v => v.cinema.displayName -> (v.cinema: Cinema)).toMap

  /** The venues on a clock that is not their page's: a page's zone is its venues'
   *  majority, and these sit across a zone line from the rest (Green River Cinema in
   *  Eastern-time Campbellsville, on a page whose other towns keep Central).
   *  [[VenueClock]] reads each one's day and wall-clock time in its own zone. */
  val venueZones: Map[Cinema, ZoneId] =
    venues.flatMap(v => v.zone.map((v.cinema: Cinema) -> _)).toMap
}
