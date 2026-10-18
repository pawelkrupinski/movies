package models

import java.time.ZoneId

/** One US page, as the roster sees it: a major city (a place of 100,000+) with
 *  only its own venues, a town with three or more, or a cluster of small towns a
 *  short drive apart named after the biggest ("Bend area"). Which page holds which
 *  venue is data, built by `data/us/scripts/build_pages.py` with the shared rule
 *  (`data/scripts/town_pages.py`); a cluster may cross a state line.
 *
 *  Plain data rather than a [[City]], for the reason [[GermanPlace]] is: the slug
 *  a page ends up addressable under depends on what every OTHER country has
 *  claimed ("Birmingham" is a UK city), and `City` is the only place that sees
 *  all of them. See `City.usCities`. */
final case class UsPlace(
  /** The slug this page wants — its own name (`bend`), or already qualified with
   *  its state where another US page has or had the name (`springfield-missouri`). */
  preferredSlug: String,
  /** The slug qualified with the state (`birmingham-alabama`) — what the page
   *  answers at where another country already serves [[preferredSlug]]. */
  qualifiedSlug: String,
  /** The town the page is named after. */
  name:          String,
  /** The state of [[name]]'s town — what the picker groups the page under. */
  stateName:     String,
  lat:           Double,
  lon:           Double,
  /** The clock most of the page's venues keep; see `generate_roster.zone_for`. */
  zoneId:        ZoneId,
  /** Several towns rather than one — the page is then "<name> area". */
  multiTown:     Boolean,
  /** The places a visitor searches the page by, most venues first, [[name]]
   *  leading: its towns, then the towns its venues give as their own — New
   *  York's boroughs, Los Angeles' Hollywood. See [[City.coveredPlaces]]. */
  towns:         Seq[String],
  cinemas:       Seq[Cinema],
)

/** Materialises the generated US roster into [[UsPlace]]s + `UsCinema` venues,
 *  once. Exposes the places for `City.usCities`, the by-STATE grouping for
 *  `Cinema.byCity` (the uptime page's level, as Germany's is the Bundesland), each
 *  cinema's Flicks slug for the scrape catalog, and the venues on their own clock
 *  — the US counterpart of [[GermanRoster]]. */
object UsRoster {
  /** Display names already spoken for, which a generated US venue may NOT reuse.
   *
   *  Same hazard [[GermanRoster.claimedElsewhere]] guards, and it now has to look
   *  wider: `displayName` is the wire key every per-cinema slot is stored under and
   *  `Source.byDisplayName` is a plain `toMap`, so a US venue colliding with a
   *  Polish, UK **or German** one silently rebinds the loser's stored showtimes to
   *  the winner. The generator already de-duplicates WITHIN the US roster (it
   *  qualifies a repeated title with its town, then its state, and refuses to emit
   *  an unresolved collision); this catches the cross-country case, which the
   *  generator cannot see. `SourceWireKeySpec` fails on any that slips through.
   *
   *  Reads `GermanRoster.byCity` rather than `GermanRosterData` so the comparison is
   *  against the names actually built — German venues are themselves qualified on
   *  collision, and it is the qualified name that ends up on the wire. */
  // Lazy, once per roster, as `GermanRoster.claimedElsewhere` is.
  private lazy val claimedElsewhere: Set[String] = Cinema.displayNamesClaimedBy(GermanRoster.byCity)

  /** One venue of the generated roster, with everything the places and the
   *  scrape catalog read off it. */
  private final case class Venue(cinema: UsCinema, flicksSlug: String, state: String, town: String,
                                zone: Option[ZoneId])

  /** The venues whose own clock is not their page's, by roster name (see `generate_roster.py`). */
  private val ownZones: Map[String, ZoneId] =
    UsRosterData.venueZones.map { case (disp, zone) => disp -> TimeZones.named(zone) }.toMap

  private val built: Seq[(UsPlace, Seq[Venue])] =
    UsRosterData.pages.map { case (slug, qualified, name, state, lat, lon, zone, multiTown, towns, cinemas) =>
      // Qualify only the venues that would collide, so the roster's names — and
      // the wire keys of every already-stored US slot — stay exactly as they are.
      // Qualified with the venue's own STATE, so a re-cluster never moves a wire key.
      val venues = cinemas.map { case (disp, pill, flicksSlug, venueState, town) =>
        val unique = if (claimedElsewhere.contains(disp)) s"$disp ($venueState)" else disp
        Venue(new UsCinema(unique, pill, Opening.generated(UsRosterData.seasonal(disp), SeasonWindow.UnitedStates)), flicksSlug, venueState, town, ownZones.get(disp))
      }
      val places = (name +: (towns ++ TownRanking.ranked(venues.map(_.town)))).distinct
      (UsPlace(slug, qualified, name, state, lat, lon, TimeZones.named(zone), multiTown, places,
               venues.map(_.cinema)), venues)
    }

  private val venues: Seq[Venue] = built.flatMap(_._2)

  /** Every US page, major cities first (biggest first), then the rest by slug. */
  val places: Seq[UsPlace] = built.map(_._1)

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
