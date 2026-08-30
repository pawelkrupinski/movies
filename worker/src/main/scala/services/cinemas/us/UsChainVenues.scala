package services.cinemas.us

/**
 * The US chain venues whose OWN site is the scrape primary, keyed by the roster
 * display name (`Cinema.displayName`) — the wire key every stored slot uses, and
 * the only stable handle a data-driven roster venue has: US cinemas are built at
 * runtime by `models.UsRoster` from `data/us/venues.json`, so there is no case
 * object to name here the way the UK chain maps do.
 *
 * Built from each chain's OWN published venue roster and then verified name by
 * name against ours. The captured rosters are checked in under `docs/venue-maps/`
 * next to the endpoint each was read from, so a future re-capture can be diffed
 * against what these maps claim rather than re-derived from memory.
 *
 * A venue absent from these maps stays on `flicks.us`. Three are, deliberately —
 * `Showcase Randolph`, `Landmark Esquire Theatre` and
 * `Landmark Plaza Frontenac Cinema` are in OUR roster but in neither chain's, so
 * there is no id to map them to and guessing one would 404 the venue into a
 * permanently red row. `CinemaScraperCatalogSpec` pins that all three stay on the
 * aggregator, and that every name here resolves to a real roster venue — a typo
 * would otherwise just silently leave its venue on Flicks.
 *
 * `CinemaScraperCatalog` reads these to build the chain-primary scrapers, and
 * derives each venue's flicks.us FALLBACK slug from `UsRoster.flicksSlugByCinema`
 * rather than restating it — primary and fallback can therefore never disagree
 * about which venue they mean.
 *
 * THE FOUR US MID-TIER CHAINS THAT ARE NOT HERE, and why (all probed 2026-08-30
 * with a browser User-Agent, at <=2 req/s, stopping on the first 403):
 *
 *   - Marcus Theatres (59 venues) — `www.marcustheatres.com` answers 403 to our
 *     egress at the edge, homepage included. Nothing to parse without a browser
 *     session, so it was not pursued.
 *   - B&B Theatres (51) — runs this SAME Webedia Gatsby platform, which would
 *     have made it the cheapest of the lot, but `www.bbtheatres.com` is serving
 *     404 for the site root and for the catalogue query alike. There is nothing
 *     to map venues against while the site is down; worth re-probing later,
 *     because if it returns it is a wiring change and not a client.
 *   - Harkins Theatres (31) — reachable (200), unlike the two above, but its
 *     showtimes source could not be confirmed (probed to a conclusion
 *     2026-08-30). `www.harkins.com` is a client-rendered Next.js SPA: the
 *     home/listing pages carry no embedded showtimes data (no `ld+json`, no
 *     hydration payload), and the apparent showtimes route
 *     `/theatres/<slug>/<date>` 404s even for today's own date — it is
 *     populated by a client-side fetch this worker never triggers. A real
 *     backend WAS found at `www.harkins.com/api/webservice/theatres` (200,
 *     JSON theatre roster with numeric ids), but it answers geo-filtered to a
 *     handful of nearby theatres with no path found to the other 29, and it
 *     carries no showtimes/movies-by-date field or sibling endpoint. Tracing
 *     the real showtimes call through the site's own JS bundle was the next
 *     step, but the shared chunk that would have named it 403'd on two
 *     independent probes (repeatable, not the single-request blip the other
 *     chunks showed) — the stop condition this reconnaissance is bound by.
 *     Un-located is not the same as measured-shorter, but the practical
 *     result is identical: there is no plain-HTTP path to Harkins' programme,
 *     so it stays on flicks.us like Marcus and Studio Movie Grill. Revisit
 *     only with a browser-rendering probe (outside this reconnaissance's
 *     plain-HTTP approach) to capture the live XHR the SPA makes.
 *   - Studio Movie Grill (18) — `www.studiomoviegrill.com` answers 403 the same
 *     way Marcus does.
 *
 * A chain also has to clear a HORIZON check before it can be listed here at all,
 * independent of whether its data is reachable: its own feed must advertise at
 * least as far ahead as flicks.us does for the same venues, or making it the
 * primary would let scrape-prune delete the advance-sale tail on every
 * successful scrape. See the measurements in `AlamoDrafthouseClient`.
 */
object UsChainVenues {

  /** One Alamo venue: the slug its schedule endpoint is keyed by, and the venue's
   *  own zone (only used to resolve "today" for the far-date sanity bound — the US
   *  spans six, so a worker in Europe must not decide it). */
  final case class AlamoVenue(slug: String, zoneId: String)

  /** One Webedia Gatsby venue: the platform's theater id, the venue's zone — which
   *  the schedule query takes VERBATIM and returns every session time in, so it is
   *  load-bearing rather than cosmetic — and the venue's own public page path,
   *  which is not derivable from the id (`/theaters/x0c11` alone 404s). */
  final case class WebediaVenue(theaterId: String, zoneId: String, venuePath: String)

  /** Alamo Drafthouse: display name -> its Alamo venue slug + the venue's own zone.
   *  40 venues, a bijection with the chain's own roster (captured 2026-08-30).
   *  See `docs/venue-maps/ALAMO-DRAFTHOUSE-VENUE-MAP.tsv`. */
  val alamoDrafthouse: Map[String, AlamoVenue] = Map(
    "Alamo Drafthouse Brooklyn" -> AlamoVenue("downtown-brooklyn", "America/New_York"),
    "Alamo Drafthouse Cedars Dallas" -> AlamoVenue("cedars", "America/Chicago"),
    "Alamo Drafthouse Charlottesville" -> AlamoVenue("charlottesville", "America/New_York"),
    "Alamo Drafthouse City Foundry" -> AlamoVenue("city-foundry", "America/Chicago"),
    "Alamo Drafthouse Corpus Christi" -> AlamoVenue("corpus-christi", "America/Chicago"),
    "Alamo Drafthouse Crystal City" -> AlamoVenue("crystal-city", "America/New_York"),
    "Alamo Drafthouse DC Bryant Street" -> AlamoVenue("dc-bryant-street", "America/New_York"),
    "Alamo Drafthouse Denton" -> AlamoVenue("denton", "America/Chicago"),
    "Alamo Drafthouse Downtown LA" -> AlamoVenue("downtown", "America/Los_Angeles"),
    "Alamo Drafthouse Indianapolis" -> AlamoVenue("indianapolis", "America/New_York"),
    "Alamo Drafthouse La Vista" -> AlamoVenue("la-vista", "America/Chicago"),
    "Alamo Drafthouse Lake Highlands" -> AlamoVenue("lake-highlands", "America/Chicago"),
    "Alamo Drafthouse Lakeline" -> AlamoVenue("lakeline", "America/Chicago"),
    "Alamo Drafthouse Laredo" -> AlamoVenue("laredo", "America/Chicago"),
    "Alamo Drafthouse Las Colinas" -> AlamoVenue("las-colinas", "America/Chicago"),
    "Alamo Drafthouse Littleton" -> AlamoVenue("littleton", "America/Denver"),
    "Alamo Drafthouse Lower Manhattan" -> AlamoVenue("lower-manhattan", "America/New_York"),
    "Alamo Drafthouse Mountain View" -> AlamoVenue("mountain-view", "America/Los_Angeles"),
    "Alamo Drafthouse Mueller" -> AlamoVenue("mueller", "America/Chicago"),
    "Alamo Drafthouse Naples" -> AlamoVenue("naples", "America/New_York"),
    "Alamo Drafthouse New Mission" -> AlamoVenue("new-mission", "America/Los_Angeles"),
    "Alamo Drafthouse One Loudoun" -> AlamoVenue("one-loudoun", "America/New_York"),
    "Alamo Drafthouse Park North" -> AlamoVenue("park-north", "America/Chicago"),
    "Alamo Drafthouse Raleigh" -> AlamoVenue("raleigh", "America/New_York"),
    "Alamo Drafthouse Richardson" -> AlamoVenue("richardson", "America/Chicago"),
    "Alamo Drafthouse Seaport Boston" -> AlamoVenue("seaport", "America/New_York"),
    "Alamo Drafthouse Slaughter Lane" -> AlamoVenue("slaughter-lane", "America/Chicago"),
    "Alamo Drafthouse Sloans Lake" -> AlamoVenue("sloans-lake", "America/Denver"),
    "Alamo Drafthouse South Lamar" -> AlamoVenue("south-lamar", "America/Chicago"),
    "Alamo Drafthouse Springfield" -> AlamoVenue("springfield", "America/Chicago"),
    "Alamo Drafthouse Staten Island" -> AlamoVenue("staten-island", "America/New_York"),
    "Alamo Drafthouse Stone Oak" -> AlamoVenue("stone-oak", "America/Chicago"),
    "Alamo Drafthouse Valley Fair" -> AlamoVenue("valley-fair", "America/Los_Angeles"),
    "Alamo Drafthouse Village Austin" -> AlamoVenue("village", "America/Chicago"),
    "Alamo Drafthouse Westminster" -> AlamoVenue("westminster", "America/Denver"),
    "Alamo Drafthouse Winchester" -> AlamoVenue("winchester", "America/New_York"),
    "Alamo Drafthouse Woodbridge" -> AlamoVenue("woodbridge", "America/New_York"),
    "Alamo Drafthouse Woodbury" -> AlamoVenue("woodbury", "America/Chicago"),
    "Alamo Drafthouse Wrigleyville" -> AlamoVenue("wrigleyville", "America/Chicago"),
    "Alamo Drafthouse Yonkers" -> AlamoVenue("yonkers", "America/New_York"),
  )

  /** Showcase Cinemas US: display name -> its Webedia theater id, zone and venue
   *  path. 13 venues — the chain's whole US roster.
   *
   *  NOTE the two venues whose roster name carries no "Showcase" prefix
   *  (Blackstone Valley 14, Island 16): they are Showcase-operated and appear in
   *  the chain's own roster, so they are mapped like the rest. Selecting this
   *  chain by display-name PREFIX would have missed both.
   *
   *  `Showcase Randolph` is deliberately absent — it is in our roster but not in
   *  the chain's, so it keeps flicks.us as its primary. */
  val showcaseUs: Map[String, WebediaVenue] = Map(
    "Blackstone Valley 14 Millbury" -> WebediaVenue("X06PR", "America/New_York", "/theaters/x06pr-blackstone-valley-14-cinema-de-lux"),
    "Island 16 Holtsville" -> WebediaVenue("X06DU", "America/New_York", "/theaters/x06du-island-16-cinema-de-lux"),
    "Showcase Chestnut Hill" -> WebediaVenue("X0PTT", "America/New_York", "/theaters/x0ptt-showcase-superlux-chestnut-hill"),
    "Showcase Cross County" -> WebediaVenue("X000P", "America/New_York", "/theaters/x000p-showcase-cinema-de-lux-cross-county"),
    "Showcase Farmingdale" -> WebediaVenue("X00UF", "America/New_York", "/theaters/x00uf-showcase-cinema-de-lux-farmingdale"),
    "Showcase Hanover Crossing" -> WebediaVenue("G019K", "America/New_York", "/theaters/g019k-showcase-cinema-de-lux-hanover-crossing"),
    "Showcase Legacy Place Dedham" -> WebediaVenue("X0C11", "America/New_York", "/theaters/x0c11-showcase-cinema-de-lux-legacy-place"),
    "Showcase Lowell" -> WebediaVenue("X02XX", "America/New_York", "/theaters/x02xx-showcase-cinema-de-lux-lowell"),
    "Showcase North Attleboro" -> WebediaVenue("X02RA", "America/New_York", "/theaters/x02ra-showcase-cinema-de-lux-north-attleboro"),
    "Showcase Ridge Hill Yonkers" -> WebediaVenue("X0L42", "America/New_York", "/theaters/x0l42-showcase-cinema-de-lux-ridge-hill"),
    "Showcase Springdale" -> WebediaVenue("X0343", "America/New_York", "/theaters/x0343-showcase-cinema-de-lux-springdale"),
    "Showcase Warwick" -> WebediaVenue("X02NZ", "America/New_York", "/theaters/x02nz-showcase-cinemas-warwick-quaker-lane"),
    "Showcase Woburn" -> WebediaVenue("X01AF", "America/New_York", "/theaters/x01af-showcase-cinema-de-lux-woburn"),
  )

  /** Landmark Theatres: display name -> its Webedia theater id, zone and venue
   *  path. 26 venues — the chain's whole roster. Landmark spans five zones
   *  (including `America/Phoenix` and `America/Indiana/Indianapolis`), which is
   *  why the zone is carried per venue rather than per country: the schedule query
   *  takes it verbatim and every session time is returned in it.
   *
   *  `Landmark Esquire Theatre` and `Landmark Plaza Frontenac Cinema` are
   *  deliberately absent — in our roster, not in the chain's, so both keep
   *  flicks.us as their primary. */
  val landmark: Map[String, WebediaVenue] = Map(
    "Landmark Aquarius" -> WebediaVenue("X00TM", "America/Los_Angeles", "/theaters/x00tm-landmark-aquarius-theatre-palo-alto"),
    "Landmark Atlantic Plumbing Cinema" -> WebediaVenue("X0WLT", "America/New_York", "/theaters/x0wlt-landmark-atlantic-plumbing-cinema"),
    "Landmark Bethesda Row Cinema" -> WebediaVenue("X06C1", "America/New_York", "/theaters/x06c1-landmark-bethesda-row-cinema"),
    "Landmark Century Centre Chicago" -> WebediaVenue("X05IO", "America/Chicago", "/theaters/x05io-landmark-century-centre-cinema-chicago"),
    "Landmark Closter Plaza" -> WebediaVenue("G01AA", "America/New_York", "/theaters/g01aa-landmark-closter-plaza"),
    "Landmark Crest Cinema Center" -> WebediaVenue("X00MT", "America/Los_Angeles", "/theaters/x00mt-landmark-crest-cinema-center-shoreline"),
    "Landmark Del Mar Theatre" -> WebediaVenue("X00QV", "America/Los_Angeles", "/theaters/x00qv-landmark-del-mar-theatre-santa-cruz"),
    "Landmark Glendale 12" -> WebediaVenue("X0KAO", "America/Indiana/Indianapolis", "/theaters/x0kao-landmark-glendale-12-indianapolis"),
    "Landmark Greenwood Village" -> WebediaVenue("X0873", "America/Denver", "/theaters/x0873-the-landmark-greenwood-village"),
    "Landmark Inwood Theatre" -> WebediaVenue("X02KC", "America/Chicago", "/theaters/x02kc-landmark-inwood-theatre-dallas"),
    "Landmark Kendall Square" -> WebediaVenue("X019B", "America/New_York", "/theaters/x019b-landmark-kendall-square-cinema-cambridge"),
    "Landmark Keystone Art Cinema" -> WebediaVenue("X07M6", "America/Indiana/Indianapolis", "/theaters/x07m6-landmark-keystone-art-cinema-indianapolis"),
    "Landmark Lagoon Cinema" -> WebediaVenue("X01QW", "America/Chicago", "/theaters/x01qw-landmark-lagoon-cinema-minneapolis"),
    "Landmark Mayan Theatre Denver" -> WebediaVenue("X02AK", "America/Denver", "/theaters/x02ak-landmark-mayan-theatre-denver"),
    "Landmark Midtown Art Atlanta" -> WebediaVenue("X00QM", "America/New_York", "/theaters/x00qm-landmark-midtown-art-cinema-atlanta"),
    "Landmark Nuart Theatre" -> WebediaVenue("X00CW", "America/Los_Angeles", "/theaters/x00cw-landmark-nuart-theatre-west-los-angeles"),
    "Landmark Opera Plaza Cinema" -> WebediaVenue("X00U8", "America/Los_Angeles", "/theaters/x00u8-landmark-opera-plaza-cinema-san-francisco"),
    "Landmark Pasadena (formerly Laemmle)" -> WebediaVenue("G01CH", "America/Los_Angeles", "/theaters/g01ch-landmark-theatres-pasadena"),
    "Landmark Piedmont" -> WebediaVenue("X00Y7", "America/Los_Angeles", "/theaters/x00y7-landmark-piedmont-theatre-oakland"),
    "Landmark Ritz Five" -> WebediaVenue("X081D", "America/New_York", "/theaters/x081d-landmark-ritz-five-philadelphia"),
    "Landmark Scottsdale Quarter Theatre" -> WebediaVenue("X03C1", "America/Phoenix", "/theaters/x03c1-landmark-scottsdale-quarter-theatre"),
    "Landmark Theaters Sunset" -> WebediaVenue("G01SI", "America/Los_Angeles", "/theaters/g01si-landmark-theatres-sunset"),
    "Landmark at Annapolis Harbour" -> WebediaVenue("G019L", "America/New_York", "/theaters/g019l-landmark-at-annapolis-harbour-center"),
    "Landmark at The Glen" -> WebediaVenue("X0JYT", "America/Chicago", "/theaters/x0jyt-landmark-at-the-glen-glenview"),
    "The Landmark Westwood" -> WebediaVenue("X00D9", "America/Los_Angeles", "/theaters/x00d9-landmark-westwood-los-angeles"),
    "The Landmark at Merrick Park" -> WebediaVenue("X0XDF", "America/New_York", "/theaters/x0xdf-the-landmark-at-merrick-park-coral-gables"),
  )

  /** Every display name served by a US chain primary — what the catalog swaps out
   *  of the Flicks-by-default US wiring, and what the cadence guard subtracts when
   *  sizing the remaining flicks.us sweep. */
  val all: Set[String] = alamoDrafthouse.keySet ++ showcaseUs.keySet ++ landmark.keySet
}
