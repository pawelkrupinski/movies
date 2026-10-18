package models

import java.util.Locale


/**
 * A country of cinema repertoire — the scope ABOVE [[City]]. Until now Poland
 * was implicit everywhere (PL scraper clients, `Europe/Warsaw`, Polish grammar,
 * `<html lang="pl">`, the `kinowo` database). `Country` makes that dimension
 * explicit so a second country can run without colliding with Poland:
 *
 *   - each country owns its own set of [[City]] objects ([[cities]]),
 *   - each maps to its OWN Mongo database ([[mongoDb]]) on the shared cluster,
 *   - each carries its UI [[language]] (for collation + i18n),
 *   - each says how its city list is GROUPED in a picker ([[cityGroups]] — a US
 *     town is found through its state, a German town through its Bundesland,
 *     a UK place through its county and its nation), and
 *   - each decides whether the Filmweb rating/fallback path applies
 *     ([[filmwebEnabled]] — a new country won't use Filmweb at all).
 *
 * A WEB deployment serves exactly ONE country, picked once at boot from
 * `KINOWO_COUNTRY` (resolved by `settings.ProcessConfiguration`); the WORKER may instantiate its object graph
 * once per country and iterate [[all]]. Nothing reads a global "current
 * country" — the resolved `Country` is passed down from the composition root.
 */
sealed abstract class Country(
  val code:           String,          // ISO-ish short code, also the URL-free identifier: "pl", "uk"
  val displayName:    String,          // human label for the country switcher (native/English name)
  val language:       Locale,          // UI language + collation locale
  val mongoDb:        String,          // database name on the shared cluster
  val filmwebEnabled: Boolean,         // is the Filmweb rating/fallback path wired for this country?
  val webOrigin:      Option[String],  // public ORIGIN of this country's deployment (scheme+host, no path, no trailing slash); None = not deployed yet
  val pathPrefix:     String,          // where the deployment is MOUNTED on that origin: "" on its own domain, "/uk" when it shares one
  val brandName:      String,          // customer-facing app name: "Kinowo" in PL, "Showtimes" elsewhere (the Polish coinage means nothing abroad)
) {
  /** The cities this country serves. Authoritative per-country list; [[City.all]]
   *  is the union across every country. */
  def cities: Seq[City]

  /** Public BASE URL of this country's site: origin + [[pathPrefix]], no
   *  trailing slash. `https://kinowo.net` for the country that owns a domain,
   *  `https://showtimes.cc/uk` for one that shares the brand domain with its
   *  siblings.
   *
   *  THE ONE THING TO APPEND A PATH TO. Everything that links AT a country —
   *  the navbar's country switcher, the front-door picker, share links, the
   *  Open Graph origin, the `/api/catalog` payload both mobile apps build every
   *  request on — reads this, so a country moving host or mount point moves all
   *  of them together. */
  def webUrl: Option[String] = webOrigin.map(_ + pathPrefix)

  /** Where THIS deployment's router is mounted (`play.http.context`): `"/"` for
   *  a country on its own domain, `"/uk/"` for one under a path prefix.
   *
   *  Trailing slash on purpose. Play's generated router matches the bare `/`
   *  route as the prefix verbatim (`PathPattern(List(StaticPart(prefix)))`), so
   *  a prefix of `"/uk"` would put the landing at `/uk` and every other page at
   *  `/uk/…`; with `"/uk/"` the landing is `/uk/` and the reverse routes emit
   *  the same shape a country at the root does, one segment deeper. */
  def mountPath: String = if (pathPrefix.isEmpty) "/" else s"$pathPrefix/"

  /** Does THIS deployment answer the brand FRONT DOOR for `host`?
   *
   *  Two conditions, and the second is the one the subdomain era did not need:
   *  the host has to be the bare apex ([[DeploymentOrigins.isApexHost]]), AND this
   *  deployment has to be mounted at the root. Since the Showtimes countries
   *  moved under `showtimes.cc/{code}/`, the apex is ALSO the host every one of
   *  their own pages arrives on — a host check alone would replace the UK
   *  site's homepage with a country picker. The country mounted at `/` is the
   *  only one whose `/` is not already a country's landing. */
  def servesApex(host: String): Boolean = pathPrefix.isEmpty && DeploymentOrigins.isApexHost(host)

  /** How the picker at `/` ARRANGES [[cities]]: empty for a flat list, and
   *  otherwise one group per US state, per German Bundesland, Polish voivodeship
   *  or Spanish province, or per UK nation over its counties. "Los Angeles" is
   *  found under "California", and none of 1,995 US pages, 436 German pages or
   *  264 UK pages is a list anybody reads straight through.
   *
   *  Depth is the country's own: Poland, Germany, the US and Spain nest one level,
   *  the UK two (nation, then county).
   *  Where it is non-empty the groups PARTITION [[cities]] — through
   *  [[CityGroup.allCities]], since a nesting country's top level holds no
   *  cities directly. `CountrySpec` holds that. */
  def cityGroups: Seq[CityGroup] = Nil

  /** [[cityGroups]] FLATTENED to one level and re-sorted — what a control that
   *  cannot nest needs. The navbar's `<select>` is the caller: an `<optgroup>`
   *  may not contain another, so a UK place appears under its NATION there, with
   *  the county it sits under on the page contributing nothing but its members.
   *
   *  Re-sorted HERE, not taken from [[CityGroup.allCities]] in tree order. The
   *  tree's order is the PAGE's: a county that kept its heading contributes its
   *  places at the county's own alphabetical position, which reads correctly
   *  under a "Glamorgan" heading and not at all without one — Cardiff would
   *  come after Dyfed in a flat list, where native type-ahead on "c" never
   *  reaches it.
   *
   *  Empty for a flat country, whose `<select>` takes the ungrouped branch. */
  def optionGroups: Seq[(String, Seq[City])] =
    cityGroups.map(group => group.label -> CityListing.sorted(group.allCities, language))

  /** The two [[Showtime.format]] tokens THIS country's sources mark a subtitled
   *  and a dubbed screening with, or `None` where nothing marks either.
   *
   *  The Filtry panel's "version" radios filter on a literal token, so the pair
   *  has to be the one the country's own scrapers emit: `NAP`/`DUB` in Poland
   *  (from `FormatTags`), `OmU`/`DF` and `VOSE`/`DOB` in the Webedia markets
   *  (from `WebediaMarket`, whose spec pins the two lists together), `SUB`/`DUB`
   *  in the English-speaking ones. `None` leaves the row out entirely rather than
   *  offering a filter that can only ever match nothing, which is what Germany
   *  and Spain were shipped with; no country is currently in that position, but
   *  a country whose every screening is in one language would be. */
  def versionTokens: Option[VersionTokens] = None

  /** How this country's visitors read a showtime's clock — see [[ClockStyle]]. */
  def clockStyle: ClockStyle = ClockStyle.TwentyFourHour

  /** The token a VOICE-OVER screening carries — one narrator read over the
   *  original soundtrack, which is neither dubbing nor subtitles and is a version
   *  of its own wherever it is offered — or `None` where the country's cinemas
   *  offer no such version. Poland writes it `LEK`; the English-speaking
   *  deployments `LEC`; Germany and Spain have nothing to write, their sources
   *  naming only original/subtitled/dubbed prints (`WebediaMarket`).
   *
   *  ABSTRACT, not defaulted: a default is Poland's, and Germany and Spain
   *  inherited `LEK` for months without any spec noticing. A new country has to
   *  say what it uses, even when the answer is nothing.
   *
   *  Its own member rather than a third field on [[VersionTokens]] because that
   *  pair is what the Filtry radios FILTER on, and a voice-over screening is not
   *  one of the two choices those radios offer. */
  def voiceoverToken: Option[String]

  /** How this country's venues bill facts inside a title, read off it at intake into the listing's own fields
   *  ([[services.movies.TitleFacts]]): the languages of the credit and label words they write — their own and English —
   *  and the age-rating system they bill, if they bill one there. ABSTRACT, as [[voiceoverToken]] is: a bare "(15)" is a
   *  certificate in the UK and nothing anywhere else. */
  def titleFacts: services.movies.TitleFacts.Rules

  /** The one zone this country is published under: its BIGGEST city's, ties by slug — what a
   *  country-wide clock (the catalogue's fallback `timezone`, /uptime's timestamps) reads.
   *
   *  Four of the five keep one zone throughout, so for them any city answers. The US spans
   *  six, and reading `cities.head` made a live value a function of roster ORDER — a
   *  generator change that reshuffled the states once moved it from Chicago to Pago Pago
   *  and nothing failed. Biggest does not move when the roster is re-sorted, and it is the
   *  answer most of the country's users are on. `Europe/Warsaw` for the — currently
   *  impossible — city-less country. */
  lazy val zone: java.time.ZoneId =
    cities.maxByOption(city => (city.cinemas.size, city.slug)).map(_.zoneId).getOrElse(TimeZones.Poland)

  /** This country's cities by slug. EVERY slug lookup goes through a country:
   *  each country owns its own slugs, so `york` is York on `/uk/` and York,
   *  Pennsylvania on `/us/`, and there is no global namespace to search. */
  lazy val bySlug: Map[String, City] = cities.map(c => c.slug -> c).toMap

  /** Slugs a city of this country USED to answer at → the one it answers at now:
   *  its retired pages, each pointing at the page holding the town it was named
   *  after (else most of its venues), and the slugs its pages were qualified to
   *  while another country held their names.
   *
   *  A city slug is published — the sitemap, whatever a search engine indexed,
   *  the `city` cookie of everyone who has visited, the share card named after
   *  it — so renaming one is a redirect, not an edit (`RenamedCityRedirectFilter`),
   *  and an entry stays FOREVER. ONE successor each. */
  def renamedSlugs: Map[String, String]

  /** The slugs a city of this country used to answer at, given the one it answers
   *  at now. The read model is PROJECTED under the slug — a `CityScreening`'s
   *  `_id` is `filmId|city|cinema` — so the instant a slug changes, the rows
   *  already projected are filed under a name nothing asks for until each film is
   *  projected again. `WebReadModel` reads these alongside the current one. */
  def formerSlugs(current: String): Seq[String] = formerSlugsByCurrent.getOrElse(current, Nil)

  private lazy val formerSlugsByCurrent: Map[String, Seq[String]] =
    renamedSlugs.toSeq.groupMap(_._2)(_._1).view.mapValues(_.sorted).toMap

  /** Every other slug a place of this country is known by → the page it 301s to:
   *  a town a page covers that is not a page itself (`/uk/ilkley/`), and a page's
   *  qualified slug (`/uk/york-north-yorkshire/`). Generated from the pages'
   *  data (`data/scripts/generate_slug_index.py`); never a page's or a renamed
   *  slug, and not a former slug the read model reads — nothing was ever filed
   *  under one. */
  def slugAliases: Map[String, String]

  /** Where a slug that is not a page of this country goes instead: the page it
   *  was renamed to, else the page it is an alias of — `None` for one this country
   *  does not know, which is the only kind that 404s. */
  def redirectFor(slug: String): Option[String] = redirect(slug).map(_.to)

  /** [[redirectFor]]'s page with the kind of old address the slug is — what the
   *  web's legacy-address usage metric is labelled by — in one lookup. */
  def redirect(slug: String): Option[SlugRedirect] =
    if (bySlug.contains(slug)) None
    else renamedSlugs.get(slug).map { to =>
      SlugRedirect(to, if (CitySlugSuccession.FormerlyQualified.of(this).contains(slug)) SlugRedirectKind.FormerlyQualified
                       else SlugRedirectKind.Retired)
    }.orElse(slugAliases.get(slug).map(SlugRedirect(_, SlugRedirectKind.Alias)))

  /** The page a slug means here, live or by redirect. */
  def resolve(slug: String): Option[City] = bySlug.get(slug).orElse(redirectFor(slug).flatMap(bySlug.get))

  /** [[cities]] ordered alphabetically by display name under this country's
   *  language collation — the list every UI city picker in this deployment
   *  iterates. */
  lazy val allSorted: Seq[City] = CityListing.sorted(cities, language)

  /** Compact JSON array of this country's cities for the client (`ALL_CITIES`,
   *  the geolocation/nearest-city picker). */
  def allJson: String = CityListing.json(cities)

  /** Public origin (scheme + host, no trailing slash) for this country's share /
   *  Open Graph links — the host Facebook scrapes and caches a preview against.
   *  A deployed country carries its own [[webUrl]]; a modelled-but-not-yet-deployed
   *  country falls back to the default country's host so a link still resolves
   *  rather than dangling. (All modelled countries are currently deployed.) */
  def ogOrigin: String = webUrl.getOrElse(Country.default.webUrl.get)

  /** This country's public base with no scheme -- `kinowo.net`, `showtimes.cc/uk`.
   *  For the places that DISPLAY the domain rather than link to it: the footer
   *  drawn into every Open Graph card. Derived from [[ogOrigin]] so a domain move
   *  updates the rendered cards with everything else, which the literal it
   *  replaced did not — every UK share card said `kinowo.fly.dev`. */
  def shareHost: String = DeploymentOrigins.withoutScheme(ogOrigin)

  /** Filename (under `assets/img/`) of the `/` landing's share-preview montage
   *  for this country. The default country keeps the original, unsuffixed
   *  `og-home.jpg`; every other country gets a per-code card so a UK deployment's
   *  preview shows English posters, a German one German — the same asset path is
   *  served by every deployment (one build), so all these files are checked in. */
  def homeOgImage: String = if (this == Country.default) "og-home.jpg" else s"og-home-$code.jpg"
}

/** A country's subtitled/dubbed [[Showtime.format]] tokens — see
 *  [[Country.versionTokens]]. */
case class VersionTokens(subtitled: String, dubbed: String)

object Country {

  case object Poland extends Country(
    code           = "pl",
    displayName    = "Polska",
    language       = Locale.forLanguageTag("pl-PL"),
    // Poland keeps the original database name so the existing prod deployment is
    // byte-identical — do NOT rename this to `kinowo_pl`.
    mongoDb        = "kinowo",
    filmwebEnabled = true,
    // Poland owns its domain outright, so it is mounted at the root and its URLs
    // are byte-identical to the ones it has always served.
    webOrigin      = Some("https://kinowo.net"),
    pathPrefix     = "",
    brandName      = "Kinowo",
  ) {
    val cities: Seq[City] = City.polishCities
    lazy val renamedSlugs: Map[String, String] = PolishPages.retired
    lazy val slugAliases: Map[String, String] = CitySlugIndexData.plAliases
    /** One group per voivodeship — see `CityPickerGroups.polishVoivodeships`. */
    override lazy val cityGroups: Seq[CityGroup] = CityPickerGroups.polishVoivodeships
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("NAP", "DUB"))
    val voiceoverToken: Option[String] = Some("LEK")
    val titleFacts: services.movies.TitleFacts.Rules = services.movies.TitleFacts.Rules(Set("pl", "en"), Some(services.movies.TitleFacts.RatingSystem.PolishAge))
  }

  /** The United Kingdom — an English-language country on its own `kinowo_uk`
   *  database that does not use Filmweb, sourced from the Flicks listings plus
   *  the Cineworld/Vue/Odeon/Everyman/Showcase chain clients. */
  case object UnitedKingdom extends Country(
    code           = "uk",
    displayName    = "United Kingdom",
    language       = Locale.forLanguageTag("en-GB"),
    mongoDb        = "kinowo_uk",
    filmwebEnabled = false,
    webOrigin      = Some("https://showtimes.cc"),
    pathPrefix     = "/uk",
    brandName      = "Showtimes",
  ) {
    val cities: Seq[City] = City.ukCities
    lazy val renamedSlugs: Map[String, String] = CitySlugSuccession.ukSuccession
    lazy val slugAliases: Map[String, String] = CitySlugIndexData.ukAliases
    /** The four nations plus the Crown Dependencies, each over the counties and
     *  pages inside it — 264 pages is not an A-to-Z anybody reads. */
    override val cityGroups: Seq[CityGroup] = CityPickerGroups.ukNations
    // Britain subtitles rather than dubs: `SUB` (captions, 4,000 screenings on
    // 2026-09-02) is the one a visitor filters for, `DUB` the rare foreign-language
    // print. Both are what the chains' own labels normalise to.
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("SUB", "DUB"))
    val voiceoverToken: Option[String] = Some("LEC")
    val titleFacts: services.movies.TitleFacts.Rules = services.movies.TitleFacts.Rules(Set("en"), Some(services.movies.TitleFacts.RatingSystem.Bbfc))
  }

  /** Germany — a German-language country on its own `kinowo_de` database,
   *  sourced from the AlloCiné/Filmstarts website-JSON
   *  ([[services.cinemas.common.WebediaShowtimesClient]], via `www.filmstarts.de`).
   *  No Filmweb (Polish-only).
   *
   *  Its cities are Poland's shape of page — a major city's own venues, a town of
   *  three or more, a cluster of small towns ([[RosterPage]]) — 436 of them,
   *  grouped in the picker by Bundesland. */
  case object Germany extends Country(
    code           = "de",
    displayName    = "Deutschland",
    language       = Locale.forLanguageTag("de-DE"),
    mongoDb        = "kinowo_de",
    filmwebEnabled = false,
    webOrigin      = Some("https://showtimes.cc"),
    pathPrefix     = "/de",
    brandName      = "Showtimes",
  ) {
    val cities: Seq[City] = City.germanCities
    lazy val renamedSlugs: Map[String, String] = CitySlugSuccession.germanSuccession
    lazy val slugAliases: Map[String, String] = CitySlugIndexData.deAliases
    /** The 16 Bundesländer, each over the pages inside it — see `CityPickerGroups.germanStates`. */
    override lazy val cityGroups: Seq[CityGroup] = CityPickerGroups.germanStates
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("OmU", "DF"))
    // Filmstarts names four versions — OV, OmU, OmeU, DF — and none is a voice-over.
    val voiceoverToken: Option[String] = None
    val titleFacts: services.movies.TitleFacts.Rules = services.movies.TitleFacts.Rules(Set("de", "en"), Some(services.movies.TitleFacts.RatingSystem.Fsk))
  }

  /** The United States — an English-language country on its own `kinowo_us`
   *  database, sourced from `www.flicks.us`: the same Flicks platform the UK runs
   *  on, reached through the same [[services.cinemas.common.FlicksClient]] on the
   *  `UnitedStates` market. No Filmweb (Polish-only).
   *
   *  Its roster is the reason the worker is split per country rather than folded
   *  into a sibling's: 5,031 venues across 55 states and territories, and ~10x the
   *  UK's PACED set once the UK's chain venues (which scrape their own sites) are
   *  discounted. That is a scrape-VOLUME problem before it is anything else: the
   *  sweep runs ~10h against an origin whose 200ms pace is a measured ceiling
   *  rather than a choice, so cadence is the only lever left. Its worker overlay
   *  runs at 840min for that reason rather than copying the UK's 420, which the
   *  sweep would overrun by 44%. See `WorkerScrapeCadenceConfigSpec`. */
  case object UnitedStates extends Country(
    code           = "us",
    displayName    = "United States",
    language       = Locale.forLanguageTag("en-US"),
    mongoDb        = "kinowo_us",
    filmwebEnabled = false,
    webOrigin      = Some("https://showtimes.cc"),
    pathPrefix     = "/us",
    brandName      = "Showtimes",
  ) {
    val cities: Seq[City] = City.usCities
    lazy val renamedSlugs: Map[String, String] = CitySlugSuccession.usSuccession
    lazy val slugAliases: Map[String, String] = CitySlugIndexData.usAliases
    /** The states and territories, each over the pages anchored in it. */
    override val cityGroups: Seq[CityGroup] = CityPickerGroups.usStates
    // Same pair as the UK, and for the same reason — a subtitled print is the
    // marked case, a dubbed one rare.
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("SUB", "DUB"))
    val voiceoverToken: Option[String] = Some("LEC")
    val titleFacts: services.movies.TitleFacts.Rules = services.movies.TitleFacts.Rules(Set("en"), None)
    // US listings, chains and box offices alike, print "7:30 PM"; a 24-hour "19:30" reads as foreign.
    override val clockStyle: ClockStyle = ClockStyle.TwelveHour
  }

  /** Spain — a Spanish-language country on its own `kinowo_es` database, sourced
   *  from SensaCine: the same AlloCiné/Webedia website-JSON Germany runs on,
   *  reached through the same [[services.cinemas.common.WebediaShowtimesClient]]
   *  on the `Spain` market. No Filmweb (Polish-only).
   *
   *  Its cities are Poland's shape of page — a major city's own venues, a town of
   *  three or more, a cluster of small towns ([[RosterPage]]) — 240 of them,
   *  grouped in the picker by province. 599 venues, a third of Germany's roster,
   *  so its worker is sized like the UK's rather than like Germany's.
   *
   *  It shares a client with Germany but NOT a request budget: `www.sensacine.com`
   *  is a different host, so the pace gate and the 429 back-off — both keyed by
   *  full hostname — keep the two markets independent. That independence is only
   *  real because the host has its own `HostPolicies` row; rows match by SUFFIX,
   *  and a market without one is not paced at all. */
  case object Spain extends Country(
    code           = "es",
    displayName    = "España",
    language       = Locale.forLanguageTag("es-ES"),
    mongoDb        = "kinowo_es",
    filmwebEnabled = false,
    webOrigin      = Some("https://showtimes.cc"),
    pathPrefix     = "/es",
    brandName      = "Showtimes",
  ) {
    val cities: Seq[City] = City.spanishCities
    lazy val renamedSlugs: Map[String, String] = CitySlugSuccession.spanishSuccession
    lazy val slugAliases: Map[String, String] = CitySlugIndexData.esAliases
    /** One group per province — see `CityPickerGroups.spanishProvinces`. */
    override lazy val cityGroups: Seq[CityGroup] = CityPickerGroups.spanishProvinces
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("VOSE", "DOB"))
    // SensaCine names VO, VOSE, VOSI, DOB and CAT — and no voice-over.
    val voiceoverToken: Option[String] = None
    val titleFacts: services.movies.TitleFacts.Rules = services.movies.TitleFacts.Rules(Set("es", "en"), None)
  }

  /** Every country the codebase knows about. A worker iterates this; a web
   *  deployment picks one via `KINOWO_COUNTRY` (`settings.ProcessConfiguration.country`). */
  val all: Seq[Country] = Seq(Poland, UnitedKingdom, Germany, UnitedStates, Spain)

  /** The fallback country when `KINOWO_COUNTRY` is unset — keeps single-country
   *  (Poland-only) deployments and tests working with no new env var. */
  val default: Country = Poland

  /** The countries a user can SWITCH to from the web navbar: those with a real
   *  deployment host ([[Country.webUrl]] defined), in [[all]] order (Poland,
   *  UK, Germany, US, Spain). The country `<select>` renders only when this holds more than
   *  one entry. */
  val switchable: Seq[Country] = all.filter(_.webOrigin.isDefined)

  /** The page a link that names no country means — a bare `showtimes.cc/{slug}/`,
   *  a `kinowo://{slug}` custom-scheme link — with its country, and whether the
   *  table CHANGED the answer (its owner is not the one rules 2 and 3 would pick):
   *   1. a slug [[LegacySlugOwners]] lists goes to the country that owned it before
   *      slugs were per country, wherever else it now answers (`york` → the US);
   *   2. else the one country that answers to it (a page, a renamed slug, an alias);
   *   3. else, several answering, the first in [[switchable]]'s order.
   *  The apps read the same table from the catalog (`bareSlugOwners`) and apply 2
   *  and 3 in the catalog's country order, which is this one. */
  def bareSlugTarget(slug: String): Option[BareSlugTarget] = {
    val withoutTable = switchable.iterator.flatMap(c => c.resolve(slug).map(c -> _)).nextOption()
    LegacySlugOwners.owners.get(slug) match {
      case Some(code) =>
        byCode(code).flatMap(c => c.resolve(slug).map(BareSlugTarget(c, _, tableChangedAnswer = !withoutTable.exists(_._1 == c))))
      case None => withoutTable.map { case (c, city) => BareSlugTarget(c, city, tableChangedAnswer = false) }
    }
  }

  def byCode(code: String): Option[Country] =
    all.find(_.code.equalsIgnoreCase(code.trim))

  /** Which country a city belongs to. Every [[City]] is in exactly one
   *  country's [[cities]] list, so this is unambiguous; falls back to
   *  [[default]] for a city not yet grouped (shouldn't happen).
   *
   *  Indexed rather than scanned: this is on the path of every film link a page
   *  renders (each one asks its city for the deployment's URL prefix), and the
   *  scan it replaces walked every country's city list per call. */
  def of(city: City): Country = byCity.getOrElse(city, default)

  private lazy val byCity: Map[City, Country] =
    all.flatMap(c => c.cities.map(_ -> c)).toMap

}
