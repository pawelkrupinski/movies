package models

import java.text.Collator
import java.util.Locale

import tools.Env

/**
 * A country of cinema repertoire — the scope ABOVE [[City]]. Until now Poland
 * was implicit everywhere (PL scraper clients, `Europe/Warsaw`, Polish grammar,
 * `<html lang="pl">`, the `kinowo` database). `Country` makes that dimension
 * explicit so a second country can run without colliding with Poland:
 *
 *   - each country owns its own set of [[City]] objects ([[cities]]),
 *   - each maps to its OWN Mongo database ([[mongoDb]]) on the shared cluster,
 *   - each carries its UI [[language]] (for collation + i18n),
 *   - each says how its city list is GROUPED in a picker ([[cityGroups]] — the
 *     US's metros are found through their state), and
 *   - each decides whether the Filmweb rating/fallback path applies
 *     ([[filmwebEnabled]] — a new country won't use Filmweb at all).
 *
 * A WEB deployment serves exactly ONE country, picked once at boot from
 * `KINOWO_COUNTRY` ([[fromEnv]]); the WORKER may instantiate its object graph
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
   *  the host has to be the bare apex ([[Country.isApexHost]]), AND this
   *  deployment has to be mounted at the root. Since the Showtimes countries
   *  moved under `showtimes.cc/{code}/`, the apex is ALSO the host every one of
   *  their own pages arrives on — a host check alone would replace the UK
   *  site's homepage with a country picker. The country mounted at `/` is the
   *  only one whose `/` is not already a country's landing. */
  def servesApex(host: String): Boolean = pathPrefix.isEmpty && Country.isApexHost(host)

  /** How the picker at `/` ARRANGES [[cities]]: empty for a flat list (Poland's
   *  41, Germany's 158, Spain's 52 — a name is all a visitor needs), one group
   *  per US state or per UK nation, because "Los Angeles" is found under
   *  "California" and neither 457 metros nor 79 counties is a list anybody reads
   *  straight through. Where it is non-empty the groups PARTITION [[cities]] —
   *  `CountrySpec` holds that. */
  def cityGroups: Seq[CityGroup] = Nil

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

  /** The token a VOICE-OVER screening carries — one narrator read over the
   *  original soundtrack, which is neither dubbing nor subtitles and is a version
   *  of its own wherever it is offered. Poland writes it `LEK`; the
   *  English-speaking deployments `LEC`.
   *
   *  Its own member rather than a third field on [[VersionTokens]] because that
   *  pair is what the Filtry radios FILTER on, and a voice-over screening is not
   *  one of the two choices those radios offer. */
  def voiceoverToken: String = "LEK"

  lazy val bySlug: Map[String, City] = cities.map(c => c.slug -> c).toMap

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
  def shareHost: String = Country.withoutScheme(ogOrigin)

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

  /** `url` with its scheme stripped — `kinowo.net`, `showtimes.cc/uk`. For the
   *  places that DISPLAY an address rather than link to it: the footer drawn
   *  into every Open Graph card ([[Country.shareHost]]), the button on a retired
   *  deployment's notice page. */
  def withoutScheme(url: String): String = url.stripPrefix("https://").stripPrefix("http://")

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
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("NAP", "DUB"))
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
    /** The four nations plus the Crown Dependencies, each over the counties and
     *  cities inside it — 79 places is not an A-to-Z anybody reads. */
    override val cityGroups: Seq[CityGroup] = City.ukNations
    // Britain subtitles rather than dubs: `SUB` (captions, 4,000 screenings on
    // 2026-09-02) is the one a visitor filters for, `DUB` the rare foreign-language
    // print. Both are what the chains' own labels normalise to.
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("SUB", "DUB"))
    override val voiceoverToken: String = "LEC"
  }

  /** Germany — a German-language country on its own `kinowo_de` database,
   *  sourced from the AlloCiné/Filmstarts website-JSON
   *  ([[services.cinemas.common.WebediaShowtimesClient]], via `www.filmstarts.de`).
   *  No Filmweb (Polish-only). */
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
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("OmU", "DF"))
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
    /** The states and territories, each over the metros cut out of it — the one
     *  country whose picker is grouped. */
    override val cityGroups: Seq[CityGroup] = City.usStates
    // Same pair as the UK, and for the same reason — a subtitled print is the
    // marked case, a dubbed one rare.
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("SUB", "DUB"))
    override val voiceoverToken: String = "LEC"
  }

  /** Spain — a Spanish-language country on its own `kinowo_es` database, sourced
   *  from SensaCine: the same AlloCiné/Webedia website-JSON Germany runs on,
   *  reached through the same [[services.cinemas.common.WebediaShowtimesClient]]
   *  on the `Spain` market. No Filmweb (Polish-only).
   *
   *  Its cities are the 52 PROVINCES SensaCine itself enumerates — flat, like
   *  Germany's regions, because 52 is a list a picker stays readable at and a
   *  province is what a Spanish visitor names. 595 venues, a third of Germany's
   *  roster, so its worker is sized like the UK's rather than like Germany's.
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
    override val versionTokens: Option[VersionTokens] = Some(VersionTokens("VOSE", "DOB"))
  }

  /** Every country the codebase knows about. A worker iterates this; a web
   *  deployment picks one via [[fromEnv]]. */
  val all: Seq[Country] = Seq(Poland, UnitedKingdom, Germany, UnitedStates, Spain)

  /** The fallback country when `KINOWO_COUNTRY` is unset — keeps single-country
   *  (Poland-only) deployments and tests working with no new env var. */
  val default: Country = Poland

  /** The countries a user can SWITCH to from the web navbar: those with a real
   *  deployment host ([[Country.webUrl]] defined), in [[all]] order (Poland,
   *  UK, Germany, US, Spain). The country `<select>` renders only when this holds more than
   *  one entry. */
  val switchable: Seq[Country] = all.filter(_.webOrigin.isDefined)

  /** THE BRAND FRONT DOOR: the bare domain that fronts several countries rather
   *  than serving one. `showtimes.cc/uk` and `showtimes.cc/de` are countries;
   *  `showtimes.cc/` itself is this — a picker listing every deployed country,
   *  Poland included, even though Poland answers on its own domain.
   *
   *  It is a check on the REQUEST rather than a deployment of its own,
   *  deliberately: there is no fourth thing to keep running, the proxy simply
   *  points the apex root at a deployment that is mounted there. Since the
   *  Showtimes countries share this host, that can only be the country on its
   *  own domain — see [[Country.servesApex]]. */
  val apexHost: String = "showtimes.cc"

  /** The brand named on the front door — the non-Polish brand, since the apex is
   *  the Showtimes domain. Poland keeps its own brand on its own domain. */
  val apexBrandName: String = "Showtimes"

  /** THE ONE ORIGIN EVERY OAUTH PROVIDER REDIRECTS BACK TO.
   *
   *  Google and Facebook each match `redirect_uri` byte-for-byte against a list
   *  registered in their console, so the obvious shape — every deployment naming
   *  its own address — costs one console entry per country per provider, and
   *  makes "register two URLs by hand, in two consoles" a silent prerequisite for
   *  launching a country. It is silent because nothing fails until a real person
   *  tries to sign in on the new site.
   *
   *  So every country sends the provider HERE, and the deployment mounted at
   *  the apex ([[servesApex]]) either finishes the flow — when the country that
   *  started it is on this same origin, so the browser is still sending the
   *  cookie holding its CSRF state — or relays it, unchanged, to the one that
   *  can. A country on its own domain is reached the second way.
   *
   *  The cost is a real dependency: the apex deployment is now in the path of
   *  every sign-in, including Poland's own. That is the trade the single entry
   *  buys, and it is why this is a constant rather than something each country
   *  could quietly point elsewhere. */
  val oauthCallbackOrigin: String = s"https://$apexHost"

  /** The origins this project actually answers on. Used to tell a REAL request
   *  from a local one: on a deployed origin the provider was handed
   *  [[oauthCallbackOrigin]] and the relay applies, while a developer on
   *  `http://localhost:9000` was handed their own address and must be left to
   *  finish where they started. Derived from the countries rather than listed,
   *  so a new deployment cannot be forgotten here. */
  lazy val deployedOrigins: Set[String] = switchable.flatMap(_.webOrigin).toSet

  /** The OTHER domain this project is served from, seen from the origin a request
   *  actually ARRIVED on — the one a session has to be established on separately,
   *  because no cookie can reach it from here.
   *
   *  THE ORIGIN, NOT THE DEPLOYMENT'S COUNTRY, and the difference is the whole
   *  point: the process mounted at the apex serves `showtimes.cc` while ITS
   *  country is Poland, so asking what Poland's sibling is answers
   *  `showtimes.cc` — the domain the request is already on. A sign-in finishing
   *  there would pair that domain with itself and leave kinowo.net untouched.
   *
   *  `None` when there is not exactly one other: a request off a deployed origin
   *  (a developer on localhost) has no sibling to speak of and must not be sent
   *  to production to look for one, and a third domain would make "the other one"
   *  meaningless rather than merely unknown. */
  def siblingOfOrigin(origin: String): Option[String] = {
    if (!deployedOrigins.contains(origin)) None
    else {
      val others = (deployedOrigins - origin).toSeq
      if (others.sizeIs == 1) others.headOption else None
    }
  }

  /** Is this request host the brand domain? Accepts the `www.` spelling and an
   *  explicit port so a direct hit still works where the proxy's redirect is not
   *  in front of it (local dev, a stale cache).
   *
   *  Being the brand domain is NOT on its own being the front door — every
   *  Showtimes country now serves off it too. [[Country.servesApex]] is the
   *  question callers want. */
  def isApexHost(host: String): Boolean =
    host.toLowerCase.takeWhile(_ != ':').stripPrefix("www.") == apexHost

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

  /** The country THIS process serves, from `KINOWO_COUNTRY` (default: Poland).
   *  A web deployment resolves it once at boot; the worker uses [[all]] instead. */
  def fromEnv: Country = Env.get("KINOWO_COUNTRY").flatMap(byCode).getOrElse(default)

  /** The ONE country this process serves, or `None` when it serves several.
   *
   *  The two deployments name their country through DIFFERENT variables: web sets
   *  `KINOWO_COUNTRY=de`, each worker sets `KINOWO_COUNTRIES=de` (the plural list,
   *  even though every deployed worker names exactly one). Anything process-global
   *  that must be country-correct has to consult BOTH — reading only the singular
   *  silently hands a worker the Poland default, which is how the country-scoped
   *  title rules shipped working on web and doing nothing on the worker that
   *  actually writes the corpus.
   *
   *  `None` for a multi-country worker: no single process-global value can be
   *  right for it, so callers must scope per country rather than pick one. */
  def soleFromEnv: Option[Country] =
    soleFrom(Env.get("KINOWO_COUNTRY"), Env.get("KINOWO_COUNTRIES"))

  /** Pure core of [[soleFromEnv]] — the precedence, testable without touching the
   *  environment. Public so a spec can assert what a GIVEN deployment's env shape
   *  resolves to (e.g. the worker's `KINOWO_COUNTRIES=de` and no `KINOWO_COUNTRY`)
   *  without mutating process state. */
  def soleFrom(country: Option[String], countries: Option[String]): Option[Country] = {
    val singular = country.flatMap(byCode)
    val listed = countries
      .map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).flatMap(byCode).toList.distinct)
      .getOrElse(Nil)
    singular.orElse(if (listed.sizeIs == 1) listed.headOption else None)
  }

  /** The countries a process was configured for when NO single one can be chosen:
   *  `KINOWO_COUNTRIES` naming several with no `KINOWO_COUNTRY` to disambiguate.
   *
   *  Exists because [[soleFrom]] answers None for TWO different situations that
   *  must not be treated alike — nothing configured at all (a dev box or a spec,
   *  where defaulting to Poland is right) and several configured (a multi-country
   *  worker, where defaulting to Poland is the 2026 incident: CinemaxX Würzburg's
   *  "Minions & Monster" keyed `minionsimonster` under Polish rules). Empty in the
   *  first case, the listed countries in the second. */
  def ambiguousFrom(country: Option[String], countries: Option[String]): List[Country] = {
    val listed = countries
      .map(_.split(",").iterator.map(_.trim).filter(_.nonEmpty).flatMap(byCode).toList.distinct)
      .getOrElse(Nil)
    if (country.flatMap(byCode).isDefined || listed.sizeIs <= 1) Nil else listed
  }

  /** [[ambiguousFrom]] over this process's environment. */
  def ambiguousFromEnv: List[Country] =
    ambiguousFrom(Env.get("KINOWO_COUNTRY"), Env.get("KINOWO_COUNTRIES"))

  /** The Mongo database name for a GIVEN country: an explicit `MONGODB_DB` wins
   *  (local dev / overrides), otherwise it is DERIVED from the country's own
   *  database. The pure per-country core the WORKER resolves each of its N
   *  countries through, so no call site re-spells the `"kinowo"` fallback and a
   *  country can never silently land in the wrong database. */
  def dbNameFor(country: Country): String = Env.get("MONGODB_DB").getOrElse(country.mongoDb)

  /** The Mongo database name THIS process should use, for the country resolved
   *  from `KINOWO_COUNTRY` ([[fromEnv]]) — the single-country (web) entry point.
   *  Same rule as [[dbNameFor]]: explicit `MONGODB_DB` wins, else the country's
   *  database (`KINOWO_COUNTRY=uk` → `kinowo_uk`, unset → Poland → `kinowo`). */
  def resolvedDbName: String = dbNameFor(fromEnv)

  /** The database holding the SHARED `users` + `userStates` collections.
   *
   *  Everything else about a deployment is per country -- its films, its cities,
   *  its own `kinowo_uk` / `kinowo_de` database -- but a PERSON is not. The
   *  web pods are several addresses onto one product, and an account that exists on
   *  `/uk` and not on `/de` is the same person being told they have no account;
   *  worse, a session cookie that DOES reach the neighbouring country (they share
   *  one origin, see [[mountPath]]) would resolve its `userId` against a database
   *  that has never heard of it, and the visitor would be silently signed out
   *  with their hidden films and /plan picks apparently gone. So the two user
   *  collections live in ONE database every country's pod reads: `MONGODB_USERS_DB`.
   *
   *  Unset, this is the deployment's OWN database -- exactly where those two
   *  collections have always been -- so the split is opt-in per environment and a
   *  local dev, a spec, or a country deployed on its own cluster keeps the
   *  single-database shape with nothing to configure.
   *
   *  Identity makes the move safe: [[models.User]]`.id` is the lowercased email,
   *  so the same person already carries the same key in every database and
   *  merging them is a union rather than a re-key. */
  def usersDbName: String = usersDbNameFrom(Env.get("MONGODB_USERS_DB"), resolvedDbName)

  /** Pure core of [[usersDbName]] -- the precedence, testable without touching
   *  the environment. A blank or whitespace-only variable counts as UNSET rather
   *  than as a database called `""`: an empty value in a ConfigMap is how the
   *  setting gets switched off, and Mongo would reject the empty name much later,
   *  at the first query rather than at boot.
   *
   *  Public, like [[soleFrom]], so a spec can resolve a GIVEN deployment's users
   *  database without mutating process state — and so the migration that fills
   *  that database picks its target through this rule rather than restating it. */
  def usersDbNameFrom(usersDb: Option[String], ownDb: String): String =
    usersDb.map(_.trim).filter(_.nonEmpty).getOrElse(ownDb)
}

/** Shared city-list rendering used by both the global [[City]] view and each
 *  per-country [[Country]] view, so collation + JSON shape live in one place. */
private[models] object CityListing {
  def sorted(cities: Seq[City], locale: Locale): Seq[City] = {
    val collator = Collator.getInstance(locale)
    cities.sortWith((a, b) => collator.compare(a.labels.nominative, b.labels.nominative) < 0)
  }

  def json(cities: Seq[City]): String =
    cities
      .map(c => s"""{"slug":"${c.slug}","name":"${c.labels.nominative}","lat":${c.lat},"lon":${c.lon}}""")
      .mkString("[", ",", "]")
}
