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
 *   - each carries its UI [[language]] (for collation + i18n), and
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
  val webUrl:         Option[String],  // public web host of this country's deployment (scheme+host, no trailing slash); None = not deployed yet
  val brandName:      String,          // customer-facing app name: "Kinowo" in PL, "Showtimes" elsewhere (the Polish coinage means nothing abroad)
) {
  /** The cities this country serves. Authoritative per-country list; [[City.all]]
   *  is the union across every country. */
  def cities: Seq[City]

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
   *  A deployed country carries its own [[webUrl]]; one modelled but not yet
   *  deployed ([[Germany]]) falls back to the default country's host so a link
   *  still resolves rather than dangling. */
  def ogOrigin: String = webUrl.getOrElse(Country.default.webUrl.get)

  /** Filename (under `assets/img/`) of the `/` landing's share-preview montage
   *  for this country. The default country keeps the original, unsuffixed
   *  `og-home.png`; every other country gets a per-code card so a UK deployment's
   *  preview shows English posters, a German one German — the same asset path is
   *  served by every deployment (one build), so all these files are checked in. */
  def homeOgImage: String = if (this == Country.default) "og-home.png" else s"og-home-$code.png"
}

object Country {

  case object Poland extends Country(
    code           = "pl",
    displayName    = "Polska",
    language       = Locale.forLanguageTag("pl-PL"),
    // Poland keeps the original database name so the existing prod deployment is
    // byte-identical — do NOT rename this to `kinowo_pl`.
    mongoDb        = "kinowo",
    filmwebEnabled = true,
    webUrl         = Some("https://kinowo.fly.dev"),
    brandName      = "Kinowo",
  ) {
    val cities: Seq[City] = City.polishCities
  }

  /** The United Kingdom — the second country, wired end-to-end but with NO
   *  cinemas yet (`cities = Nil`): an English-language deployment on its own
   *  `kinowo_uk` database that does not use Filmweb. Its cinema scrapers + city
   *  list are the remaining `§6` work; until then a `KINOWO_COUNTRY=uk` web
   *  renders in English with an empty city list, and the worker only runs it
   *  when `KINOWO_COUNTRIES` names `uk`. */
  case object UnitedKingdom extends Country(
    code           = "uk",
    displayName    = "United Kingdom",
    language       = Locale.forLanguageTag("en-GB"),
    mongoDb        = "kinowo_uk",
    filmwebEnabled = false,
    webUrl         = Some("https://showtimes-uk.fly.dev"),
    brandName      = "Showtimes",
  ) {
    val cities: Seq[City] = City.ukCities
  }

  /** Germany — the third country: a German-language deployment on its own
   *  `kinowo_de` database, sourced from the AlloCiné/Filmstarts website-JSON
   *  ([[services.cinemas.WebediaShowtimesClient]], via `www.filmstarts.de`).
   *  No Filmweb (Polish-only). */
  case object Germany extends Country(
    code           = "de",
    displayName    = "Deutschland",
    language       = Locale.forLanguageTag("de-DE"),
    mongoDb        = "kinowo_de",
    filmwebEnabled = false,
    webUrl         = None,
    brandName      = "Showtimes",
  ) {
    val cities: Seq[City] = City.germanCities
  }

  /** Every country the codebase knows about. A worker iterates this; a web
   *  deployment picks one via [[fromEnv]]. */
  val all: Seq[Country] = Seq(Poland, UnitedKingdom, Germany)

  /** The fallback country when `KINOWO_COUNTRY` is unset — keeps single-country
   *  (Poland-only) deployments and tests working with no new env var. */
  val default: Country = Poland

  /** The countries a user can SWITCH to from the web navbar: those with a real
   *  deployment host ([[Country.webUrl]] defined), in [[all]] order (Poland
   *  first). Germany is modelled but not deployed, so it is excluded. The
   *  country `<select>` renders only when this holds more than one entry. */
  val switchable: Seq[Country] = all.filter(_.webUrl.isDefined)

  def byCode(code: String): Option[Country] =
    all.find(_.code.equalsIgnoreCase(code.trim))

  /** Which country a city belongs to. Every [[City]] is in exactly one
   *  country's [[cities]] list, so this is unambiguous; falls back to
   *  [[default]] for a city not yet grouped (shouldn't happen). */
  def of(city: City): Country = all.find(_.cities.contains(city)).getOrElse(default)

  /** The country THIS process serves, from `KINOWO_COUNTRY` (default: Poland).
   *  A web deployment resolves it once at boot; the worker uses [[all]] instead. */
  def fromEnv: Country = Env.get("KINOWO_COUNTRY").flatMap(byCode).getOrElse(default)

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
