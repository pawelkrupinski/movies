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
  val code:           String,   // ISO-ish short code, also the URL-free identifier: "pl", "uk"
  val language:       Locale,   // UI language + collation locale
  val mongoDb:        String,   // database name on the shared cluster
  val filmwebEnabled: Boolean,  // is the Filmweb rating/fallback path wired for this country?
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
}

object Country {

  case object Poland extends Country(
    code           = "pl",
    language       = Locale.forLanguageTag("pl-PL"),
    // Poland keeps the original database name so the existing prod deployment is
    // byte-identical — do NOT rename this to `kinowo_pl`.
    mongoDb        = "kinowo",
    filmwebEnabled = true,
  ) {
    val cities: Seq[City] = City.polishCities
  }

  /** Every country the codebase knows about. A worker iterates this; a web
   *  deployment picks one via [[fromEnv]]. */
  val all: Seq[Country] = Seq(Poland)

  /** The fallback country when `KINOWO_COUNTRY` is unset — keeps single-country
   *  (Poland-only) deployments and tests working with no new env var. */
  val default: Country = Poland

  def byCode(code: String): Option[Country] =
    all.find(_.code.equalsIgnoreCase(code.trim))

  /** Which country a city belongs to. Every [[City]] is in exactly one
   *  country's [[cities]] list, so this is unambiguous; falls back to
   *  [[default]] for a city not yet grouped (shouldn't happen). */
  def of(city: City): Country = all.find(_.cities.contains(city)).getOrElse(default)

  /** The country THIS process serves, from `KINOWO_COUNTRY` (default: Poland).
   *  A web deployment resolves it once at boot; the worker uses [[all]] instead. */
  def fromEnv: Country = Env.get("KINOWO_COUNTRY").flatMap(byCode).getOrElse(default)

  /** The Mongo database name this process should use: an explicit `MONGODB_DB`
   *  wins (local dev / overrides), otherwise it is DERIVED from the country
   *  (`KINOWO_COUNTRY=uk` → `kinowo_uk`, unset → Poland → `kinowo`). Single
   *  source of truth so no call site re-spells the `"kinowo"` fallback and a
   *  country can never silently land in the wrong database. */
  def resolvedDbName: String = Env.get("MONGODB_DB").getOrElse(fromEnv.mongoDb)
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
