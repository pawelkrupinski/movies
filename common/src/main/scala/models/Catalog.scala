package models

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
 * The mobile "catalog": the set of deployed countries and every city they serve,
 * plus a content [[etag]]. Static per build — it changes only when a country or
 * city is added/removed — so the apps fetch it once and then revalidate with a
 * conditional GET; an unchanged catalog costs a `304 Not Modified` with no body.
 *
 * Deliberately country-AGNOSTIC and identical on every deployment: built from
 * [[Country.switchable]] (the deployed countries, i.e. those with a `webUrl`), so
 * `kinowo.fly.dev` and `showtimes-uk.fly.dev` serve byte-identical bytes and the
 * same ETag. The apps ship a checked-in snapshot of [[json]] + [[etag]] as their
 * bundled seed (`tools.CatalogSnapshot`), so a fresh install can render offline
 * and its first fetch already carries the seed's ETag — a 304 when the build is
 * current.
 */
object Catalog {

  /**
   * Canonical, deterministic JSON body: `{"countries":[…],"cities":[…]}`. Order
   * is fixed ([[Country.switchable]] order; each country's cities in declared
   * order), so [[etag]] and the checked-in bundled seed stay stable across
   * builds. Hand-built (no play-json in `common`); the field values carry no
   * characters needing JSON escaping. Mirrors the `{slug,name,lat,lon}` city
   * shape the web `ALL_CITIES` clients already parse, plus the owning country
   * `code` — the single country-code space (`pl`/`uk`) the apps key on.
   */
  val json: String = {
    val countries = Country.switchable
      .map { c =>
        // The country's IANA zone, so the mobile apps prune past showtimes
        // against local wall-clock (a London show disappears on Europe/London,
        // not Warsaw). Every city within a country shares one zone, so the
        // first city's is representative; `Europe/Warsaw` is a safe default for
        // the (currently impossible) city-less country.
        val timezone = c.cities.headOption.map(_.zoneId.getId).getOrElse("Europe/Warsaw")
        s"""{"code":"${c.code}","name":"${c.displayName}","baseUrl":"${c.webUrl.get}","language":"${c.language.getLanguage}","brand":"${c.brandName}","timezone":"$timezone"}"""
      }
      .mkString("[", ",", "]")
    val cities = Country.switchable
      .flatMap(c => c.cities.map(city =>
        s"""{"slug":"${city.slug}","name":"${city.labels.nominative}","lat":${city.lat},"lon":${city.lon},"country":"${c.code}"}"""))
      .mkString("[", ",", "]")
    s"""{"countries":$countries,"cities":$cities}"""
  }

  /**
   * Strong ETag over [[json]] — a quoted 16-hex-char SHA-256 prefix. Immutable
   * per build, so it's computed once. The apps send it as `If-None-Match`; the
   * server answers `304 Not Modified` when it matches.
   */
  val etag: String =
    "\"" + MessageDigest.getInstance("SHA-256")
      .digest(json.getBytes(StandardCharsets.UTF_8))
      .take(8)
      .map("%02x".format(_))
      .mkString + "\""
}
