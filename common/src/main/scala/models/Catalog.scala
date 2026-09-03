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
 * `kinowo.net` and `showtimes.cc/uk` serve byte-identical bytes and the
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
   * `code` — the single country-code space (`pl`/`uk`) the apps key on — and,
   * where the country's picker groups its cities, the group's label as `region`.
   */
  /** The one zone a country is published under: its BIGGEST city's, ties by slug.
   *
   *  Four of the five keep one zone throughout, so for them any city answers. The
   *  US spans six, and reading `cities.head` made a live value a function of
   *  roster ORDER — a generator change that reshuffled the states once moved it
   *  from Chicago to Pago Pago and nothing failed. Biggest does not move when the
   *  roster is re-sorted, and it is the answer most of the country's users are on.
   *
   *  A client that reads a city's own `timezone` never needs this; it is the
   *  fallback for a city that omits one (every city of a single-zone country) and
   *  for an app too old to look. `Europe/Warsaw` for the — currently impossible —
   *  city-less country. */
  private def countryTimezone(c: Country): String =
    c.cities.maxByOption(city => (city.cinemas.size, city.slug))
      .map(_.zoneId.getId).getOrElse("Europe/Warsaw")

  val json: String = {
    val countries = Country.switchable
      .map { c =>
        // The country's IANA zone, so the mobile apps prune past showtimes
        // against local wall-clock (a London show disappears on Europe/London,
        // not Warsaw).
        //
        // The zone of the country's BIGGEST city, not of whichever happens to
        // sort first. Four of the five countries keep one zone throughout, so
        // for them any city answers; the US spans six, and reading the first
        // made this value a function of roster ORDER — a generator change that
        // reshuffled the states once moved it from Chicago to Pago Pago, and
        // nothing failed. Biggest is at least the answer most of the country's
        // users are on, and it does not move when the roster is re-sorted.
        //
        // Biggest is also the safer of the two errors it can make. It is still
        // ONE zone for a country that has six, and the US's biggest metro is Los
        // Angeles: pruning on Pacific means every zone east of it prunes LATE, so
        // a show LINGERS a couple of hours past its start. Reading the first city
        // gave Central, on which a Los Angeles user drops a 19:00 show two hours
        // EARLY — hiding a screening someone could still get to, which is the
        // error that costs them something.
        //
        // Neither is right. The fix is a per-CITY zone in this payload with both
        // apps preferring it over the country's; `City.zoneId` has been correct
        // per metro since the US metro split, so the data is already here — it is
        // the apps that still read only this field (`ios/Kinowo/Models/Country.swift`,
        // `android/.../model/Country.kt`, both for past-showtime pruning and the
        // day boundary).
        val timezone = countryTimezone(c)
        s"""{"code":"${c.code}","name":"${c.displayName}","baseUrl":"${c.webUrl.get}","language":"${c.language.getLanguage}","brand":"${c.brandName}","timezone":"$timezone"}"""
      }
      .mkString("[", ",", "]")
    val cities = Country.switchable
      .flatMap { c =>
        // A country whose picker GROUPS its cities (only the US, by state) names
        // each city's group, so the apps can offer the same two-step pick the web
        // does: 457 metros in one A-to-Z is not a list anybody reads, "California"
        // then "Los Angeles" is. Absent everywhere else, where a name is all a
        // visitor needs — so the field costs bytes only where it earns them.
        val regionOf = c.cityGroups.flatMap(g => g.cities.map(_.slug -> g.label)).toMap
        // The city's own zone, but ONLY where it differs from the country's — the
        // field a client falls back from, so writing it out where it would say the
        // same thing costs bytes and says nothing. Four countries keep one zone
        // throughout and emit none at all; the US spans six, and this is what lets
        // an app prune a Los Angeles showtime on Pacific instead of on whatever one
        // zone the country had to pick (see the country `timezone` above).
        val countryZone = countryTimezone(c)
        c.cities.map { city =>
          val region = regionOf.get(city.slug).fold("")(label => s""","region":"$label"""")
          val zone   = city.zoneId.getId
          val tz     = if (zone == countryZone) "" else s""","timezone":"$zone""""
          s"""{"slug":"${city.slug}","name":"${city.labels.nominative}","lat":${city.lat},"lon":${city.lon},"country":"${c.code}"$region$tz}"""
        }
      }
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
