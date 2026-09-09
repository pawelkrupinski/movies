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
   * where the country's picker groups its cities, the group's label as `region`
   * and, where that group nests a sub-group worth a tap of its own (more than
   * one city under it), that sub-group's label as `subregion`. Each country
   * carries its `timezone` and, where it has one, the `versionTokens` pair its
   * "version" filter matches on.
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
        // Neither is right on its own, which is why each city carries its OWN
        // zone below and both apps prefer it (`ios/Kinowo/Models/City.swift`,
        // `android/.../model/City.kt`); this field is what they fall back to for
        // a city that omits one.
        val timezone = countryTimezone(c)
        // The pair the country's own scrapers mark a subtitled/dubbed screening
        // with — the same one the web's version radios are rendered from. The
        // apps' "version" filter matches a literal `Showtime.format` token, and
        // before this field existed both hardcoded Poland's `NAP`/`DUB`, so the
        // filter matched nothing in Germany (`OmU`/`DF`) or Spain (`VOSE`/`DOB`).
        // Absent for a country that marks neither, and the apps then hide the
        // row rather than offer a filter that can only match nothing.
        val versionTokens = c.versionTokens.fold("")(t => s""","versionTokens":{"subtitled":"${t.subtitled}","dubbed":"${t.dubbed}"}""")
        s"""{"code":"${c.code}","name":"${c.displayName}","baseUrl":"${c.webUrl.get}","language":"${c.language.getLanguage}","brand":"${c.brandName}","timezone":"$timezone"$versionTokens}"""
      }
      .mkString("[", ",", "]")
    val cities = Country.switchable
      .flatMap { c =>
        // A country whose picker GROUPS its cities (the US by state, Germany by
        // Bundesland, the UK by nation) names each city's group, so the apps can
        // offer the same two-step pick the web does: 468 US places in one A-to-Z
        // is not a list anybody reads, "California" then "Los Angeles" is. Absent
        // in the flat countries — Poland and Spain — where a name is all a visitor
        // needs, so the field costs bytes only where it earns them.
        //
        // The TOP level, through `allCities`, wherever the web nests deeper than
        // one (the UK puts a county between its nation and its places). `region`
        // names the nation regardless of depth, so a city keeps the same `region`
        // whether or not its county went on to earn a `subregion` below.
        //
        // Absent where the TOP group itself collapsed onto its one city
        // (`soleCity` — Berlin and Hamburg, Germany's single-region
        // city-states; Delaware and Vermont, US states too small to split).
        // Naming a "Berlin" region a visitor would only ever pick to reach the
        // one thing under it costs a tap for nothing, so that city shows as a
        // direct row on the region STEP instead, exactly where a flat
        // country's cities always have.
        val regionOf = c.cityGroups.filter(_.soleCity.isEmpty)
          .flatMap(g => g.allCities.map(_.slug -> g.label)).toMap
        // The SECOND level — present only for a sub-group that actually holds MORE
        // THAN ONE city (`CityGroup.soleCity` is `None`): the UK's West Midlands
        // (Birmingham/Dudley/Sandwell), Glamorgan (Cardiff/Glamorgan) and Antrim
        // (Antrim/Belfast) are the only three today. A county that collapsed onto
        // its one place already reads correctly through `region` alone — Cheshire
        // needs no extra tap to reach Cheshire — so this stays absent there, and
        // absent entirely for Germany and the US, whose groups don't nest a level
        // this deep. The apps' third picker step reads this field and skip it when
        // it's absent, same shape as `region`/`timezone` below.
        val subregionOf = c.cityGroups.flatMap { g =>
          g.groups.filter(_.soleCity.isEmpty).flatMap(sub => sub.allCities.map(_.slug -> sub.label))
        }.toMap
        // The city's own zone, but ONLY where it differs from the country's — the
        // field a client falls back from, so writing it out where it would say the
        // same thing costs bytes and says nothing. Four countries keep one zone
        // throughout and emit none at all; the US spans six, and this is what lets
        // an app prune a Los Angeles showtime on Pacific instead of on whatever one
        // zone the country had to pick (see the country `timezone` above).
        val countryZone = countryTimezone(c)
        c.cities.map { city =>
          val region = regionOf.get(city.slug).fold("")(label => s""","region":"$label"""")
          val subregion = subregionOf.get(city.slug).fold("")(label => s""","subregion":"$label"""")
          val zone   = city.zoneId.getId
          val tz     = if (zone == countryZone) "" else s""","timezone":"$zone""""
          s"""{"slug":"${city.slug}","name":"${city.labels.nominative}","lat":${city.lat},"lon":${city.lon},"country":"${c.code}"$region$subregion$tz}"""
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
