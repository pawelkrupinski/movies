package models

import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString, JsValue, Json}

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

  /** Every city's `slug` as the catalog emits it ([[InstalledBuildSlugs]]) — see
   *  `formerSlugs` in [[json]]. */
  private lazy val catalogSlugs: Set[String] =
    Country.switchable.flatMap(c => c.cities.map(InstalledBuildSlugs.of(c, _))).toSet

  /** The zone a country is published under — see [[Country.zone]]. */
  private def countryTimezone(c: Country): String = c.zone.getId

  /** A row's fields in order, each absent one (`None`) left out: an optional field is absent rather than `null` or
   *  `[]`, so it costs bytes only where it says something. */
  private def row(fields: (String, Option[JsValue])*): JsObject =
    JsObject(fields.collect { case (name, Some(value)) => name -> value })

  private def text(value: String): Option[JsValue] = Some(JsString(value))
  private def texts(values: Seq[String]): Option[JsValue] = Option.when(values.nonEmpty)(JsArray(values.map(JsString(_))))

  /** One city's row — see the fields' reasons in [[json]]. */
  private[models] def cityRow(slug: String, pageSlug: String, name: String, lat: Double, lon: Double, country: String,
      region: Option[String], subregion: Option[String], aliases: Seq[String], timezone: Option[String],
      formerSlugs: Seq[String], otherSlugs: Seq[String]): JsObject =
    row("slug" -> text(slug), "pageSlug" -> text(pageSlug), "name" -> text(name), "lat" -> Some(JsNumber(lat)),
      "lon" -> Some(JsNumber(lon)), "country" -> text(country), "region" -> region.flatMap(text),
      "subregion" -> subregion.flatMap(text), "aliases" -> texts(aliases), "timezone" -> timezone.flatMap(text),
      "formerSlugs" -> texts(formerSlugs), "otherSlugs" -> texts(otherSlugs))

  /**
   * Canonical, deterministic JSON body: `{"countries":[…],"cities":[…]}`. Order
   * is fixed ([[Country.switchable]] order; each country's cities in PICKER
   * order — [[Country.cityGroups]]'s nested traversal for a grouped country,
   * alphabetical for a flat one — never the roster's raw declared order), so
   * [[etag]] and the checked-in bundled seed stay stable across builds, AND
   * every client that walks this array in encounter order (region/subregion
   * grouping on the apps and the web's dynamic picker) reproduces the exact
   * same order the picker tree already computed, rather than re-deriving its
   * own. Every value is escaped as JSON (a name with a quote in it is still that
   * name). Mirrors the `{slug,name,lat,lon}` city
   * shape the web `ALL_CITIES` clients already parse, plus the owning country
   * `code` — the single country-code space (`pl`/`uk`) the apps key on — and,
   * where the country's picker groups its cities, the group's label as `region`
   * and, where that group nests a sub-group worth a tap of its own (more than
   * one city under it), that sub-group's label as `subregion`, and, where the
   * web redirects older slugs onto it, those as `formerSlugs`. Each country
   * carries its `timezone` and, where it has one, the `versionTokens` pair its
   * "version" filter matches on.
   *
   * A city's `slug` is unique across the catalog — the name installed builds from
   * before slugs were per country know it by — and its `pageSlug` is its slug on
   * its own country, the key current builds read with `country`
   * ([[InstalledBuildSlugs]]; temporary, with [[LegacySlugOwners]]).
   *
   * `bareSlugOwners` is [[LegacySlugOwners]] — slug → the country that owned it
   * before slugs were per country — which decides a link that names no country (a
   * bare `showtimes.cc/{slug}/`, `kinowo://{slug}`) on the web
   * ([[Country.bareSlugTarget]]) and in the apps alike; a slug it does not list
   * goes to the first country, in this catalog's order, that answers to it.
   * Temporary, with the table.
   */
  val json: String = {
    val countries = Country.switchable
      .map { c =>
        // The country's IANA zone, so the mobile apps prune past showtimes
        // against local wall-clock (a London show disappears on Europe/London,
        // not Warsaw).
        //
        // The zone of the country's BIGGEST city (most venues), not of whichever
        // happens to sort first. Four of the five countries keep one zone
        // throughout, so for them any city answers; the US spans six, and
        // reading the first made this value a function of roster ORDER — a
        // generator change that reshuffled the states once moved it from Chicago
        // to Pago Pago, and nothing failed. Biggest is at least the answer most
        // of the country's users are on, and it does not move when the roster is
        // re-sorted. For the US that is New York, so America/New_York.
        //
        // It is still ONE zone for a country that has six: pruning a Los Angeles
        // show on Eastern drops a 19:00 show three hours EARLY. That is why each
        // city whose zone differs from this one carries its OWN zone below and
        // both apps prefer it (`ios/Kinowo/Models/City.swift`,
        // `android/.../model/City.kt`); this field is what they fall back to for
        // a city that omits one, which only a city in this very zone does.
        val timezone = countryTimezone(c)
        // The pair the country's own scrapers mark a subtitled/dubbed screening
        // with — the same one the web's version radios are rendered from. The
        // apps' "version" filter matches a literal `Showtime.format` token, and
        // before this field existed both hardcoded Poland's `NAP`/`DUB`, so the
        // filter matched nothing in Germany (`OmU`/`DF`) or Spain (`VOSE`/`DOB`).
        // Absent for a country that marks neither, and the apps then hide the
        // row rather than offer a filter that can only match nothing.
        row("code" -> text(c.code), "name" -> text(c.displayName), "baseUrl" -> text(c.webUrl.get),
          "language" -> text(c.language.getLanguage), "brand" -> text(c.brandName), "timezone" -> text(timezone),
          "versionTokens" -> c.versionTokens.map(t => Json.obj("subtitled" -> t.subtitled, "dubbed" -> t.dubbed)))
      }
    val cities = Country.switchable
      .flatMap { c =>
        val aliasesByPage = c.slugAliases.toSeq.groupMap(_._2)(_._1)
        // A country whose picker GROUPS its cities (the US by state, Germany by
        // Bundesland, Poland by voivodeship, Spain by province, the UK by nation)
        // names each city's group, so the apps can offer the same two-step pick the
        // web does: 1,995 US pages in one A-to-Z is not a list anybody reads,
        // "California" then "Los Angeles" is. Absent in a flat country, where a name
        // is all a visitor needs, so the field costs bytes only where it earns them.
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
        // THAN ONE city (`CityGroup.soleCity` is `None`): a UK county
        // holding several pages (Glamorgan: Cardiff, Swansea…). A county that collapsed onto
        // its one place already reads correctly through `region` alone — Shetland
        // needs no extra tap to reach Lerwick — so this stays absent there, and
        // absent entirely for every other country, whose groups don't nest a level
        // this deep. The apps' third picker step reads this field and skip it when
        // it's absent, same shape as `region`/`timezone` below.
        val subregionOf = c.cityGroups.flatMap { g =>
          g.groups.filter(_.soleCity.isEmpty).flatMap(sub => sub.allCities.map(_.slug -> sub.label))
        }.toMap
        // The other names a city is findable by (`CityGroup.aliasesOf`), from the
        // group it sits directly in: a COLLAPSED group's own name — the District
        // of Columbia, Guam and American Samoa over their one place, Shetland
        // over Lerwick — and the retired pages that redirect to it ("Sandwell" →
        // West Bromwich). The web's picker search reads the same list
        // (`data-alias`); without it here, an app user typing "guam" finds
        // nothing, the row being "Dededo Village area". Absent where there is
        // nothing to add.
        def everyGroup(groups: Seq[CityGroup]): Seq[CityGroup] =
          groups.flatMap(g => g +: everyGroup(g.groups))
        val aliasesOf = everyGroup(c.cityGroups)
          .flatMap(g => g.cities.map(city => city.slug -> g.aliasesOf(city)))
          .toMap
        // The city's own zone, but ONLY where it differs from the country's — the
        // field a client falls back from, so writing it out where it would say the
        // same thing costs bytes and says nothing. Four countries keep one zone
        // throughout and emit none at all; the US spans six, and this is what lets
        // an app prune a Los Angeles showtime on Pacific instead of on whatever one
        // zone the country had to pick (see the country `timezone` above).
        val countryZone = countryTimezone(c)
        // PICKER order, not `c.cities`' raw declared order: a grouped country's
        // own nested tree (nation→county→city, Land→city, state→city — each
        // level already sorted the way its own picker wants), a flat country's
        // plain alphabetical list otherwise. Every client that derives group
        // order from first-occurrence over this array — the apps' `region`/
        // `subregion`, the web's dynamic picker — inherits that order for free,
        // instead of re-deriving (and disagreeing on) its own.
        val orderedCities = if (c.cityGroups.nonEmpty) c.cityGroups.flatMap(_.allCities) else c.allSorted
        orderedCities.map { city =>
          val zone = city.zoneId.getId
          // The slugs the web now 301s onto this city (`Country.renamedSlugs`), its own
          // country's only — a retired page's, a formerly qualified one's. The apps claim
          // those URLs too (App Links, Universal Links), and a link opened in an app never
          // reaches the server's redirect: without this an app rejected
          // `/miedzyrzec-podlaski/movie/…` as an unknown city and opened nothing.
          //
          // Less any slug the catalog names a city by (`slug`, below). No RELEASED build
          // reads this field — 2.0.11 predates it (fe6a847dd) and current builds read
          // `otherSlugs` — but the unreleased builds between the two read every
          // country's former slugs as one map onto `slug`, applied to the saved city of
          // whichever country is selected: a former slug that is a catalog `slug` would
          // move such a build off the city that answers at it. Kept, with the filter,
          // for those builds and as current builds' fallback from an older server
          // without `otherSlugs`; it goes when that fallback does. Such a link still
          // 301s on the web.
          val formerSlugs = c.formerSlugs(city.slug).filterNot(catalogSlugs)
          // EVERY other slug of its own country that leads here, unfiltered — what
          // current app builds read in place of `formerSlugs`, resolving a link within
          // the country its URL names: a renamed or retired page's slug (the UK's
          // `/kent/` → Chatham, though the US has a Kent), a town the page covers
          // (`/uk/ilkley/`) and the page's qualified slug (`/uk/york-north-yorkshire/`)
          // — every URL the App Links and Universal Links claim that the web 301s here.
          val otherSlugs = (c.formerSlugs(city.slug) ++ aliasesByPage.getOrElse(city.slug, Nil)).distinct.sorted
          // `slug` is what installed builds from before slugs were per country know
          // the city by — unique across the whole catalog, as they need it to be
          // ([[InstalledBuildSlugs]]); `pageSlug` is the city's own slug on its own
          // country, what current builds key on with `country`. TEMPORARY: once those
          // builds are gone, `slug` goes back to being `pageSlug`.
          cityRow(InstalledBuildSlugs.of(c, city), city.slug, city.labels.nominative, city.lat, city.lon, c.code,
            regionOf.get(city.slug), subregionOf.get(city.slug), aliasesOf.getOrElse(city.slug, Nil),
            Option.when(zone != countryZone)(zone), formerSlugs, otherSlugs)
        }
      }
    val owners = JsObject(LegacySlugOwners.owners.toSeq.sorted.map { case (slug, code) => slug -> JsString(code) })
    Json.stringify(Json.obj("countries" -> countries, "cities" -> cities, "bareSlugOwners" -> owners))
  }

  /**
   * Strong ETag over [[json]] — a quoted 16-hex-char SHA-256 prefix. Immutable
   * per build, so it's computed once. The apps send it as `If-None-Match`; the
   * server answers `304 Not Modified` when it matches.
   */
  val etag: String = tools.Digest.contentEtag(json)
}
