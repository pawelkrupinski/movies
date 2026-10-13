package models

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

  /** Each country's live slugs, built once for the whole catalog. */
  private lazy val liveSlugs: Map[Country, Set[String]] =
    Country.switchable.map(c => c -> c.cities.map(_.slug).toSet).toMap

  /** Slugs a city of a country other than `c` answers at live — see `formerSlugs` in [[json]]. */
  private def liveElsewhere(c: Country): Set[String] =
    liveSlugs.collect { case (other, slugs) if other != c => slugs }.flatten.toSet

  /** The zone a country is published under — see [[Country.zone]]. */
  private def countryTimezone(c: Country): String = c.zone.getId


  /**
   * Canonical, deterministic JSON body: `{"countries":[…],"cities":[…]}`. Order
   * is fixed ([[Country.switchable]] order; each country's cities in PICKER
   * order — [[Country.cityGroups]]'s nested traversal for a grouped country,
   * alphabetical for a flat one — never the roster's raw declared order), so
   * [[etag]] and the checked-in bundled seed stay stable across builds, AND
   * every client that walks this array in encounter order (region/subregion
   * grouping on the apps and the web's dynamic picker) reproduces the exact
   * same order the picker tree already computed, rather than re-deriving its
   * own. Hand-built (no play-json in `common`); the field values carry no
   * characters needing JSON escaping. Mirrors the `{slug,name,lat,lon}` city
   * shape the web `ALL_CITIES` clients already parse, plus the owning country
   * `code` — the single country-code space (`pl`/`uk`) the apps key on — and,
   * where the country's picker groups its cities, the group's label as `region`
   * and, where that group nests a sub-group worth a tap of its own (more than
   * one city under it), that sub-group's label as `subregion`, and, where the
   * web redirects older slugs onto it, those as `formerSlugs`. Each country
   * carries its `timezone` and, where it has one, the `versionTokens` pair its
   * "version" filter matches on.
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
        val elsewhere = liveElsewhere(c)
        val aliasesByPage = c.slugAliases.toSeq.groupMap(_._2)(_._1)
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
        // THAN ONE city (`CityGroup.soleCity` is `None`): a UK county
        // holding several pages (Glamorgan: Cardiff, Swansea…). A county that collapsed onto
        // its one place already reads correctly through `region` alone — Shetland
        // needs no extra tap to reach Lerwick — so this stays absent there, and
        // absent entirely for Germany and the US, whose groups don't nest a level
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
          .filter { case (_, names) => names.nonEmpty }
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
          val region = regionOf.get(city.slug).fold("")(label => s""","region":"$label"""")
          val subregion = subregionOf.get(city.slug).fold("")(label => s""","subregion":"$label"""")
          val aliases = aliasesOf.get(city.slug).fold("")(names => names.map(n => s""""$n"""").mkString(""","aliases":[""", ",", "]"))
          val zone   = city.zoneId.getId
          val tz     = if (zone == countryZone) "" else s""","timezone":"$zone""""
          // The slugs the web now 301s onto this city (`Country.renamedSlugs`), its own
          // country's only — a retired page's, a formerly qualified one's. The apps claim
          // those URLs too (App Links, Universal Links), and a link opened in an app never
          // reaches the server's redirect: without this an app rejected
          // `/miedzyrzec-podlaski/movie/…` as an unknown city and opened nothing.
          //
          // Less any slug ANOTHER country serves as a live city. App builds from before
          // slugs were per country read every country's former slugs as one map and
          // apply it to the saved city of whichever country is selected: the UK's
          // retired `/kent/` (→ Chatham) would turn a US user's Kent, Washington into a
          // city the US does not have. Such a link still 301s on the web.
          val formerSlugs = c.formerSlugs(city.slug).filterNot(elsewhere)
          val former = if (formerSlugs.isEmpty) "" else formerSlugs.map(s => s""""$s"""").mkString(""","formerSlugs":[""", ",", "]")
          // EVERY other slug of its own country that leads here, unfiltered — what
          // current app builds read in place of `formerSlugs`, resolving a link within
          // the country its URL names: a renamed or retired page's slug (the UK's
          // `/kent/` → Chatham, though the US has a Kent), a town the page covers
          // (`/uk/ilkley/`) and the page's qualified slug (`/uk/york-north-yorkshire/`)
          // — every URL the App Links and Universal Links claim that the web 301s here.
          val others = (c.formerSlugs(city.slug) ++ aliasesByPage.getOrElse(city.slug, Nil)).distinct.sorted
          val otherSlugs = if (others.isEmpty) "" else others.map(s => s""""$s"""").mkString(""","otherSlugs":[""", ",", "]")
          s"""{"slug":"${city.slug}","name":"${city.labels.nominative}","lat":${city.lat},"lon":${city.lon},"country":"${c.code}"$region$subregion$aliases$tz$former$otherSlugs}"""
        }
      }
      .mkString("[", ",", "]")
    val owners = LegacySlugOwners.owners.toSeq.sorted.map { case (slug, code) => s""""$slug":"$code"""" }.mkString("{", ",", "}")
    s"""{"countries":$countries,"cities":$cities,"bareSlugOwners":$owners}"""
  }

  /**
   * Strong ETag over [[json]] — a quoted 16-hex-char SHA-256 prefix. Immutable
   * per build, so it's computed once. The apps send it as `If-None-Match`; the
   * server answers `304 Not Modified` when it matches.
   */
  val etag: String = tools.Digest.contentEtag(json)
}
