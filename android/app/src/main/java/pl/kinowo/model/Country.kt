package pl.kinowo.model

import java.time.ZoneId

/** Historical default zone, and the fallback for a country whose catalog entry
 *  carries no timezone (an older seed, or a future country the server hasn't
 *  tagged). */
private val WARSAW_ZONE: ZoneId = ZoneId.of("Europe/Warsaw")

/**
 * A country the app can serve. Each country is its own web deployment
 * ([baseUrl]) serving its own localized `/{city}/api/repertoire` +
 * `/{city}/api/details`, and carries the UI [languageTag] the app forces when
 * that country is selected (deliberately NOT derived from the device locale, so
 * a Polish phone browsing the UK deployment still reads English — and the choice
 * stays deterministic and testable).
 *
 * Mirrors the iOS `Country` registry one-for-one so the two apps agree on the
 * set of countries, their base URLs, and their forced languages.
 */
data class Country(
    /** Server country code, e.g. `pl`, `uk` — the single code space the catalog
     *  keys on (cities carry the same code). Also the persisted selection key. */
    val code: String,
    /** Human-readable label for the country picker. */
    val displayName: String,
    /** Scheme + host of this country's web deployment; the [pl.kinowo.net.KinowoApi]
     *  base every request is built on. No trailing slash. */
    val baseUrl: String,
    /** BCP-47 primary language subtag forced as the app locale when selected. */
    val languageTag: String,
    /** The country's local IANA zone (e.g. `Europe/London`), from the catalog's
     *  per-country `timezone`. Past-showtime pruning and the Dziś/Jutro day
     *  buckets reason in this zone, so a London show disappears on London time,
     *  not Warsaw. Defaults to Warsaw when the source omits it. */
    val zoneId: ZoneId = WARSAW_ZONE,
    /** The two `Showtime.format` tokens this country's sources mark a subtitled
     *  and a dubbed screening with, from the catalog's per-country
     *  `versionTokens`. The Filtry "Wersja" choice offers exactly this pair and
     *  a deep link's `?lang=` is accepted only from it — the filter matches a
     *  LITERAL token, so a pair spelled for another country matches nothing,
     *  which is what Germany (`OmU`/`DF`) and Spain (`VOSE`/`DOB`) shipped with
     *  while both apps hardcoded Poland's. Defaults to Poland's pair when the
     *  source omits the field (a cached catalog that predates it). */
    val versionTokens: VersionTokens = VersionTokens.POLAND,
) {
    companion object {
        /** Compile-time FALLBACK registry, used only until the bundled/fetched
         *  catalog loads (and if it ever fails to decode). The live registry is
         *  the `/api/catalog` payload the catalog repository publishes. Poland is
         *  the default. Codes match the server (`pl`/`uk`/`de`/`us`/`es`). */
        val all: List<Country> = listOf(
            Country(
                code = "pl",
                displayName = "Polska",
                baseUrl = "https://kinowo.net",
                languageTag = "pl",
                zoneId = ZoneId.of("Europe/Warsaw"),
                versionTokens = VersionTokens.POLAND,
            ),
            Country(
                code = "uk",
                displayName = "United Kingdom",
                baseUrl = "https://showtimes.cc/uk",
                languageTag = "en",
                zoneId = ZoneId.of("Europe/London"),
                versionTokens = VersionTokens(subtitled = "SUB", dubbed = "DUB"),
            ),
            Country(
                code = "de",
                displayName = "Deutschland",
                baseUrl = "https://showtimes.cc/de",
                languageTag = "de",
                zoneId = ZoneId.of("Europe/Berlin"),
                versionTokens = VersionTokens(subtitled = "OmU", dubbed = "DF"),
            ),
            Country(
                code = "us",
                displayName = "United States",
                baseUrl = "https://showtimes.cc/us",
                languageTag = "en",
                // The US spans six zones, so no single one is "the country's".
                // This nominal Eastern value only matters in the window before
                // the catalog loads; the live payload's per-country `timezone`
                // (derived server-side from the first US region) replaces it.
                zoneId = ZoneId.of("America/New_York"),
                versionTokens = VersionTokens(subtitled = "SUB", dubbed = "DUB"),
            ),
            Country(
                code = "es",
                displayName = "España",
                baseUrl = "https://showtimes.cc/es",
                languageTag = "es",
                // Peninsular Spain. The Canary provinces run an hour behind on
                // Atlantic/Canary; like the US entry above this nominal value
                // only matters before the catalog loads, and the live payload's
                // per-country `timezone` replaces it.
                zoneId = ZoneId.of("Europe/Madrid"),
                versionTokens = VersionTokens(subtitled = "VOSE", dubbed = "DOB"),
            ),
        )

        val default: Country = all.first()

        /** The country for [code] in the fallback registry, or [default] when
         *  null / unknown. Bootstrap only; live lookups use the catalog list. */
        fun byCode(code: String?): Country = all.firstOrNull { it.code == normalizeCode(code) } ?: default

        /** Map a legacy persisted selection code to the current server code space.
         *  Earlier builds stored ISO codes (`PL`/`GB`/`US`); the catalog keys on
         *  `pl`/`uk`/`us`. Applied wherever a persisted code is read so an upgrade
         *  keeps the user's country without a migration write. */
        fun normalizeCode(code: String?): String? = when (code) {
            "PL" -> "pl"
            "GB" -> "uk"
            "US" -> "us"
            else -> code
        }
    }
}

/** Registry lookups over a catalog's country list — the live list the catalog
 *  repository holds, so a country added server-side appears without an app update. */
fun List<Country>.withCode(code: String?): Country? = firstOrNull { it.code == code }

/** The country for a (possibly legacy) persisted selection [code]: the live
 *  list's entry when it has one, else the compile-time registry's (which also
 *  answers the default for a null / unknown code). Mirrors iOS
 *  `CatalogStore.country(code:)`. */
fun List<Country>.selected(code: String?): Country =
    withCode(Country.normalizeCode(code)) ?: Country.byCode(code)

/** Whether an in-app country switcher is worth showing (more than one deployed
 *  country). With one there's nothing to switch to. */
val List<Country>.isSwitchable: Boolean get() = size > 1

/** A country's subtitled/dubbed `Showtime.format` pair — see
 *  [Country.versionTokens]. Its wire shape is the catalog's `versionTokens`
 *  object verbatim, so it decodes straight off the country row. */
@kotlinx.serialization.Serializable
data class VersionTokens(val subtitled: String, val dubbed: String) {
    /** The values a `?lang=` deep-link parameter may take for this country. */
    val accepted: Set<String> get() = setOf(subtitled, dubbed)

    companion object {
        /** Poland's pair — the historical hardcoded one, and the fallback for a
         *  catalog row that omits the field. */
        val POLAND = VersionTokens(subtitled = "NAP", dubbed = "DUB")
    }
}

/** Wire shape of one country in the `/api/catalog` payload (`{code,name,baseUrl,
 *  language,brand,timezone,versionTokens}`). Decoded then mapped to [Country];
 *  `brand` is ignored. */
@kotlinx.serialization.Serializable
data class CountryDto(
    val code: String,
    val name: String,
    val baseUrl: String,
    val language: String,
    /** IANA zone id, e.g. `Europe/London`. Nullable so an older bundled seed (or
     *  a server predating the field) still decodes — it then falls back to
     *  Warsaw, exactly the pre-fix behaviour. */
    val timezone: String? = null,
    /** `{subtitled,dubbed}` — the country's version-filter pair. Nullable for
     *  the same reason as [timezone]: a cached catalog that predates the field
     *  still decodes, and then gets Poland's pair, exactly what the app
     *  hardcoded before. */
    val versionTokens: VersionTokens? = null,
) {
    fun toCountry(): Country = Country(
        code = code,
        displayName = name,
        baseUrl = baseUrl,
        languageTag = language,
        zoneId = timezone?.let { runCatching { ZoneId.of(it) }.getOrNull() } ?: WARSAW_ZONE,
        versionTokens = versionTokens ?: VersionTokens.POLAND,
    )
}
