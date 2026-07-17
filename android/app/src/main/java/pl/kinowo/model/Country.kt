package pl.kinowo.model

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
) {
    companion object {
        /** Compile-time FALLBACK registry, used only until the bundled/fetched
         *  catalog loads (and if it ever fails to decode). The live registry is
         *  the `/api/catalog` payload the catalog repository publishes. Poland is
         *  the default. Codes match the server (`pl`/`uk`). */
        val all: List<Country> = listOf(
            Country(
                code = "pl",
                displayName = "Polska",
                baseUrl = "https://kinowo.fly.dev",
                languageTag = "pl",
            ),
            Country(
                code = "uk",
                displayName = "United Kingdom",
                baseUrl = "https://showtimes-uk.fly.dev",
                languageTag = "en",
            ),
        )

        val default: Country = all.first()

        /** The country for [code] in the fallback registry, or [default] when
         *  null / unknown. Bootstrap only; live lookups use the catalog list. */
        fun byCode(code: String?): Country = all.firstOrNull { it.code == normalizeCode(code) } ?: default

        /** Map a legacy persisted selection code to the current server code space.
         *  Earlier builds stored ISO codes (`PL`/`GB`); the catalog keys on
         *  `pl`/`uk`. Applied wherever a persisted code is read so an upgrade keeps
         *  the user's country without a migration write. */
        fun normalizeCode(code: String?): String? = when (code) {
            "PL" -> "pl"
            "GB" -> "uk"
            else -> code
        }
    }
}

/** Registry lookups over a catalog's country list — the live list the catalog
 *  repository holds, so a country added server-side appears without an app update. */
fun List<Country>.withCode(code: String?): Country? = firstOrNull { it.code == code }

/** Whether an in-app country switcher is worth showing (more than one deployed
 *  country). With one there's nothing to switch to. */
val List<Country>.isSwitchable: Boolean get() = size > 1

/** Wire shape of one country in the `/api/catalog` payload (`{code,name,baseUrl,
 *  language,brand}`). Decoded then mapped to [Country]; `brand` is ignored. */
@kotlinx.serialization.Serializable
data class CountryDto(
    val code: String,
    val name: String,
    val baseUrl: String,
    val language: String,
) {
    fun toCountry(): Country = Country(code = code, displayName = name, baseUrl = baseUrl, languageTag = language)
}
