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
    /** ISO 3166-1 alpha-2, e.g. `PL`, `GB`. The persisted selection key. */
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
        /** Poland is the default: the current production deployment, Polish UI. */
        val all: List<Country> = listOf(
            Country(
                code = "PL",
                displayName = "Polska",
                baseUrl = "https://kinowo.fly.dev",
                languageTag = "pl",
            ),
            // TODO(§6): swap this placeholder host for the real UK deployment URL
            // once §6 stands the English deployment up.
            Country(
                code = "GB",
                displayName = "United Kingdom",
                baseUrl = "https://kinowo.co.uk",
                languageTag = "en",
            ),
        )

        val default: Country = all.first()

        /** The country for [code], or [default] when null / unknown. */
        fun byCode(code: String?): Country = all.firstOrNull { it.code == code } ?: default
    }
}
