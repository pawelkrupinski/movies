package pl.kinowo.deeplink

import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Cities
import java.net.URI
import java.net.URLDecoder

/**
 * A parsed deep link into the app — the Android counterpart of iOS `DeepLink`.
 *
 * The grammar mirrors the web URLs one-for-one so the SAME `kinowo.fly.dev`
 * links open the app via App Links — including the copy-to-clipboard filter
 * links, whose query string decodes back into [DeepLinkFilters]. The `kinowo://`
 * custom scheme is accepted too (host = city slug).
 *
 *   https://kinowo.fly.dev/poznan/                       → city
 *   https://kinowo.fly.dev/poznan/?dim=2D&genre=Komedia   → city + filters
 *   https://kinowo.fly.dev/poznan/film?title=Oppenheimer  → city + film detail
 *   kinowo://poznan/                                      → city (custom scheme)
 *   kinowo://poznan/film?title=Oppenheimer                → film (custom scheme)
 *
 * Parsed with [java.net.URI] (not `android.net.Uri`) so it's a pure function,
 * unit-tested on the JVM in `testDebugUnitTest` without Robolectric.
 */
data class DeepLink(
    val citySlug: String,
    /** Set for `/:city/film?title=…`; the title is the film's identity, matched
     *  against the loaded repertoire once it arrives. */
    val filmTitle: String?,
    val filters: DeepLinkFilters,
) {
    companion object {
        private val WEB_HOSTS = setOf("kinowo.fly.dev", "www.kinowo.fly.dev")
        /** Reserved custom-scheme host (the OAuth callback) — never a city. */
        private val RESERVED_SCHEME_HOSTS = setOf("auth-done")

        /**
         * Parse an App Link / custom-scheme URL into a destination, or `null`
         * when it isn't a recognisable city link (OAuth callback, unknown host,
         * unknown city). Returning `null` lets the caller no-op rather than
         * navigate somewhere wrong.
         */
        fun parse(
            url: String,
            knownCitySlugs: Set<String> = Cities.all.map { it.slug }.toSet(),
        ): DeepLink? {
            // Split the query (and any fragment) off the raw string BEFORE
            // handing the base to java.net.URI. The single-arg URI(String)
            // constructor is strict — it throws URISyntaxException on a literal
            // space or other un-percent-encoded character. MIUI/Xiaomi delivers
            // App Links with the query already percent-DECODED (so
            // `?title=Minionki i straszydła` arrives with literal spaces), which
            // made the strict parse throw → parse returned null → the film page
            // never opened. We parse the query ourselves below; parseQuery
            // decodes %XX and leaves already-literal chars intact, so it handles
            // both the encoded and the decoded delivery. The base (scheme / host
            // / path: a city slug + optional "film") is always ASCII here, so
            // URI parses it fine.
            val withoutFragment = url.substringBefore("#")
            val base = withoutFragment.substringBefore("?")
            val rawQuery = if (withoutFragment.contains("?")) withoutFragment.substringAfter("?") else null

            val uri = try {
                URI(base)
            } catch (_: Exception) {
                return null
            }
            val scheme = uri.scheme?.lowercase() ?: return null
            val host = uri.host?.lowercase() ?: return null

            val city: String
            val trailing: List<String>
            when (scheme) {
                "https", "http" -> {
                    if (host !in WEB_HOSTS) return null
                    val segments = pathSegments(uri.rawPath)
                    city = segments.firstOrNull() ?: return null
                    trailing = segments.drop(1)
                }
                "kinowo" -> {
                    if (host in RESERVED_SCHEME_HOSTS) return null
                    city = host
                    trailing = pathSegments(uri.rawPath)
                }
                else -> return null
            }

            if (city !in knownCitySlugs) return null

            val query = parseQuery(rawQuery)
            val filmTitle = if (trailing.firstOrNull() == "film") {
                query.firstOrNull { it.first == "title" }?.second?.takeIf { it.isNotEmpty() }
            } else null

            return DeepLink(city, filmTitle, DeepLinkFilters.from(query))
        }

        private fun pathSegments(rawPath: String?): List<String> =
            rawPath.orEmpty().split("/").filter { it.isNotEmpty() }.map(::decode)

        /** Decoded (name, value) pairs, repeats preserved (the multi-value
         *  filters rely on order/repetition). */
        private fun parseQuery(rawQuery: String?): List<Pair<String, String>> =
            rawQuery.orEmpty().split("&").filter { it.isNotEmpty() }.map { pair ->
                val i = pair.indexOf('=')
                if (i < 0) decode(pair) to "" else decode(pair.substring(0, i)) to decode(pair.substring(i + 1))
            }

        /** Percent-decode, preserving a literal `+` (the web encodes spaces as
         *  `%20`, never `+`, so matching iOS's URLComponents — which leaves `+`
         *  alone — keeps a title containing `+` intact). */
        private fun decode(s: String): String =
            URLDecoder.decode(s.replace("+", "%2B"), "UTF-8")
    }
}

/**
 * The filter state a link carries, decoded from the query the web's
 * `buildShareURL()` emits. Scalar axes map straight onto the app's filter
 * state; the multi-value axes (country/genre/director/cast) and `cinema` are
 * stored as the web emits them — the INCLUSION set of values to KEEP — and
 * converted to the app's exclusion model via [excluded] once the repertoire
 * (hence the value universe) is known.
 */
data class DeepLinkFilters(
    val date: DateFilter? = null,
    val query: String? = null,
    val dimension: String? = null,   // "2D" | "3D"
    val language: String? = null,    // "NAP" | "DUB"
    val imax: Boolean? = null,
    val fromHour: Int? = null,
    val fromMinute: Int? = null,
    val sort: SortOption? = null,
    val includedCountries: List<String> = emptyList(),
    val includedGenres: List<String> = emptyList(),
    val includedDirectors: List<String> = emptyList(),
    val includedCast: List<String> = emptyList(),
    /** `null` = the `cinema` param was absent (leave the user's cinema choice
     *  alone); non-null = the explicit set of ENABLED cinemas to keep. */
    val enabledCinemas: List<String>? = null,
) {
    val isEmpty: Boolean get() = this == EMPTY

    /** The [FormatFilter] these scalar axes describe. Absent axes fall back to
     *  [base] so a `?dim=2D` link doesn't wipe an unrelated axis. */
    fun formatFilter(base: FormatFilter = FormatFilter.EMPTY): FormatFilter =
        base.copy(
            dimension = dimension ?: base.dimension,
            language = language ?: base.language,
            imax = imax ?: base.imax,
            fromHour = fromHour ?: base.fromHour,
            fromMinute = fromHour?.let { fromMinute ?: 0 } ?: base.fromMinute,
        )

    /** Convert one INCLUSION list (values to keep) into the app's EXCLUSION set
     *  given the full universe of values in the loaded repertoire. An empty
     *  keep-list means "no constraint" → no exclusions. */
    fun excluded(included: List<String>, universe: Set<String>): Set<String> =
        if (included.isEmpty()) emptySet() else universe - included.toSet()

    /** Cinemas to DISABLE: every known cinema not in the enabled list. `null`
     *  when the `cinema` param was absent (don't touch the choice). */
    fun disabledCinemas(allCinemas: Set<String>): Set<String>? =
        enabledCinemas?.let { allCinemas - it.toSet() }

    companion object {
        val EMPTY = DeepLinkFilters()

        private val ISO_DATE = Regex("""^\d{4}-\d{2}-\d{2}$""")

        fun from(query: List<Pair<String, String>>): DeepLinkFilters {
            fun first(key: String): String? =
                query.firstOrNull { it.first == key }?.second?.takeIf { it.isNotEmpty() }
            // Repeated params AND legacy comma-lists both flatten to one set.
            fun all(key: String): List<String> =
                query.filter { it.first == key }.flatMap { it.second.split(",") }.filter { it.isNotEmpty() }

            val from = first("from")?.let(::parseFrom)
            return DeepLinkFilters(
                date = first("date")?.let(::parseDate),
                query = first("q"),
                dimension = first("dim")?.takeIf { it == "2D" || it == "3D" },
                language = first("lang")?.takeIf { it == "NAP" || it == "DUB" },
                imax = if (query.any { it.first == "imax" }) first("imax") == "1" else null,
                fromHour = from?.first,
                fromMinute = from?.second,
                sort = first("sort")?.let { v -> SortOption.entries.firstOrNull { it.name.equals(v, ignoreCase = true) } },
                includedCountries = all("country"),
                includedGenres = all("genre"),
                includedDirectors = all("director"),
                includedCast = all("cast"),
                enabledCinemas = if (query.any { it.first == "cinema" }) all("cinema") else null,
            )
        }

        private fun parseDate(value: String): DateFilter? = when (value) {
            "today" -> DateFilter.Today
            "tomorrow" -> DateFilter.Tomorrow
            "week" -> DateFilter.Week
            "anytime" -> DateFilter.Anytime
            else -> if (ISO_DATE.matches(value)) DateFilter.Specific(value) else null
        }

        private fun parseFrom(value: String): Pair<Int, Int>? {
            val parts = value.split(":")
            if (parts.size != 2) return null
            val h = parts[0].toIntOrNull() ?: return null
            val m = parts[1].toIntOrNull() ?: return null
            return if (h in 0..23 && m in 0..59) h to m else null
        }
    }
}
