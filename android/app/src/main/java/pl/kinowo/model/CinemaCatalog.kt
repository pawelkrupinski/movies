package pl.kinowo.model

import kotlinx.serialization.Serializable

/**
 * A city's cinema universe plus its optional area grouping, decoded off
 * `GET /:city/api/cinemas` (mirrors the web `ApiCityCinemas` / iOS
 * `CinemaCatalog`). A **flat** city returns an empty [areas]; a **split** city
 * (e.g. London) returns one entry per area, together partitioning [cinemas].
 * Android shows the multi-select area picker only when [isSplit] — flat cities
 * keep the single-select pill bar.
 */
@Serializable
data class CinemaCatalog(
    /** Every venue in the city, in city order (the full universe, including
     *  venues with no showings today so the picker is stable). */
    val cinemas: List<String> = emptyList(),
    /** The area grouping, or empty for a flat city. */
    val areas: List<CinemaArea> = emptyList(),
    /** The city's seasonal venues (open-air, drive-in) — absent from the payload
     *  when it has none, and from servers older than the field. */
    val seasons: List<CinemaSeason> = emptyList(),
) {
    val isSplit: Boolean get() = areas.isNotEmpty()

    /** The season of [cinema], when it is a seasonal venue. */
    fun seasonOf(cinema: String): CinemaSeason? = seasons.firstOrNull { it.cinema == cinema }

    /** Whether [cinema] is out of season today — the SERVER's verdict, on its own
     *  clock, so the app never disagrees with the web about it. */
    fun isOffSeason(cinema: String): Boolean = seasonOf(cinema)?.offSeason ?: false

    /** [cinemas] with the out-of-season ones moved to the bottom, order otherwise
     *  kept — how the filter lists a flat city, and each area of a split one. */
    fun seasonOrdered(cinemas: List<String>): List<String> =
        cinemas.filterNot(::isOffSeason) + cinemas.filter(::isOffSeason)

    /** The cinemas to EXCLUDE when only the areas in [keepingAreas] (slugs) are
     *  kept — every cinema in an unchecked area. Drives the first-visit area
     *  picker: checked areas stay shown, the rest go into `disabledCinemas`.
     *  Areas are a partition, so this is unambiguous. */
    fun cinemasToDisable(keepingAreas: Set<String>): List<String> =
        areas.filterNot { it.slug in keepingAreas }.flatMap { it.cinemas }

    companion object {
        val EMPTY = CinemaCatalog()
    }
}

/**
 * One area group in a split city: display name, stable slug, and the full
 * display names of the venues it holds. Mirrors web `ApiCinemaArea`.
 */
@Serializable
data class CinemaArea(
    val name: String,
    val slug: String,
    val cinemas: List<String> = emptyList(),
)

/**
 * A seasonal venue's typical window, months 1..12 both inclusive, and whether
 * today is outside it (decided by the server). Mirrors web `ApiCinemaSeason`.
 */
@Serializable
data class CinemaSeason(
    val cinema: String,
    val opens: Int,
    val closes: Int,
    val offSeason: Boolean,
) {
    /** The filter row's muted second line, "back in season · May–Sept", in the UI
     *  [language] ("pl", "en", "de", "es"; anything else reads English). */
    fun backInSeasonLine(language: String): String {
        val words = SeasonWords.of(language)
        return "${words.back} · ${words.months[opens - 1]}–${words.months[closes - 1]}"
    }
}

/** The words a season is spelled in, per UI language — kept identical to the web's
 *  `season.back` / `season.month.N` messages and iOS's `SeasonWords`. */
data class SeasonWords(val back: String, val months: List<String>) {
    companion object {
        fun of(language: String): SeasonWords = when (language) {
            "pl" -> SeasonWords("wraca w sezonie",
                listOf("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paź", "lis", "gru"))
            "de" -> SeasonWords("öffnet wieder",
                listOf("Jan.", "Feb.", "März", "Apr.", "Mai", "Juni", "Juli", "Aug.", "Sept.", "Okt.", "Nov.", "Dez."))
            "es" -> SeasonWords("vuelve en temporada",
                listOf("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sept", "oct", "nov", "dic"))
            else -> SeasonWords("back in season",
                listOf("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
        }
    }
}
