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
) {
    val isSplit: Boolean get() = areas.isNotEmpty()

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
