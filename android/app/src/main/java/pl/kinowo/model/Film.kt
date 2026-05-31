package pl.kinowo.model

import kotlinx.serialization.Serializable

/**
 * Listing model — decoded straight off `GET /api/repertoire`, whose JSON
 * is emitted by the server's `MovieController.apiRepertoire`. Field names
 * match the wire shape 1:1 (the iOS `Film` Codable decodes the same bytes),
 * so no `@SerialName` remaps are needed.
 *
 * URL-bearing fields are kept as `String?` rather than a parsed URI type:
 * the server occasionally ships poster links to images that 404, and we'd
 * rather pass the raw string to Coil / an Intent and let it fail at fetch
 * time than drop the field at decode time.
 */
@Serializable
data class Film(
    val title: String,
    val posterURL: String? = null,
    /** Alternative poster URLs in source-priority order, tried in turn when
     *  `posterURL` fails to load (cinema CDNs intermittently 403/404). */
    val fallbackPosterURLs: List<String> = emptyList(),
    val runtimeMinutes: Int? = null,
    val ratings: Ratings = Ratings.EMPTY,
    val countries: List<String> = emptyList(),
    val directors: List<String> = emptyList(),
    val cast: List<String> = emptyList(),
    val showings: List<DayShowings> = emptyList(),
) {
    /** Title is the stable identity (matches iOS `Film.id`). */
    val id: String get() = title

    /** Full poster chain: primary first, then declared fallbacks. */
    val posterChain: List<String>
        get() = (listOfNotNull(posterURL) + fallbackPosterURLs)

    /** Per-cinema external film-page links for the detail screen, derived
     *  from the showings tree (each cinema-group already carries its
     *  `cinemaURL`). Deduped by cinema, ordered alphabetically by display
     *  name — matches what the web `/film` page emitted as `cinema-link`
     *  buttons, so no scraping is needed. */
    val cinemaLinks: List<CinemaLink>
        get() = showings
            .asSequence()
            .flatMap { it.cinemas.asSequence() }
            .mapNotNull { cg -> cg.cinemaURL?.let { CinemaLink(cg.cinema, it) } }
            .distinctBy { it.cinema }
            .sortedBy { it.cinema }
            .toList()
}

/** A cinema's external film-page link, shown as a pill on the detail screen. */
data class CinemaLink(val cinema: String, val url: String)

@Serializable
data class Ratings(
    val imdb: Double? = null,
    val imdbURL: String? = null,
    val metascore: Int? = null,
    val metacriticURL: String? = null,
    val rottenTomatoes: Int? = null,
    val rottenTomatoesURL: String? = null,
    val filmweb: Double? = null,
    val filmwebURL: String? = null,
) {
    val isEmpty: Boolean
        get() = imdb == null && metascore == null && rottenTomatoes == null && filmweb == null

    companion object {
        val EMPTY = Ratings()
    }
}

@Serializable
data class DayShowings(
    /** `YYYY-MM-DD`, comparable as a string thanks to ISO layout. */
    val date: String,
    /** Polish day-and-date label verbatim from the server (e.g. "Czwartek 21 maja"). */
    val label: String,
    val cinemas: List<CinemaShowings>,
)

@Serializable
data class CinemaShowings(
    val cinema: String,
    val cinemaURL: String? = null,
    val showtimes: List<Showtime>,
)

@Serializable
data class Showtime(
    /** `HH:MM`. */
    val time: String,
    /** Space-separated format tokens ("2D NAP", "IMAX 3D", …). */
    val format: String = "",
    val room: String? = null,
    val bookingURL: String? = null,
) {
    val id: String get() = "$time|$format|${bookingURL ?: ""}"
}
