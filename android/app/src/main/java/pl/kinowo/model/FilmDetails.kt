package pl.kinowo.model

import kotlinx.serialization.Serializable

/**
 * Detail-only payload from `GET /api/details`, fetched in parallel with the
 * listing and merged by [title]. Carries the two fields the grid never needs —
 * the plot synopsis and ready-to-embed trailer URLs — kept off the
 * latency-sensitive listing fetch. Only films with a synopsis or a trailer are
 * present, so a missing entry just means "no extra detail".
 */
@Serializable
data class FilmDetails(
    val title: String,
    // Original (production-language) title — e.g. the English name of a film
    // listed under a Polish cinema title. The backend only sends it when it
    // genuinely differs from [title], so it can be shown verbatim.
    val originalTitle: String? = null,
    val synopsis: String? = null,
    val trailerURLs: List<String> = emptyList(),
)
