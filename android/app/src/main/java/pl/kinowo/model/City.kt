package pl.kinowo.model

import kotlin.math.atan2
import kotlin.math.cos
import kotlin.math.sin
import kotlin.math.sqrt

/**
 * A city the repertoire is served for. The [slug] is the URL prefix the server
 * mounts every page + API under (`/{slug}/api/repertoire`, …); [name] is the
 * display label; [lat]/[lon] place it for the nearest-city location gate.
 */
data class City(val slug: String, val name: String, val lat: Double, val lon: Double)

/**
 * A one-shot offer to switch the repertoire to a [target] city the device is
 * now nearer than the chosen one. [key] is the `chosen→nearest` pair the prompt
 * is remembered against, so we only ask once per pair.
 */
data class CitySwitchSuggestion(val target: City, val key: String)

/**
 * The (currently single) catalogue of supported cities, plus the location →
 * city resolution used by the first-launch gate. New cities are added by
 * extending [all]; nothing else here changes.
 */
object Cities {
    val all: List<City> = listOf(
        City("poznan", "Poznań", 52.4064, 16.9252),
        City("wroclaw", "Wrocław", 51.1079, 17.0385),
        City("warszawa", "Warszawa", 52.2297, 21.0122),
        City("krakow", "Kraków", 50.0647, 19.9450),
    )

    val DEFAULT: City = all.first()

    /**
     * The supported city nearest to ([lat], [lon]), or null when the nearest is
     * still farther than 100 km — i.e. the user isn't near any city we serve,
     * so the gate falls back to an explicit pick.
     */
    fun nearestWithin100km(lat: Double, lon: Double): City? =
        all.minByOrNull { haversineKm(lat, lon, it.lat, it.lon) }
            ?.takeIf { haversineKm(lat, lon, it.lat, it.lon) <= 100.0 }

    /**
     * Whether to offer switching from [chosenSlug] to a nearer supported city
     * for a device at ([lat], [lon]). Returns null — no offer — when the device
     * is out of range of every city, when the nearest is already the chosen one,
     * or when this exact `chosen→nearest` pair was the [lastPromptKey] last
     * offered (so we ask at most once per pair, but re-ask after the pair
     * changes — e.g. travelling back to a previously-declined city).
     */
    fun switchSuggestion(
        chosenSlug: String,
        lat: Double,
        lon: Double,
        lastPromptKey: String?,
    ): CitySwitchSuggestion? {
        val nearest = nearestWithin100km(lat, lon) ?: return null
        if (nearest.slug == chosenSlug) return null
        val key = switchPromptKey(chosenSlug, nearest.slug)
        if (key == lastPromptKey) return null
        return CitySwitchSuggestion(nearest, key)
    }

    /** The stable de-dupe key for a `chosen→nearest` pair. One source of truth so
     *  the value [switchSuggestion] compares against is the same one the gate
     *  pre-records via [initialChoiceSuppressKey]. */
    fun switchPromptKey(chosenSlug: String, nearestSlug: String): String =
        "$chosenSlug→$nearestSlug"

    /**
     * The prompt key to pre-record when the user *deliberately* picks
     * [chosenSlug] at the first-launch gate while location placed them nearest
     * [nearestSlug]. Seeding it stops [switchSuggestion] from immediately
     * offering to switch back to the city they just chose against — the choice
     * was intentional. Returns null when there's nothing to suppress: no
     * location fix ([nearestSlug] is null), or the chosen city *is* the nearest.
     * Only this one pair is suppressed, so travelling elsewhere re-arms the prompt.
     */
    fun initialChoiceSuppressKey(chosenSlug: String, nearestSlug: String?): String? =
        nearestSlug?.takeIf { it != chosenSlug }?.let { switchPromptKey(chosenSlug, it) }

    /** Great-circle distance in kilometres between two lat/lon points. */
    private fun haversineKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
        val earthRadiusKm = 6371.0
        val dLat = Math.toRadians(lat2 - lat1)
        val dLon = Math.toRadians(lon2 - lon1)
        val a = sin(dLat / 2) * sin(dLat / 2) +
            cos(Math.toRadians(lat1)) * cos(Math.toRadians(lat2)) *
            sin(dLon / 2) * sin(dLon / 2)
        return earthRadiusKm * 2 * atan2(sqrt(a), sqrt(1 - a))
    }
}
