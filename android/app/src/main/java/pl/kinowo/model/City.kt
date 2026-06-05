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
 * The (currently single) catalogue of supported cities, plus the location →
 * city resolution used by the first-launch gate. New cities are added by
 * extending [all]; nothing else here changes.
 */
object Cities {
    val all: List<City> = listOf(
        City("poznan", "Poznań", 52.4064, 16.9252),
        City("wroclaw", "Wrocław", 51.1079, 17.0385),
        City("warszawa", "Warszawa", 52.2297, 21.0122),
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
