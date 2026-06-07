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
 * The catalogue of supported cities, plus the location → city resolution used
 * by the first-launch gate. [all] mirrors the web `City.all` ordering so the
 * "Miasto" picker reads identically across platforms; new cities are added by
 * extending it, nothing else here changes.
 */
object Cities {
    val all: List<City> = listOf(
        City("poznan", "Poznań", 52.4064, 16.9252),
        City("wroclaw", "Wrocław", 51.1079, 17.0385),
        City("warszawa", "Warszawa", 52.2297, 21.0122),
        City("krakow", "Kraków", 50.0647, 19.9450),
        City("lodz", "Łódź", 51.7592, 19.4560),
        City("katowice", "Katowice", 50.2649, 19.0238),
        City("szczecin", "Szczecin", 53.4285, 14.5528),
        City("bialystok", "Białystok", 53.1325, 23.1688),
        City("trojmiasto", "Trójmiasto", 54.4416, 18.5601),
        City("bydgoszcz", "Bydgoszcz", 53.1235, 18.0084),
        City("lublin", "Lublin", 51.2465, 22.5684),
        City("czestochowa", "Częstochowa", 50.8118, 19.1203),
        City("radom", "Radom", 51.4027, 21.1471),
        City("sosnowiec", "Sosnowiec", 50.2863, 19.1041),
        City("torun", "Toruń", 53.0138, 18.5984),
        City("kielce", "Kielce", 50.8661, 20.6286),
        City("rzeszow", "Rzeszów", 50.0413, 21.9990),
        City("gliwice", "Gliwice", 50.2945, 18.6714),
        City("zabrze", "Zabrze", 50.3249, 18.7857),
        // The 22 mid-size cities that round the catalogue out to 41. They serve
        // an empty repertoire until the worker wires their venues, but appear in
        // the nearest-city pick + "Miasto" picker now (mirrors web `City.all`).
        City("olsztyn", "Olsztyn", 53.7784, 20.4801),
        City("bielsko-biala", "Bielsko-Biała", 49.8224, 19.0584),
        City("opole", "Opole", 50.6751, 17.9213),
        City("rybnik", "Rybnik", 50.0971, 18.5416),
        City("gorzow-wielkopolski", "Gorzów Wielkopolski", 52.7368, 15.2288),
        City("elblag", "Elbląg", 54.1522, 19.4088),
        City("koszalin", "Koszalin", 54.1943, 16.1722),
        City("kalisz", "Kalisz", 51.7611, 18.0911),
        City("zielona-gora", "Zielona Góra", 51.9356, 15.5062),
        City("tychy", "Tychy", 50.1357, 18.9985),
        City("walbrzych", "Wałbrzych", 50.7714, 16.2845),
        City("tarnow", "Tarnów", 50.0121, 20.9858),
        City("wloclawek", "Włocławek", 52.6483, 19.0677),
        City("legnica", "Legnica", 51.2070, 16.1619),
        City("plock", "Płock", 52.5468, 19.7064),
        City("bytom", "Bytom", 50.3483, 18.9157),
        City("dabrowa-gornicza", "Dąbrowa Górnicza", 50.3219, 19.1876),
        City("nowy-sacz", "Nowy Sącz", 49.6175, 20.7154),
        City("slupsk", "Słupsk", 54.4641, 17.0287),
        City("jelenia-gora", "Jelenia Góra", 50.9044, 15.7197),
        City("przemysl", "Przemyśl", 49.7838, 22.7677),
        City("konin", "Konin", 52.2230, 18.2511),
    )

    /**
     * [all] ordered alphabetically by display name under Polish collation, so
     * the UI city pickers read A→Z with `Ł` after `L`, `Ó` after `O`, etc.
     * rather than dumping the diacritic letters at the end (code-point order).
     * This is the list the pickers iterate; [all] keeps its hand-tuned order
     * for [DEFAULT] and the nearest-city pick, where order is semantic.
     */
    val allSorted: List<City> = run {
        val collator = java.text.Collator.getInstance(java.util.Locale("pl", "PL"))
        all.sortedWith(compareBy(collator) { it.name })
    }

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
