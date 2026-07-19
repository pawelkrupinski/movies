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
@kotlinx.serialization.Serializable
data class City(
    val slug: String,
    val name: String,
    val lat: Double,
    val lon: Double,
    /**
     * ISO country code (`"PL"`, `"GB"`) matching [Country.code]. The app is
     * multi-country: every city picker, the nearest-city gate, and the default
     * city are scoped to the SELECTED country's cities, so a UK user browses UK
     * regions and a Polish user browses Polish cities — never a mix.
     */
    val country: String,
)

/**
 * A one-shot offer to switch the repertoire to a [target] city the device is
 * now nearer than the chosen one. [key] is the `chosen→nearest` pair the prompt
 * is remembered against, so we only ask once per pair.
 */
data class CitySwitchSuggestion(val target: City, val key: String)

/**
 * The catalogue of supported cities across ALL countries, plus the location →
 * city resolution used by the first-launch gate. [all] mirrors the web
 * `City.all` ordering (Polish cities first, then the UK regions) so the pickers
 * read identically across platforms. This is the GLOBAL union; the pickers and
 * the nearest-city gate scope it to the selected country via [citiesIn] /
 * [matching] / [nearestWithin100km]. A new city is added by extending [all]
 * (with its [City.country]).
 */
object Cities {
    val all: List<City> = listOf(
        City("poznan", "Poznań", 52.4064, 16.9252, "pl"),
        City("wroclaw", "Wrocław", 51.1079, 17.0385, "pl"),
        City("warszawa", "Warszawa", 52.2297, 21.0122, "pl"),
        City("krakow", "Kraków", 50.0647, 19.9450, "pl"),
        City("lodz", "Łódź", 51.7592, 19.4560, "pl"),
        City("katowice", "Katowice", 50.2649, 19.0238, "pl"),
        City("szczecin", "Szczecin", 53.4285, 14.5528, "pl"),
        City("bialystok", "Białystok", 53.1325, 23.1688, "pl"),
        City("trojmiasto", "Trójmiasto", 54.4416, 18.5601, "pl"),
        City("bydgoszcz", "Bydgoszcz", 53.1235, 18.0084, "pl"),
        City("lublin", "Lublin", 51.2465, 22.5684, "pl"),
        City("czestochowa", "Częstochowa", 50.8118, 19.1203, "pl"),
        City("radom", "Radom", 51.4027, 21.1471, "pl"),
        City("sosnowiec", "Sosnowiec", 50.2863, 19.1041, "pl"),
        City("torun", "Toruń", 53.0138, 18.5984, "pl"),
        City("kielce", "Kielce", 50.8661, 20.6286, "pl"),
        City("rzeszow", "Rzeszów", 50.0413, 21.9990, "pl"),
        City("gliwice", "Gliwice", 50.2945, 18.6714, "pl"),
        City("zabrze", "Zabrze", 50.3249, 18.7857, "pl"),
        City("olsztyn", "Olsztyn", 53.7784, 20.4801, "pl"),
        City("bielsko-biala", "Bielsko-Biała", 49.8224, 19.0584, "pl"),
        City("opole", "Opole", 50.6751, 17.9213, "pl"),
        City("rybnik", "Rybnik", 50.0971, 18.5416, "pl"),
        City("gorzow-wielkopolski", "Gorzów Wielkopolski", 52.7368, 15.2288, "pl"),
        City("elblag", "Elbląg", 54.1522, 19.4088, "pl"),
        City("koszalin", "Koszalin", 54.1943, 16.1722, "pl"),
        City("kalisz", "Kalisz", 51.7611, 18.0911, "pl"),
        City("zielona-gora", "Zielona Góra", 51.9356, 15.5062, "pl"),
        City("tychy", "Tychy", 50.1357, 18.9985, "pl"),
        City("walbrzych", "Wałbrzych", 50.7714, 16.2845, "pl"),
        City("tarnow", "Tarnów", 50.0121, 20.9858, "pl"),
        City("wloclawek", "Włocławek", 52.6483, 19.0677, "pl"),
        City("legnica", "Legnica", 51.2070, 16.1619, "pl"),
        City("plock", "Płock", 52.5468, 19.7064, "pl"),
        City("bytom", "Bytom", 50.3483, 18.9157, "pl"),
        City("dabrowa-gornicza", "Dąbrowa Górnicza", 50.3219, 19.1876, "pl"),
        City("nowy-sacz", "Nowy Sącz", 49.6175, 20.7154, "pl"),
        City("slupsk", "Słupsk", 54.4641, 17.0287, "pl"),
        City("jelenia-gora", "Jelenia Góra", 50.9044, 15.7197, "pl"),
        City("przemysl", "Przemyśl", 49.7838, 22.7677, "pl"),
        City("konin", "Konin", 52.2230, 18.2511, "pl"),
        // ── United Kingdom (79 Flicks regions; English labels). ──────────────
        City("london", "London", 51.5074, -0.1278, "uk"),
        City("manchester", "Manchester", 53.4808, -2.2426, "uk"),
        City("norwich", "Norwich", 52.6309, 1.2974, "uk"),
        City("aberdeenshire", "Aberdeenshire", 57.308, -2.3393, "uk"),
        City("antrim", "Antrim", 54.762, -6.0127, "uk"),
        City("armagh", "Armagh", 54.4492, -6.398, "uk"),
        City("ayrshire-and-arran", "Ayrshire and Arran", 55.5093, -4.581, "uk"),
        City("bedfordshire", "Bedfordshire", 52.0082, -0.4435, "uk"),
        City("belfast", "Belfast", 54.5857, -5.9428, "uk"),
        City("berkshire", "Berkshire", 51.4268, -0.9169, "uk"),
        City("birmingham", "Birmingham", 52.4581, -1.9041, "uk"),
        City("bristol", "Bristol", 51.4659, -2.5805, "uk"),
        City("buckinghamshire", "Buckinghamshire", 51.7582, -0.7609, "uk"),
        City("cambridgeshire", "Cambridgeshire", 52.4301, -0.0137, "uk"),
        City("cardiff", "Cardiff", 51.4892, -3.1939, "uk"),
        City("central-scotland", "Central Scotland", 56.08, -3.8066, "uk"),
        City("cheshire", "Cheshire", 53.2917, -2.4966, "uk"),
        City("clwyd", "Clwyd", 53.3083, -3.6072, "uk"),
        City("cornwall", "Cornwall", 50.317, -4.9211, "uk"),
        City("county-durham", "County Durham", 54.7289, -1.5139, "uk"),
        City("cumbria", "Cumbria", 54.4593, -3.1119, "uk"),
        City("derbyshire", "Derbyshire", 52.9886, -1.5219, "uk"),
        City("devon", "Devon", 50.6651, -3.687, "uk"),
        City("dorset", "Dorset", 50.7664, -2.1122, "uk"),
        City("down", "Down", 54.4293, -5.9704, "uk"),
        City("dudley", "Dudley", 52.497, -2.0918, "uk"),
        City("dumfries-and-galloway", "Dumfries and Galloway", 54.9881, -3.8232, "uk"),
        City("dunbartonshire-argyll-bute", "Dunbartonshire and Argyll & Bute", 55.7795, -4.9973, "uk"),
        City("dyfed", "Dyfed", 51.9892, -4.3329, "uk"),
        City("east-sussex", "East Sussex", 50.8499, 0.2215, "uk"),
        City("east-yorkshire", "East Yorkshire", 53.8685, -0.3985, "uk"),
        City("edinburgh-and-lothians", "Edinburgh & Lothians", 55.9404, -3.2039, "uk"),
        City("essex", "Essex", 51.7621, 0.5901, "uk"),
        City("fermanagh", "Fermanagh", 54.3499, -7.6316, "uk"),
        City("fife", "Fife", 56.1287, -3.2424, "uk"),
        City("glamorgan", "Glamorgan", 51.6388, -3.7535, "uk"),
        City("glasgow", "Glasgow", 55.8682, -4.2316, "uk"),
        City("gloucestershire", "Gloucestershire", 51.8387, -2.2712, "uk"),
        City("guernsey", "Guernsey", 49.4446, -2.5695, "uk"),
        City("gwent", "Gwent", 51.6882, -3.0066, "uk"),
        City("gwynedd", "Gwynedd", 53.0098, -4.153, "uk"),
        City("hampshire", "Hampshire", 50.9234, -1.165, "uk"),
        City("herefordshire", "Herefordshire", 52.031, -2.7825, "uk"),
        City("hertfordshire", "Hertfordshire", 51.7791, -0.3102, "uk"),
        City("highlands-and-islands", "Highlands and Islands", 58.086, -4.0855, "uk"),
        City("isle-of-man", "Isle of Man", 54.1578, -4.4775, "uk"),
        City("isle-of-wight", "Isle of Wight", 50.7118, -1.2248, "uk"),
        City("jersey", "Jersey", 49.1839, -2.1144, "uk"),
        City("kent", "Kent", 51.2682, 0.8631, "uk"),
        City("lanarkshire", "Lanarkshire", 55.7953, -4.0904, "uk"),
        City("lancashire", "Lancashire", 53.7367, -2.6625, "uk"),
        City("leicestershire", "Leicestershire", 52.6656, -1.1514, "uk"),
        City("lincolnshire", "Lincolnshire", 53.2194, -0.2916, "uk"),
        City("londonderry", "Londonderry", 54.9949, -7.0636, "uk"),
        City("liverpool", "Liverpool", 53.4084, -2.9916, "uk"),
        City("north-yorkshire", "North Yorkshire", 54.2402, -1.156, "uk"),
        City("northamptonshire", "Northamptonshire", 52.288, -0.8653, "uk"),
        City("northumberland", "Northumberland", 55.2158, -1.7422, "uk"),
        City("nottinghamshire", "Nottinghamshire", 53.0236, -1.15, "uk"),
        City("oxfordshire", "Oxfordshire", 51.7572, -1.2545, "uk"),
        City("powys", "Powys", 52.3806, -3.26, "uk"),
        City("renfrewshire", "Renfrewshire", 55.9204, -4.5838, "uk"),
        City("roxburgh-ettrick-and-lauderdale", "Roxburgh, Ettrick and Lauderdale", 55.5183, -2.7969, "uk"),
        City("sandwell", "Sandwell", 52.5175, -1.9932, "uk"),
        City("shropshire", "Shropshire", 52.6813, -2.6215, "uk"),
        City("somerset", "Somerset", 51.2159, -2.824, "uk"),
        City("south-yorkshire", "South Yorkshire", 53.5141, -1.3109, "uk"),
        City("staffordshire", "Staffordshire", 52.7942, -1.9887, "uk"),
        City("suffolk", "Suffolk", 52.1492, 1.0262, "uk"),
        City("surrey", "Surrey", 51.2269, -0.5354, "uk"),
        City("tayside", "Tayside", 56.5061, -3.0128, "uk"),
        City("tyne-and-wear", "Tyne and Wear", 54.9749, -1.5397, "uk"),
        City("tyrone", "Tyrone", 54.5255, -6.8664, "uk"),
        City("warwickshire", "Warwickshire", 52.3602, -1.5034, "uk"),
        City("west-sussex", "West Sussex", 50.9492, -0.3262, "uk"),
        City("west-yorkshire", "West Yorkshire", 53.7878, -1.665, "uk"),
        City("wiltshire", "Wiltshire", 51.2955, -1.8505, "uk"),
        City("worcestershire", "Worcestershire", 52.1923, -2.2079, "uk"),
        City("yorkshire", "Yorkshire", 53.4082, -1.4756, "uk"),
    )

    /**
     * The subset of [all] belonging to [countryCode] (`"PL"`, `"GB"`), in [all]'s
     * hand-tuned order — the per-country roster the app scopes every picker and
     * the nearest-city pick to.
     */
    // The per-country queries below delegate to the [List]<[City]> extensions so
    // the app can run the SAME logic over the live catalog's cities (the fetched
    // or seeded list the ViewModel holds), with [all] as the fallback list here.
    fun citiesIn(countryCode: String): List<City> = all.inCountry(countryCode)

    fun sortedIn(countryCode: String): List<City> = all.sortedForPicker(countryCode)

    /** Ultimate fallback city — the first bundled city overall (Poznań). Only the
     *  compile-time default before the gate sets a real city; live flows pick a
     *  country-scoped default from the catalog. */
    val DEFAULT: City = all.first()

    fun defaultCityIn(countryCode: String): City = all.defaultCity(countryCode) ?: DEFAULT

    /**
     * Fold a Polish string to its diacritic-free, lower-case form for search
     * matching, so a query typed without Polish letters still finds the city
     * ("lodz" → "Łódź", "krakow" → "Kraków"). Unicode normalisation won't fold
     * ł/ą/ę/ń, so map the Polish letters explicitly.
     */
    fun searchFold(s: String): String {
        val out = StringBuilder(s.length)
        for (ch in s.lowercase()) {
            out.append(
                when (ch) {
                    'ą' -> 'a'; 'ć' -> 'c'; 'ę' -> 'e'; 'ł' -> 'l'; 'ń' -> 'n'
                    'ó' -> 'o'; 'ś' -> 's'; 'ź' -> 'z'; 'ż' -> 'z'; else -> ch
                }
            )
        }
        return out.toString()
    }

    /**
     * [sortedIn] narrowed to the cities whose folded name contains the folded
     * [query] — case- and diacritic-insensitive substring match. A blank query
     * yields that country's whole list (an empty search box shows everything).
     * Drives the search box on the manual city picker for [countryCode].
     */
    fun matching(query: String, countryCode: String): List<City> = all.matching(query, countryCode)

    /**
     * The city IN [countryCode] nearest to ([lat], [lon]), or null when the
     * nearest is still farther than 100 km — i.e. the user isn't near any city
     * that country serves, so the gate falls back to an explicit pick. Scoped to
     * the country so a Polish fix never resolves to a UK region, or vice versa.
     */
    fun nearestWithin100km(lat: Double, lon: Double, countryCode: String): City? =
        all.nearestWithin100km(lat, lon, countryCode)

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
        countryCode: String,
    ): CitySwitchSuggestion? = all.switchSuggestion(chosenSlug, lat, lon, lastPromptKey, countryCode)

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
}

// ── Per-country queries over a catalog's city list ────────────────────────────
// `this` is the live catalog's cities — the fetched list, the bundled seed, or
// (as a fallback) [Cities.all]. Pure (no I/O) so they're unit-tested by passing a
// fixed list; the ViewModel calls them with the catalog's current cities.

/** The subset belonging to [countryCode] (`"pl"`, `"uk"`), in this list's order. */
fun List<City>.inCountry(countryCode: String): List<City> = filter { it.country == countryCode }

/** The country code of the city with [slug], or null when no such city. Lets a
 *  deep link that lands on another country's city (a showtimes-uk / showtimes-de
 *  link) switch the app to the right deployment before the repertoire loads. */
fun List<City>.countryOf(slug: String): String? = firstOrNull { it.slug == slug }?.country

/** [inCountry] ordered alphabetically under that country's collation (Polish for
 *  `pl`, so `Ł` sorts after `L`; English elsewhere) — what the pickers show. */
fun List<City>.sortedForPicker(countryCode: String): List<City> {
    val collator = java.text.Collator.getInstance(collationLocale(countryCode))
    return inCountry(countryCode).sortedWith(compareBy(collator) { it.name })
}

/** [sortedForPicker] narrowed to the cities matching [query] (case- and
 *  diacritic-insensitive substring). A blank query yields the whole country list. */
fun List<City>.matching(query: String, countryCode: String): List<City> {
    val q = Cities.searchFold(query.trim())
    val cities = sortedForPicker(countryCode)
    return if (q.isEmpty()) cities else cities.filter { Cities.searchFold(it.name).contains(q) }
}

/** The default city for [countryCode] — its first entry in this list, or null. */
fun List<City>.defaultCity(countryCode: String): City? = inCountry(countryCode).firstOrNull()

/** The city IN [countryCode] nearest ([lat], [lon]), or null beyond 100 km.
 *  Scoped so a Polish fix never resolves to a UK region, or vice versa. */
fun List<City>.nearestWithin100km(lat: Double, lon: Double, countryCode: String): City? =
    inCountry(countryCode).minByOrNull { haversineKm(lat, lon, it.lat, it.lon) }
        ?.takeIf { haversineKm(lat, lon, it.lat, it.lon) <= 100.0 }

/** The "you're nearer another city — switch?" suggestion, scoped to [countryCode];
 *  null when already nearest, out of range, or this pair was the [lastPromptKey]. */
fun List<City>.switchSuggestion(
    chosenSlug: String,
    lat: Double,
    lon: Double,
    lastPromptKey: String?,
    countryCode: String,
): CitySwitchSuggestion? {
    val nearest = nearestWithin100km(lat, lon, countryCode) ?: return null
    if (nearest.slug == chosenSlug) return null
    val key = Cities.switchPromptKey(chosenSlug, nearest.slug)
    if (key == lastPromptKey) return null
    return CitySwitchSuggestion(nearest, key)
}

/** Collation locale for a country's city names: Polish for `pl` (so the diacritic
 *  letters sort in their alphabet positions), English elsewhere. */
private fun collationLocale(countryCode: String): java.util.Locale =
    if (countryCode == "pl") java.util.Locale("pl", "PL") else java.util.Locale("en", "GB")

/** Great-circle distance in kilometres between two lat/lon points. */
private fun haversineKm(latitude1: Double, longitude1: Double, latitude2: Double, longitude2: Double): Double {
    val earthRadiusKm = 6371.0
    val deltaLatitude = Math.toRadians(latitude2 - latitude1)
    val deltaLongitude = Math.toRadians(longitude2 - longitude1)
    val squareHalfChord = sin(deltaLatitude / 2) * sin(deltaLatitude / 2) +
        cos(Math.toRadians(latitude1)) * cos(Math.toRadians(latitude2)) *
        sin(deltaLongitude / 2) * sin(deltaLongitude / 2)
    return earthRadiusKm * 2 * atan2(sqrt(squareHalfChord), sqrt(1 - squareHalfChord))
}
