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
        City("poznan", "Poznań", 52.4064, 16.9252, "PL"),
        City("wroclaw", "Wrocław", 51.1079, 17.0385, "PL"),
        City("warszawa", "Warszawa", 52.2297, 21.0122, "PL"),
        City("krakow", "Kraków", 50.0647, 19.9450, "PL"),
        City("lodz", "Łódź", 51.7592, 19.4560, "PL"),
        City("katowice", "Katowice", 50.2649, 19.0238, "PL"),
        City("szczecin", "Szczecin", 53.4285, 14.5528, "PL"),
        City("bialystok", "Białystok", 53.1325, 23.1688, "PL"),
        City("trojmiasto", "Trójmiasto", 54.4416, 18.5601, "PL"),
        City("bydgoszcz", "Bydgoszcz", 53.1235, 18.0084, "PL"),
        City("lublin", "Lublin", 51.2465, 22.5684, "PL"),
        City("czestochowa", "Częstochowa", 50.8118, 19.1203, "PL"),
        City("radom", "Radom", 51.4027, 21.1471, "PL"),
        City("sosnowiec", "Sosnowiec", 50.2863, 19.1041, "PL"),
        City("torun", "Toruń", 53.0138, 18.5984, "PL"),
        City("kielce", "Kielce", 50.8661, 20.6286, "PL"),
        City("rzeszow", "Rzeszów", 50.0413, 21.9990, "PL"),
        City("gliwice", "Gliwice", 50.2945, 18.6714, "PL"),
        City("zabrze", "Zabrze", 50.3249, 18.7857, "PL"),
        City("olsztyn", "Olsztyn", 53.7784, 20.4801, "PL"),
        City("bielsko-biala", "Bielsko-Biała", 49.8224, 19.0584, "PL"),
        City("opole", "Opole", 50.6751, 17.9213, "PL"),
        City("rybnik", "Rybnik", 50.0971, 18.5416, "PL"),
        City("gorzow-wielkopolski", "Gorzów Wielkopolski", 52.7368, 15.2288, "PL"),
        City("elblag", "Elbląg", 54.1522, 19.4088, "PL"),
        City("koszalin", "Koszalin", 54.1943, 16.1722, "PL"),
        City("kalisz", "Kalisz", 51.7611, 18.0911, "PL"),
        City("zielona-gora", "Zielona Góra", 51.9356, 15.5062, "PL"),
        City("tychy", "Tychy", 50.1357, 18.9985, "PL"),
        City("walbrzych", "Wałbrzych", 50.7714, 16.2845, "PL"),
        City("tarnow", "Tarnów", 50.0121, 20.9858, "PL"),
        City("wloclawek", "Włocławek", 52.6483, 19.0677, "PL"),
        City("legnica", "Legnica", 51.2070, 16.1619, "PL"),
        City("plock", "Płock", 52.5468, 19.7064, "PL"),
        City("bytom", "Bytom", 50.3483, 18.9157, "PL"),
        City("dabrowa-gornicza", "Dąbrowa Górnicza", 50.3219, 19.1876, "PL"),
        City("nowy-sacz", "Nowy Sącz", 49.6175, 20.7154, "PL"),
        City("slupsk", "Słupsk", 54.4641, 17.0287, "PL"),
        City("jelenia-gora", "Jelenia Góra", 50.9044, 15.7197, "PL"),
        City("przemysl", "Przemyśl", 49.7838, 22.7677, "PL"),
        City("konin", "Konin", 52.2230, 18.2511, "PL"),
        // ── United Kingdom (79 Flicks regions; English labels). ──────────────
        City("london", "London", 51.5074, -0.1278, "GB"),
        City("manchester", "Manchester", 53.4808, -2.2426, "GB"),
        City("norwich", "Norwich", 52.6309, 1.2974, "GB"),
        City("aberdeenshire", "Aberdeenshire", 57.308, -2.3393, "GB"),
        City("antrim", "Antrim", 54.762, -6.0127, "GB"),
        City("armagh", "Armagh", 54.4492, -6.398, "GB"),
        City("ayrshire-and-arran", "Ayrshire and Arran", 55.5093, -4.581, "GB"),
        City("bedfordshire", "Bedfordshire", 52.0082, -0.4435, "GB"),
        City("belfast", "Belfast", 54.5857, -5.9428, "GB"),
        City("berkshire", "Berkshire", 51.4268, -0.9169, "GB"),
        City("birmingham", "Birmingham", 52.4581, -1.9041, "GB"),
        City("bristol", "Bristol", 51.4659, -2.5805, "GB"),
        City("buckinghamshire", "Buckinghamshire", 51.7582, -0.7609, "GB"),
        City("cambridgeshire", "Cambridgeshire", 52.4301, -0.0137, "GB"),
        City("cardiff", "Cardiff", 51.4892, -3.1939, "GB"),
        City("central-scotland", "Central Scotland", 56.08, -3.8066, "GB"),
        City("cheshire", "Cheshire", 53.2917, -2.4966, "GB"),
        City("clwyd", "Clwyd", 53.3083, -3.6072, "GB"),
        City("cornwall", "Cornwall", 50.317, -4.9211, "GB"),
        City("county-durham", "County Durham", 54.7289, -1.5139, "GB"),
        City("cumbria", "Cumbria", 54.4593, -3.1119, "GB"),
        City("derbyshire", "Derbyshire", 52.9886, -1.5219, "GB"),
        City("devon", "Devon", 50.6651, -3.687, "GB"),
        City("dorset", "Dorset", 50.7664, -2.1122, "GB"),
        City("down", "Down", 54.4293, -5.9704, "GB"),
        City("dudley", "Dudley", 52.497, -2.0918, "GB"),
        City("dumfries-and-galloway", "Dumfries and Galloway", 54.9881, -3.8232, "GB"),
        City("dunbartonshire-argyll-bute", "Dunbartonshire and Argyll & Bute", 55.7795, -4.9973, "GB"),
        City("dyfed", "Dyfed", 51.9892, -4.3329, "GB"),
        City("east-sussex", "East Sussex", 50.8499, 0.2215, "GB"),
        City("east-yorkshire", "East Yorkshire", 53.8685, -0.3985, "GB"),
        City("edinburgh-and-lothians", "Edinburgh & Lothians", 55.9404, -3.2039, "GB"),
        City("essex", "Essex", 51.7621, 0.5901, "GB"),
        City("fermanagh", "Fermanagh", 54.3499, -7.6316, "GB"),
        City("fife", "Fife", 56.1287, -3.2424, "GB"),
        City("glamorgan", "Glamorgan", 51.6388, -3.7535, "GB"),
        City("glasgow", "Glasgow", 55.8682, -4.2316, "GB"),
        City("gloucestershire", "Gloucestershire", 51.8387, -2.2712, "GB"),
        City("guernsey", "Guernsey", 49.4446, -2.5695, "GB"),
        City("gwent", "Gwent", 51.6882, -3.0066, "GB"),
        City("gwynedd", "Gwynedd", 53.0098, -4.153, "GB"),
        City("hampshire", "Hampshire", 50.9234, -1.165, "GB"),
        City("herefordshire", "Herefordshire", 52.031, -2.7825, "GB"),
        City("hertfordshire", "Hertfordshire", 51.7791, -0.3102, "GB"),
        City("highlands-and-islands", "Highlands and Islands", 58.086, -4.0855, "GB"),
        City("isle-of-man", "Isle of Man", 54.1578, -4.4775, "GB"),
        City("isle-of-wight", "Isle of Wight", 50.7118, -1.2248, "GB"),
        City("jersey", "Jersey", 49.1839, -2.1144, "GB"),
        City("kent", "Kent", 51.2682, 0.8631, "GB"),
        City("lanarkshire", "Lanarkshire", 55.7953, -4.0904, "GB"),
        City("lancashire", "Lancashire", 53.7367, -2.6625, "GB"),
        City("leicestershire", "Leicestershire", 52.6656, -1.1514, "GB"),
        City("lincolnshire", "Lincolnshire", 53.2194, -0.2916, "GB"),
        City("londonderry", "Londonderry", 54.9949, -7.0636, "GB"),
        City("liverpool", "Liverpool", 53.4084, -2.9916, "GB"),
        City("north-yorkshire", "North Yorkshire", 54.2402, -1.156, "GB"),
        City("northamptonshire", "Northamptonshire", 52.288, -0.8653, "GB"),
        City("northumberland", "Northumberland", 55.2158, -1.7422, "GB"),
        City("nottinghamshire", "Nottinghamshire", 53.0236, -1.15, "GB"),
        City("oxfordshire", "Oxfordshire", 51.7572, -1.2545, "GB"),
        City("powys", "Powys", 52.3806, -3.26, "GB"),
        City("renfrewshire", "Renfrewshire", 55.9204, -4.5838, "GB"),
        City("roxburgh-ettrick-and-lauderdale", "Roxburgh, Ettrick and Lauderdale", 55.5183, -2.7969, "GB"),
        City("sandwell", "Sandwell", 52.5175, -1.9932, "GB"),
        City("shropshire", "Shropshire", 52.6813, -2.6215, "GB"),
        City("somerset", "Somerset", 51.2159, -2.824, "GB"),
        City("south-yorkshire", "South Yorkshire", 53.5141, -1.3109, "GB"),
        City("staffordshire", "Staffordshire", 52.7942, -1.9887, "GB"),
        City("suffolk", "Suffolk", 52.1492, 1.0262, "GB"),
        City("surrey", "Surrey", 51.2269, -0.5354, "GB"),
        City("tayside", "Tayside", 56.5061, -3.0128, "GB"),
        City("tyne-and-wear", "Tyne and Wear", 54.9749, -1.5397, "GB"),
        City("tyrone", "Tyrone", 54.5255, -6.8664, "GB"),
        City("warwickshire", "Warwickshire", 52.3602, -1.5034, "GB"),
        City("west-sussex", "West Sussex", 50.9492, -0.3262, "GB"),
        City("west-yorkshire", "West Yorkshire", 53.7878, -1.665, "GB"),
        City("wiltshire", "Wiltshire", 51.2955, -1.8505, "GB"),
        City("worcestershire", "Worcestershire", 52.1923, -2.2079, "GB"),
        City("yorkshire", "Yorkshire", 53.4082, -1.4756, "GB"),
    )

    /**
     * The subset of [all] belonging to [countryCode] (`"PL"`, `"GB"`), in [all]'s
     * hand-tuned order — the per-country roster the app scopes every picker and
     * the nearest-city pick to.
     */
    fun citiesIn(countryCode: String): List<City> = all.filter { it.country == countryCode }

    /**
     * [citiesIn] ordered alphabetically by display name under that country's
     * collation (Polish for PL, so `Ł` sorts after `L`, `Ó` after `O`; English
     * elsewhere). This is the list the UI pickers iterate; [all] keeps its
     * hand-tuned order for the default/nearest picks, where order is semantic.
     */
    fun sortedIn(countryCode: String): List<City> {
        val collator = java.text.Collator.getInstance(collationLocale(countryCode))
        return citiesIn(countryCode).sortedWith(compareBy(collator) { it.name })
    }

    /** Collation locale for a country's city names: Polish for PL (so the
     *  diacritic letters sort in their alphabet positions), English elsewhere. */
    private fun collationLocale(countryCode: String): java.util.Locale =
        if (countryCode == "PL") java.util.Locale("pl", "PL") else java.util.Locale("en", "GB")

    /** Ultimate fallback city — the first modelled city overall (Poznań, the
     *  default country's first). Flows that know the country pick a
     *  country-scoped default via [defaultCityIn]. */
    val DEFAULT: City = all.first()

    /** The default city for [countryCode]: the first entry of that country's
     *  hand-ordered roster (Poznań for PL, London for GB). Falls back to
     *  [DEFAULT] if the country lists no cities. */
    fun defaultCityIn(countryCode: String): City = citiesIn(countryCode).firstOrNull() ?: DEFAULT

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
    fun matching(query: String, countryCode: String): List<City> {
        val q = searchFold(query.trim())
        val cities = sortedIn(countryCode)
        return if (q.isEmpty()) cities
        else cities.filter { searchFold(it.name).contains(q) }
    }

    /**
     * The city IN [countryCode] nearest to ([lat], [lon]), or null when the
     * nearest is still farther than 100 km — i.e. the user isn't near any city
     * that country serves, so the gate falls back to an explicit pick. Scoped to
     * the country so a Polish fix never resolves to a UK region, or vice versa.
     */
    fun nearestWithin100km(lat: Double, lon: Double, countryCode: String): City? =
        citiesIn(countryCode).minByOrNull { haversineKm(lat, lon, it.lat, it.lon) }
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
        countryCode: String,
    ): CitySwitchSuggestion? {
        val nearest = nearestWithin100km(lat, lon, countryCode) ?: return null
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
    private fun haversineKm(latitude1: Double, longitude1: Double, latitude2: Double, longitude2: Double): Double {
        val earthRadiusKm = 6371.0
        val deltaLatitude = Math.toRadians(latitude2 - latitude1)
        val deltaLongitude = Math.toRadians(longitude2 - longitude1)
        val squareHalfChord = sin(deltaLatitude / 2) * sin(deltaLatitude / 2) +
            cos(Math.toRadians(latitude1)) * cos(Math.toRadians(latitude2)) *
            sin(deltaLongitude / 2) * sin(deltaLongitude / 2)
        return earthRadiusKm * 2 * atan2(sqrt(squareHalfChord), sqrt(1 - squareHalfChord))
    }
}
