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
    /**
     * The group this city is picked under, where its country groups them at all
     * — a US state ("California"), a UK nation ("Scotland") — and null in the
     * countries that group nothing. Neither 457 US metros nor 79 UK counties in
     * one A-to-Z is a list anybody reads, so the picker asks for the group
     * first; a country without regions keeps the single flat list.
     *
     * Carried by `/api/catalog` (and the bundled seed). Defaulted so a catalog
     * from an older server, or one of the hand-written [Cities.all] rows, still
     * decodes.
     */
    val region: String? = null,
    /**
     * This city's own IANA zone, where it differs from its country's; null
     * otherwise — which is every city of the four countries that keep one zone
     * throughout, so the field costs nothing there.
     *
     * The US spans SIX zones and fifteen of its states straddle a boundary, so a
     * single country zone is wrong for most of it: pruning a Knoxville showtime
     * on the country's Pacific drops it three hours early. Resolve through
     * [zoneFor], never by reading this directly.
     *
     * Carried by `/api/catalog` (and the bundled seed), same shape as [region]:
     * defaulted so a catalog from an older server, or one of the hand-written
     * [Cities.all] rows, still decodes.
     */
    val timezone: String? = null,
)

/**
 * The zone to reason about [slug]'s showtimes in — that city's own where the
 * catalog gave it one, else [fallback] (its country's).
 *
 * Also the answer for a slug this catalog does not know, which is what a stale
 * saved selection or a deep link into another deployment looks like, and for an
 * identifier the platform cannot parse. Past-showtime pruning and the day
 * buckets both go through here, so they cannot disagree.
 */
fun List<City>.zoneFor(slug: String?, fallback: java.time.ZoneId): java.time.ZoneId {
    val city = slug?.let { s -> firstOrNull { it.slug == s } } ?: return fallback
    val id = city.timezone ?: return fallback
    return runCatching { java.time.ZoneId.of(id) }.getOrDefault(fallback)
}

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
 * read identically across platforms. The data-driven rosters — Germany's 158
 * regions and the US's 55 states/territories — are NOT mirrored here: they
 * arrive with the catalog (bundled seed, then the live `/api/catalog`), so there
 * is one copy of them rather than two that drift.
 * This is the GLOBAL union; the pickers and
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
        // ── United Kingdom (79 Flicks regions; English labels). Each carries the
        //    NATION it is picked under, matching what `/api/catalog` sends for it
        //    — this list is the fallback that renders before the catalog arrives,
        //    and a fallback that groups differently is a picker that rearranges
        //    itself under the reader. ─────────────────────────────────────────
        City("london", "London", 51.5074, -0.1278, "uk", "England"),
        City("manchester", "Manchester", 53.4808, -2.2426, "uk", "England"),
        City("norwich", "Norwich", 52.6309, 1.2974, "uk", "England"),
        City("aberdeenshire", "Aberdeenshire", 57.308, -2.3393, "uk", "Scotland"),
        City("antrim", "Antrim", 54.762, -6.0127, "uk", "Northern Ireland"),
        City("armagh", "Armagh", 54.4492, -6.398, "uk", "Northern Ireland"),
        City("ayrshire-and-arran", "Ayrshire and Arran", 55.5093, -4.581, "uk", "Scotland"),
        City("bedfordshire", "Bedfordshire", 52.0082, -0.4435, "uk", "England"),
        City("belfast", "Belfast", 54.5857, -5.9428, "uk", "Northern Ireland"),
        City("berkshire", "Berkshire", 51.4268, -0.9169, "uk", "England"),
        City("birmingham", "Birmingham", 52.4581, -1.9041, "uk", "England"),
        City("bristol", "Bristol", 51.4659, -2.5805, "uk", "England"),
        City("buckinghamshire", "Buckinghamshire", 51.7582, -0.7609, "uk", "England"),
        City("cambridgeshire", "Cambridgeshire", 52.4301, -0.0137, "uk", "England"),
        City("cardiff", "Cardiff", 51.4892, -3.1939, "uk", "Wales"),
        City("central-scotland", "Central Scotland", 56.08, -3.8066, "uk", "Scotland"),
        City("cheshire", "Cheshire", 53.2917, -2.4966, "uk", "England"),
        City("clwyd", "Clwyd", 53.3083, -3.6072, "uk", "Wales"),
        City("cornwall", "Cornwall", 50.317, -4.9211, "uk", "England"),
        City("county-durham", "County Durham", 54.7289, -1.5139, "uk", "England"),
        City("cumbria", "Cumbria", 54.4593, -3.1119, "uk", "England"),
        City("derbyshire", "Derbyshire", 52.9886, -1.5219, "uk", "England"),
        City("devon", "Devon", 50.6651, -3.687, "uk", "England"),
        City("dorset", "Dorset", 50.7664, -2.1122, "uk", "England"),
        City("down", "Down", 54.4293, -5.9704, "uk", "Northern Ireland"),
        City("dudley", "Dudley", 52.497, -2.0918, "uk", "England"),
        City("dumfries-and-galloway", "Dumfries and Galloway", 54.9881, -3.8232, "uk", "Scotland"),
        City("dunbartonshire-argyll-bute", "Dunbartonshire and Argyll & Bute", 55.7795, -4.9973, "uk", "Scotland"),
        City("dyfed", "Dyfed", 51.9892, -4.3329, "uk", "Wales"),
        City("east-sussex", "East Sussex", 50.8499, 0.2215, "uk", "England"),
        City("east-yorkshire", "East Yorkshire", 53.8685, -0.3985, "uk", "England"),
        City("edinburgh-and-lothians", "Edinburgh & Lothians", 55.9404, -3.2039, "uk", "Scotland"),
        City("essex", "Essex", 51.7621, 0.5901, "uk", "England"),
        City("fermanagh", "Fermanagh", 54.3499, -7.6316, "uk", "Northern Ireland"),
        City("fife", "Fife", 56.1287, -3.2424, "uk", "Scotland"),
        City("glamorgan", "Glamorgan", 51.6388, -3.7535, "uk", "Wales"),
        City("glasgow", "Glasgow", 55.8682, -4.2316, "uk", "Scotland"),
        City("gloucestershire", "Gloucestershire", 51.8387, -2.2712, "uk", "England"),
        City("guernsey", "Guernsey", 49.4446, -2.5695, "uk", "Crown Dependencies"),
        City("gwent", "Gwent", 51.6882, -3.0066, "uk", "Wales"),
        City("gwynedd", "Gwynedd", 53.0098, -4.153, "uk", "Wales"),
        City("hampshire", "Hampshire", 50.9234, -1.165, "uk", "England"),
        City("herefordshire", "Herefordshire", 52.031, -2.7825, "uk", "England"),
        City("hertfordshire", "Hertfordshire", 51.7791, -0.3102, "uk", "England"),
        City("highlands-and-islands", "Highlands and Islands", 58.086, -4.0855, "uk", "Scotland"),
        City("isle-of-man", "Isle of Man", 54.1578, -4.4775, "uk", "Crown Dependencies"),
        City("isle-of-wight", "Isle of Wight", 50.7118, -1.2248, "uk", "England"),
        City("jersey", "Jersey", 49.1839, -2.1144, "uk", "Crown Dependencies"),
        City("kent", "Kent", 51.2682, 0.8631, "uk", "England"),
        City("lanarkshire", "Lanarkshire", 55.7953, -4.0904, "uk", "Scotland"),
        City("lancashire", "Lancashire", 53.7367, -2.6625, "uk", "England"),
        City("leicestershire", "Leicestershire", 52.6656, -1.1514, "uk", "England"),
        City("lincolnshire", "Lincolnshire", 53.2194, -0.2916, "uk", "England"),
        City("londonderry", "Londonderry", 54.9949, -7.0636, "uk", "Northern Ireland"),
        City("liverpool", "Liverpool", 53.4084, -2.9916, "uk", "England"),
        City("north-yorkshire", "North Yorkshire", 54.2402, -1.156, "uk", "England"),
        City("northamptonshire", "Northamptonshire", 52.288, -0.8653, "uk", "England"),
        City("northumberland", "Northumberland", 55.2158, -1.7422, "uk", "England"),
        City("nottinghamshire", "Nottinghamshire", 53.0236, -1.15, "uk", "England"),
        City("oxfordshire", "Oxfordshire", 51.7572, -1.2545, "uk", "England"),
        City("powys", "Powys", 52.3806, -3.26, "uk", "Wales"),
        City("renfrewshire", "Renfrewshire", 55.9204, -4.5838, "uk", "Scotland"),
        City("roxburgh-ettrick-and-lauderdale", "Roxburgh, Ettrick and Lauderdale", 55.5183, -2.7969, "uk", "Scotland"),
        City("sandwell", "Sandwell", 52.5175, -1.9932, "uk", "England"),
        City("shropshire", "Shropshire", 52.6813, -2.6215, "uk", "England"),
        City("somerset", "Somerset", 51.2159, -2.824, "uk", "England"),
        City("south-yorkshire", "South Yorkshire", 53.5141, -1.3109, "uk", "England"),
        City("staffordshire", "Staffordshire", 52.7942, -1.9887, "uk", "England"),
        City("suffolk", "Suffolk", 52.1492, 1.0262, "uk", "England"),
        City("surrey", "Surrey", 51.2269, -0.5354, "uk", "England"),
        City("tayside", "Tayside", 56.5061, -3.0128, "uk", "Scotland"),
        City("tyne-and-wear", "Tyne and Wear", 54.9749, -1.5397, "uk", "England"),
        City("tyrone", "Tyrone", 54.5255, -6.8664, "uk", "Northern Ireland"),
        City("warwickshire", "Warwickshire", 52.3602, -1.5034, "uk", "England"),
        City("west-sussex", "West Sussex", 50.9492, -0.3262, "uk", "England"),
        City("west-yorkshire", "West Yorkshire", 53.7878, -1.665, "uk", "England"),
        City("wiltshire", "Wiltshire", 51.2955, -1.8505, "uk", "England"),
        City("worcestershire", "Worcestershire", 52.1923, -2.2079, "uk", "England"),
        City("yorkshire", "Yorkshire", 53.4082, -1.4756, "uk", "England"),
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

/** The regions [countryCode]'s cities are grouped under (US states), in the
 *  catalog's own order — which is the order the web picker lists them in, so the
 *  two read alike. Empty for a country that does not group its cities, and that
 *  emptiness is what the picker reads as "show one flat list". */
fun List<City>.regionsIn(countryCode: String): List<String> =
    inCountry(countryCode).mapNotNull { it.region }.distinct()

/** [regionsIn] narrowed to those matching [query], folded the same way city
 *  names are, so "calif" finds "California". A blank query yields them all. */
fun List<City>.regionsMatching(query: String, countryCode: String): List<String> {
    val q = Cities.searchFold(query.trim())
    val regions = regionsIn(countryCode)
    return if (q.isEmpty()) regions else regions.filter { Cities.searchFold(it).contains(q) }
}

/** [matching] confined to one [region] — the second step of a grouped country's
 *  pick. A null [region] leaves the country-wide list alone, so the same call
 *  serves both a grouped country's second screen and an ungrouped country's only
 *  one. */
fun List<City>.matchingInRegion(query: String, countryCode: String, region: String?): List<City> =
    matching(query, countryCode).let { cities ->
        if (region == null) cities else cities.filter { it.region == region }
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
