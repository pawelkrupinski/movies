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
     * countries that group nothing. Neither 2,000 US places nor 260 UK pages in
     * one A-to-Z is a list anybody reads, so the picker asks for the group
     * first; a country without regions keeps the single flat list.
     *
     * Carried by `/api/catalog` (and the bundled seed). Defaulted so a catalog
     * from an older server, or one of the hand-written [Cities.all] rows, still
     * decodes.
     */
    val region: String? = null,
    /**
     * The SECOND-level group [region] sits under a further split, where that
     * split actually holds more than one city — a UK county holding
     * several pages (Glamorgan: Cardiff, Swansea…).
     * Null everywhere else: a county that collapsed onto its
     * one place reads correctly through [region] alone (Shetland needs no
     * third tap to reach Lerwick), and Germany/the US never nest this deep.
     *
     * Carried by `/api/catalog` (and the bundled seed), same shape as [region].
     */
    val subregion: String? = null,
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
    /**
     * Slugs this city used to answer at, which the web now 301s onto it — a
     * retired page's (`miedzyrzec-podlaski` → `biala-podlaska`), a renamed
     * metro's. App Links hand those URLs to the app without ever reaching that
     * redirect, so [pl.kinowo.deeplink.DeepLink.parse] resolves them through
     * [currentSlugByFormer] instead. Carried by `/api/catalog` (and the bundled
     * seed), same shape as [region].
     *
     * The server leaves out any slug ANOTHER country has live, which protects
     * builds from before slugs were per country; this build reads [otherSlugs]
     * instead ([slugsLeadingHere]), and this only from a server too old to send it.
     */
    val formerSlugs: List<String> = emptyList(),
    /**
     * EVERY other slug of this city's own country that leads to it: a retired or
     * renamed page's (the UK's `kent` → Chatham, though the US has a Kent of its
     * own), a town the page covers (`/uk/ilkley/`), the page's qualified slug.
     * The App Links claim all of those URLs, which the web 301s here. `null` from
     * a server too old to send it.
     */
    val otherSlugs: List<String>? = null,
    /**
     * Other names search should find this city by: the group it was pulled up
     * out of when that group held only it (Guam over "Dededo Village area",
     * Merseyside over Liverpool), and pages folded into it. The web's picker
     * reads the same list (`data-alias`). Read through [matchesFolded]. Carried
     * by `/api/catalog` (and the bundled seed), same shape as [region].
     */
    val aliases: List<String> = emptyList(),
)

/** Whether [City.name] or any of [City.aliases] contains the already-folded
 *  query [q]. Every search over cities goes through here, so an alias is
 *  found wherever the name would be. */
fun City.matchesFolded(q: String): Boolean =
    (listOf(name) + aliases).any { Cities.searchFold(it).contains(q) }

/** The slugs besides its own that open this city — [City.otherSlugs], else (an
 *  older server) [City.formerSlugs]. */
val City.slugsLeadingHere: List<String> get() = otherSlugs ?: formerSlugs

/** Each other slug ([slugsLeadingHere]) of [countryCode]'s cities → the city
 *  that answers at it now. Per country: another country may use the same slug
 *  for a live city of its own. */
fun List<City>.currentSlugByFormer(countryCode: String): Map<String, String> =
    buildMap { inCountry(countryCode).forEach { city -> city.slugsLeadingHere.forEach { putIfAbsent(it, city.slug) } } }

/** The city [slug] names in [countryCode] — a slug is only its country's own:
 *  `york` is York on `uk` and York, Pennsylvania on `us`. */
fun List<City>.city(slug: String, countryCode: String): City? =
    firstOrNull { it.country == countryCode && it.slug == slug }

/**
 * The zone to reason about [slug]'s showtimes in — that [countryCode] city's
 * own where the catalog gave it one, else [fallback] (its country's).
 *
 * Also the answer for a slug this catalog does not know, which is what a stale
 * saved selection or a deep link into another deployment looks like, and for an
 * identifier the platform cannot parse. Past-showtime pruning and the day
 * buckets both go through here, so they cannot disagree.
 */
fun List<City>.zoneFor(slug: String?, countryCode: String, fallback: java.time.ZoneId): java.time.ZoneId {
    val city = slug?.let { city(it, countryCode) } ?: return fallback
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
        // ── Poland: GENERATED by data/pl/scripts/generate_polish_pages.py — do not edit by hand.
        City("poznan", "Poznań", 52.4064, 16.9252, "pl", "Wielkopolskie"),
        City("wroclaw", "Wrocław", 51.1079, 17.0385, "pl", "Dolnośląskie"),
        City("warszawa", "Warszawa", 52.2297, 21.0122, "pl", "Mazowieckie"),
        City("krakow", "Kraków", 50.0647, 19.9450, "pl", "Małopolskie"),
        City("lodz", "Łódź", 51.7592, 19.4560, "pl", "Łódzkie"),
        City("katowice", "Katowice", 50.2649, 19.0238, "pl", "Śląskie"),
        City("szczecin", "Szczecin", 53.4285, 14.5528, "pl", "Zachodniopomorskie"),
        City("bialystok", "Białystok", 53.1325, 23.1688, "pl", "Podlaskie"),
        City("trojmiasto", "Trójmiasto", 54.4416, 18.5601, "pl", "Pomorskie"),
        City("bydgoszcz", "Bydgoszcz", 53.1235, 18.0084, "pl", "Kujawsko-pomorskie"),
        City("lublin", "Lublin", 51.2465, 22.5684, "pl", "Lubelskie"),
        City("czestochowa", "Częstochowa", 50.8118, 19.1203, "pl", "Śląskie"),
        City("radom", "Radom", 51.4027, 21.1471, "pl", "Mazowieckie"),
        City("sosnowiec", "Sosnowiec", 50.2863, 19.1041, "pl", "Śląskie"),
        City("torun", "Toruń", 53.0138, 18.5984, "pl", "Kujawsko-pomorskie"),
        City("kielce", "Kielce", 50.8661, 20.6286, "pl", "Świętokrzyskie"),
        City("rzeszow", "Rzeszów", 50.0413, 21.9990, "pl", "Podkarpackie"),
        City("gliwice", "Gliwice", 50.2945, 18.6714, "pl", "Śląskie"),
        City("zabrze", "Zabrze", 50.3249, 18.7857, "pl", "Śląskie"),
        City("olsztyn", "Olsztyn", 53.7784, 20.4801, "pl", "Warmińsko-mazurskie"),
        City("bielsko-biala", "Bielsko-Biała", 49.8224, 19.0584, "pl", "Śląskie"),
        City("opole", "Opole", 50.6751, 17.9213, "pl", "Opolskie"),
        City("rybnik", "Rybnik", 50.0971, 18.5416, "pl", "Śląskie"),
        City("gorzow-wielkopolski", "Gorzów Wielkopolski", 52.7368, 15.2288, "pl", "Lubuskie"),
        City("elblag", "Elbląg", 54.1522, 19.4088, "pl", "Warmińsko-mazurskie"),
        City("koszalin", "Koszalin", 54.1943, 16.1722, "pl", "Zachodniopomorskie"),
        City("kalisz", "Kalisz", 51.7611, 18.0911, "pl", "Wielkopolskie"),
        City("zielona-gora", "Zielona Góra", 51.9356, 15.5062, "pl", "Lubuskie"),
        City("tychy", "Tychy", 50.1357, 18.9985, "pl", "Śląskie"),
        City("walbrzych", "Wałbrzych", 50.7714, 16.2845, "pl", "Dolnośląskie"),
        City("tarnow", "Tarnów", 50.0121, 20.9858, "pl", "Małopolskie"),
        City("wloclawek", "Włocławek", 52.6483, 19.0677, "pl", "Kujawsko-pomorskie"),
        City("legnica", "Legnica", 51.2070, 16.1619, "pl", "Dolnośląskie"),
        City("plock", "Płock", 52.5468, 19.7064, "pl", "Mazowieckie"),
        City("bytom", "Bytom", 50.3483, 18.9157, "pl", "Śląskie"),
        City("dabrowa-gornicza", "Dąbrowa Górnicza", 50.3219, 19.1876, "pl", "Śląskie"),
        City("nowy-sacz", "Nowy Sącz", 49.6175, 20.7154, "pl", "Małopolskie"),
        City("slupsk", "Słupsk", 54.4641, 17.0287, "pl", "Pomorskie"),
        City("jelenia-gora", "Jelenia Góra", 50.9044, 15.7197, "pl", "Dolnośląskie"),
        City("przemysl", "Przemyśl", 49.7838, 22.7677, "pl", "Podkarpackie"),
        City("konin", "Konin", 52.2230, 18.2511, "pl", "Wielkopolskie"),
        City("augustow", "Augustów", 53.8432, 22.9798, "pl", "Podlaskie"),
        City("belchatow", "Bełchatów", 51.3688, 19.3567, "pl", "Łódzkie"),
        City("biala-podlaska", "Biała Podlaska i okolice", 52.0324, 23.1165, "pl", "Lubelskie"),
        City("bielsk-podlaski", "Bielsk Podlaski", 52.7651, 23.1865, "pl", "Podlaskie"),
        City("bilgoraj", "Biłgoraj", 50.5411, 22.7220, "pl", "Lubelskie"),
        City("bochnia", "Bochnia i okolice", 49.9691, 20.4303, "pl", "Małopolskie"),
        City("boleslawiec", "Bolesławiec i okolice", 51.2642, 15.5697, "pl", "Dolnośląskie"),
        City("braniewo", "Braniewo", 54.3797, 19.8196, "pl", "Warmińsko-mazurskie"),
        City("brzeg", "Brzeg", 50.8608, 17.4674, "pl", "Opolskie"),
        City("brzeg-dolny", "Brzeg Dolny i okolice", 51.2730, 16.7081, "pl", "Dolnośląskie"),
        City("busko-zdroj", "Busko-Zdrój i okolice", 50.4708, 20.7188, "pl", "Świętokrzyskie"),
        City("chelm", "Chełm", 51.1431, 23.4716, "pl", "Lubelskie"),
        City("chojnice", "Chojnice i okolice", 53.6955, 17.5570, "pl", "Pomorskie"),
        City("choszczno", "Choszczno i okolice", 53.1690, 15.4205, "pl", "Zachodniopomorskie"),
        City("ciechanow", "Ciechanów i okolice", 52.8814, 20.6200, "pl", "Mazowieckie"),
        City("czechowice-dziedzice", "Czechowice-Dziedzice i okolice", 49.9134, 19.0048, "pl", "Śląskie"),
        City("dabrowa-tarnowska", "Dąbrowa Tarnowska i okolice", 50.1746, 20.9863, "pl", "Małopolskie"),
        City("elk", "Ełk", 53.8282, 22.3647, "pl", "Warmińsko-mazurskie"),
        City("gizycko", "Giżycko i okolice", 54.0381, 21.7644, "pl", "Warmińsko-mazurskie"),
        City("glogow", "Głogów", 51.6636, 16.0845, "pl", "Dolnośląskie"),
        City("gniezno", "Gniezno i okolice", 52.5348, 17.5826, "pl", "Wielkopolskie"),
        City("goldap", "Gołdap", 54.3063, 22.3036, "pl", "Warmińsko-mazurskie"),
        City("grajewo", "Grajewo", 53.6473, 22.4554, "pl", "Podlaskie"),
        City("grudziadz", "Grudziądz i okolice", 53.4841, 18.7537, "pl", "Kujawsko-pomorskie"),
        City("hrubieszow", "Hrubieszów", 50.8050, 23.8925, "pl", "Lubelskie"),
        City("ilawa", "Iława i okolice", 53.5960, 19.5685, "pl", "Warmińsko-mazurskie"),
        City("inowroclaw", "Inowrocław i okolice", 52.7989, 18.2639, "pl", "Kujawsko-pomorskie"),
        City("jaroslaw", "Jarosław", 50.0162, 22.6778, "pl", "Podkarpackie"),
        City("jaslo", "Jasło i okolice", 49.7451, 21.4725, "pl", "Podkarpackie"),
        City("jastrzebie-zdroj", "Jastrzębie-Zdrój i okolice", 49.9554, 18.5748, "pl", "Śląskie"),
        City("jaworzno", "Jaworzno i okolice", 50.2053, 19.2750, "pl", "Śląskie"),
        City("kedzierzyn-kozle", "Kędzierzyn-Koźle", 50.3498, 18.2261, "pl", "Opolskie"),
        City("kepno", "Kępno i okolice", 51.2784, 17.9891, "pl", "Wielkopolskie"),
        City("klodzko", "Kłodzko i okolice", 50.4349, 16.6614, "pl", "Dolnośląskie"),
        City("kluczbork", "Kluczbork", 50.9728, 18.2182, "pl", "Opolskie"),
        City("kolobrzeg", "Kołobrzeg i okolice", 54.1756, 15.5834, "pl", "Zachodniopomorskie"),
        City("koscierzyna", "Kościerzyna i okolice", 54.1223, 17.9812, "pl", "Pomorskie"),
        City("krapkowice", "Krapkowice", 50.4751, 17.9654, "pl", "Opolskie"),
        City("krasnik", "Kraśnik i okolice", 50.9236, 22.2271, "pl", "Lubelskie"),
        City("krasnystaw", "Krasnystaw", 50.9846, 23.1742, "pl", "Lubelskie"),
        City("krosno", "Krosno", 49.6887, 21.7706, "pl", "Podkarpackie"),
        City("krotoszyn", "Krotoszyn i okolice", 51.6987, 17.4374, "pl", "Wielkopolskie"),
        City("krynica-zdroj", "Krynica-Zdrój i okolice", 49.4222, 20.9594, "pl", "Małopolskie"),
        City("kutno", "Kutno i okolice", 52.2306, 19.3641, "pl", "Łódzkie"),
        City("lapy", "Łapy", 52.9911, 22.8842, "pl", "Podlaskie"),
        City("leba", "Łeba", 54.7610, 17.5555, "pl", "Pomorskie"),
        City("legionowo", "Legionowo i okolice", 52.4015, 20.9266, "pl", "Mazowieckie"),
        City("leszno", "Leszno", 51.8403, 16.5749, "pl", "Wielkopolskie"),
        City("lidzbark-warminski", "Lidzbark Warmiński", 54.1259, 20.5795, "pl", "Warmińsko-mazurskie"),
        City("lipno", "Lipno i okolice", 52.8444, 19.1785, "pl", "Kujawsko-pomorskie"),
        City("lomza", "Łomża i okolice", 53.1781, 22.0593, "pl", "Podlaskie"),
        City("lubin", "Lubin", 51.4009, 16.2015, "pl", "Dolnośląskie"),
        City("lubliniec", "Lubliniec", 50.6690, 18.6844, "pl", "Śląskie"),
        City("lukow", "Łuków", 51.9290, 22.3796, "pl", "Lubelskie"),
        City("miechow", "Miechów i okolice", 50.3565, 20.0279, "pl", "Małopolskie"),
        City("miedzyrzecz", "Międzyrzecz", 52.4446, 15.5780, "pl", "Lubuskie"),
        City("miedzyzdroje", "Międzyzdroje", 53.9292, 14.4510, "pl", "Zachodniopomorskie"),
        City("mielec", "Mielec i okolice", 50.2871, 21.4239, "pl", "Podkarpackie"),
        City("mlawa", "Mława i okolice", 53.1128, 20.3841, "pl", "Mazowieckie"),
        City("morag", "Morąg", 53.9171, 19.9260, "pl", "Warmińsko-mazurskie"),
        City("myszkow", "Myszków", 50.5752, 19.3246, "pl", "Śląskie"),
        City("naklo-nad-notecia", "Nakło nad Notecią i okolice", 53.1421, 17.6018, "pl", "Kujawsko-pomorskie"),
        City("nowa-sol", "Nowa Sól", 51.8033, 15.7170, "pl", "Lubuskie"),
        City("nowy-targ", "Nowy Targ i okolice", 49.4778, 20.0323, "pl", "Małopolskie"),
        City("nowy-tomysl", "Nowy Tomyśl i okolice", 52.3195, 16.1284, "pl", "Wielkopolskie"),
        City("nysa", "Nysa i okolice", 50.4738, 17.3344, "pl", "Opolskie"),
        City("olawa", "Oława", 50.9466, 17.2926, "pl", "Dolnośląskie"),
        City("olecko", "Olecko", 54.0337, 22.5070, "pl", "Warmińsko-mazurskie"),
        City("olesnica", "Oleśnica", 51.2134, 17.3899, "pl", "Dolnośląskie"),
        City("olsztynek", "Olsztynek", 53.5837, 20.2847, "pl", "Warmińsko-mazurskie"),
        City("opoczno", "Opoczno i okolice", 51.3757, 20.2783, "pl", "Łódzkie"),
        City("ostrow-mazowiecka", "Ostrów Mazowiecka", 52.8025, 21.8951, "pl", "Mazowieckie"),
        City("ostrow-wielkopolski", "Ostrów Wielkopolski", 51.6550, 17.8069, "pl", "Wielkopolskie"),
        City("ostrowiec-swietokrzyski", "Ostrowiec Świętokrzyski i okolice", 50.9294, 21.3852, "pl", "Świętokrzyskie"),
        City("oswiecim", "Oświęcim i okolice", 50.0344, 19.2104, "pl", "Małopolskie"),
        City("otwock", "Otwock i okolice", 52.1058, 21.2613, "pl", "Mazowieckie"),
        City("pabianice", "Pabianice", 51.6645, 19.3547, "pl", "Łódzkie"),
        City("parczew", "Parczew", 51.6402, 22.9006, "pl", "Lubelskie"),
        City("pila", "Piła i okolice", 53.1514, 16.7378, "pl", "Wielkopolskie"),
        City("piotrkow-trybunalski", "Piotrków Trybunalski", 51.4055, 19.7032, "pl", "Łódzkie"),
        City("police", "Police i okolice", 53.5521, 14.5718, "pl", "Zachodniopomorskie"),
        City("polkowice", "Polkowice", 51.5039, 16.0726, "pl", "Dolnośląskie"),
        City("pruszkow", "Pruszków i okolice", 52.1707, 20.8121, "pl", "Mazowieckie"),
        City("przeworsk", "Przeworsk i okolice", 50.0591, 22.4941, "pl", "Podkarpackie"),
        City("pulawy", "Puławy i okolice", 51.4166, 21.9694, "pl", "Lubelskie"),
        City("pultusk", "Pułtusk i okolice", 52.7025, 21.0828, "pl", "Mazowieckie"),
        City("radomsko", "Radomsko", 51.0671, 19.4448, "pl", "Łódzkie"),
        City("rawicz", "Rawicz i okolice", 51.6095, 16.8585, "pl", "Wielkopolskie"),
        City("ruda-slaska", "Ruda Śląska i okolice", 50.2584, 18.8563, "pl", "Śląskie"),
        City("rypin", "Rypin", 53.0660, 19.4094, "pl", "Kujawsko-pomorskie"),
        City("sanok", "Sanok i okolice", 49.5557, 22.2056, "pl", "Podkarpackie"),
        City("siedlce", "Siedlce", 52.1677, 22.2901, "pl", "Mazowieckie"),
        City("sierpc", "Sierpc", 52.8568, 19.6691, "pl", "Mazowieckie"),
        City("skierniewice", "Skierniewice", 51.9549, 20.1584, "pl", "Łódzkie"),
        City("slawno", "Sławno i okolice", 54.3628, 16.6789, "pl", "Zachodniopomorskie"),
        City("slubice", "Słubice", 52.3509, 14.5607, "pl", "Lubuskie"),
        City("slupca", "Słupca i okolice", 52.2873, 17.8719, "pl", "Wielkopolskie"),
        City("sokolka", "Sokółka", 53.4072, 23.5023, "pl", "Podlaskie"),
        City("sokolow-podlaski", "Sokołów Podlaski i okolice", 52.4068, 22.2531, "pl", "Mazowieckie"),
        City("srem", "Śrem i okolice", 52.0887, 17.0151, "pl", "Wielkopolskie"),
        City("stalowa-wola", "Stalowa Wola i okolice", 50.5829, 22.0533, "pl", "Podkarpackie"),
        City("stara-blotnica", "Stara Błotnica", 51.5468, 20.9748, "pl", "Mazowieckie"),
        City("starachowice", "Starachowice i okolice", 51.0374, 21.0713, "pl", "Świętokrzyskie"),
        City("stargard", "Stargard i okolice", 53.3367, 15.0499, "pl", "Zachodniopomorskie"),
        City("strzelin", "Strzelin", 50.7816, 17.0648, "pl", "Dolnośląskie"),
        City("suwalki", "Suwałki", 54.1118, 22.9309, "pl", "Podlaskie"),
        City("swidnica", "Świdnica i okolice", 50.8438, 16.4886, "pl", "Dolnośląskie"),
        City("swidnik", "Świdnik i okolice", 51.2190, 22.6962, "pl", "Lubelskie"),
        City("swiebodzin", "Świebodzin", 52.2475, 15.5335, "pl", "Lubuskie"),
        City("szamotuly", "Szamotuły i okolice", 52.6120, 16.5779, "pl", "Wielkopolskie"),
        City("szczecinek", "Szczecinek", 53.7079, 16.6994, "pl", "Zachodniopomorskie"),
        City("szczytno", "Szczytno", 53.5626, 20.9875, "pl", "Warmińsko-mazurskie"),
        City("tczew", "Tczew i okolice", 54.0924, 18.7779, "pl", "Pomorskie"),
        City("tomaszow-lubelski", "Tomaszów Lubelski", 50.4477, 23.4162, "pl", "Lubelskie"),
        City("tomaszow-mazowiecki", "Tomaszów Mazowiecki", 51.5313, 20.0085, "pl", "Łódzkie"),
        City("turek", "Turek i okolice", 52.0155, 18.5006, "pl", "Wielkopolskie"),
        City("wadowice", "Wadowice i okolice", 49.8834, 19.4929, "pl", "Małopolskie"),
        City("wagrowiec", "Wągrowiec i okolice", 52.8084, 17.1996, "pl", "Wielkopolskie"),
        City("wejherowo", "Wejherowo i okolice", 54.6057, 18.2356, "pl", "Pomorskie"),
        City("wielen", "Wieleń", 52.8946, 16.1714, "pl", "Wielkopolskie"),
        City("wieliczka", "Wieliczka i okolice", 49.9874, 20.0647, "pl", "Małopolskie"),
        City("wielun", "Wieluń", 51.2210, 18.5696, "pl", "Łódzkie"),
        City("wlodawa", "Włodawa", 51.5500, 23.5500, "pl", "Lubelskie"),
        City("wloszczowa", "Włoszczowa", 50.8526, 19.9659, "pl", "Świętokrzyskie"),
        City("wolomin", "Wołomin i okolice", 52.3401, 21.2421, "pl", "Mazowieckie"),
        City("wysokie-mazowieckie", "Wysokie Mazowieckie", 52.9166, 22.5171, "pl", "Podlaskie"),
        City("wyszkow", "Wyszków i okolice", 52.5928, 21.4584, "pl", "Mazowieckie"),
        City("zakopane", "Zakopane", 49.2990, 19.9489, "pl", "Małopolskie"),
        City("zamosc", "Zamość", 50.7231, 23.2520, "pl", "Lubelskie"),
        City("zary", "Żary", 51.6420, 15.1373, "pl", "Lubuskie"),
        City("zawiercie", "Zawiercie", 50.4877, 19.4168, "pl", "Śląskie"),
        City("zdunska-wola", "Zduńska Wola i okolice", 51.5992, 18.9397, "pl", "Łódzkie"),
        City("zgierz", "Zgierz i okolice", 51.8556, 19.4062, "pl", "Łódzkie"),
        City("zgorzelec", "Zgorzelec i okolice", 51.1494, 15.0084, "pl", "Dolnośląskie"),
        City("zlocieniec", "Złocieniec i okolice", 53.5329, 16.0113, "pl", "Zachodniopomorskie"),
        City("zlotow", "Złotów i okolice", 53.3635, 17.0408, "pl", "Wielkopolskie"),
        City("zory", "Żory", 50.0452, 18.7006, "pl", "Śląskie"),
        City("zyrardow", "Żyrardów i okolice", 52.0488, 20.4460, "pl", "Mazowieckie"),
        City("zywiec", "Żywiec i okolice", 49.6853, 19.1924, "pl", "Śląskie"),
        // ── end Poland (generated)
        // ── United Kingdom: GENERATED by data/uk/scripts/generate_roster.py — do not edit by hand.
        City("london", "London", 51.5085, -0.1257, "uk", "England"),
        City("birmingham", "Birmingham", 52.4814, -1.8998, "uk", "England", "West Midlands"),
        City("glasgow", "Glasgow", 55.8651, -4.2576, "uk", "Scotland"),
        City("manchester", "Manchester", 53.4809, -2.2374, "uk", "England", "Greater Manchester"),
        City("sheffield", "Sheffield", 53.3830, -1.4659, "uk", "England", "South Yorkshire"),
        City("leeds", "Leeds", 53.7965, -1.5478, "uk", "England", "West Yorkshire"),
        City("edinburgh", "Edinburgh", 55.9521, -3.1965, "uk", "Scotland", "Lothian"),
        City("liverpool", "Liverpool", 53.4106, -2.9779, "uk", "England", "Merseyside"),
        City("bristol", "Bristol", 51.4552, -2.5966, "uk", "England"),
        City("cardiff", "Cardiff", 51.4800, -3.1800, "uk", "Wales", "Glamorgan"),
        City("leicester", "Leicester", 52.6386, -1.1317, "uk", "England", "Leicestershire"),
        City("bradford", "Bradford", 53.7939, -1.7521, "uk", "England", "West Yorkshire"),
        City("belfast", "Belfast", 54.5968, -5.9254, "uk", "Northern Ireland"),
        City("coventry", "Coventry", 52.4066, -1.5122, "uk", "England", "West Midlands"),
        City("birkenhead", "Birkenhead", 53.3934, -3.0148, "uk", "England", "Merseyside"),
        City("nottingham", "Nottingham", 52.9536, -1.1505, "uk", "England", "Nottinghamshire"),
        City("reading", "Reading", 51.4562, -0.9711, "uk", "England", "Berkshire"),
        City("hull", "Hull", 53.7446, -0.3352, "uk", "England", "East Riding of Yorkshire"),
        City("preston", "Preston", 53.7628, -2.7045, "uk", "England", "Lancashire"),
        City("swansea", "Swansea", 51.6208, -3.9432, "uk", "Wales", "Glamorgan"),
        City("newcastle-upon-tyne", "Newcastle upon Tyne", 54.9733, -1.6140, "uk", "England", "Tyne and Wear"),
        City("southend-on-sea", "Southend-on-Sea", 51.5378, 0.7143, "uk", "England", "Essex"),
        City("brighton", "Brighton", 50.8284, -0.1395, "uk", "England", "East Sussex"),
        City("derby", "Derby", 52.9228, -1.4766, "uk", "England", "Derbyshire"),
        City("southampton", "Southampton", 50.9040, -1.4043, "uk", "England", "Hampshire"),
        City("wolverhampton", "Wolverhampton", 52.5855, -2.1230, "uk", "England", "West Midlands"),
        City("plymouth", "Plymouth", 50.3715, -4.1430, "uk", "England", "Devon"),
        City("stoke-on-trent", "Stoke-on-Trent", 53.0042, -2.1854, "uk", "England", "Staffordshire"),
        City("milton-keynes", "Milton Keynes", 52.0417, -0.7558, "uk", "England", "Buckinghamshire"),
        City("northampton", "Northampton", 52.2500, -0.8833, "uk", "England", "Northamptonshire"),
        City("oldham", "Oldham", 53.5405, -2.1183, "uk", "England", "Greater Manchester"),
        City("luton", "Luton", 51.8797, -0.4175, "uk", "England", "Bedfordshire"),
        City("portsmouth", "Portsmouth", 50.7990, -1.0913, "uk", "England", "Hampshire"),
        City("swindon", "Swindon", 51.5580, -1.7812, "uk", "England", "Wiltshire"),
        City("dudley", "Dudley", 52.5000, -2.0833, "uk", "England", "West Midlands"),
        City("aberdeen", "Aberdeen", 57.1437, -2.0981, "uk", "Scotland", "Aberdeenshire"),
        City("st-helens", "St Helens", 53.4500, -2.7333, "uk", "England", "Merseyside"),
        City("ipswich", "Ipswich", 52.0592, 1.1555, "uk", "England", "Suffolk"),
        City("wigan", "Wigan", 53.5430, -2.6371, "uk", "England", "Greater Manchester"),
        City("warrington", "Warrington", 53.3925, -2.5802, "uk", "England", "Cheshire"),
        City("walsall", "Walsall", 52.5853, -1.9840, "uk", "England", "West Midlands"),
        City("mansfield", "Mansfield", 53.1333, -1.2000, "uk", "England", "Nottinghamshire"),
        City("sunderland", "Sunderland", 54.9046, -1.3822, "uk", "England", "Tyne and Wear"),
        City("bournemouth", "Bournemouth", 50.7205, -1.8795, "uk", "England", "Dorset"),
        City("peterborough", "Peterborough", 52.5736, -0.2478, "uk", "England", "Cambridgeshire"),
        City("oxford", "Oxford", 51.7522, -1.2560, "uk", "England", "Oxfordshire"),
        City("newport", "Newport", 51.5877, -2.9983, "uk", "Wales", "Gwent"),
        City("york", "York", 53.9576, -1.0827, "uk", "England", "North Yorkshire"),
        City("telford", "Telford", 52.6766, -2.4493, "uk", "England", "Shropshire"),
        City("poole", "Poole", 50.7143, -1.9846, "uk", "England", "Dorset"),
        City("burnley", "Burnley", 53.8000, -2.2333, "uk", "England", "Lancashire"),
        City("huddersfield", "Huddersfield", 53.6490, -1.7842, "uk", "England", "West Yorkshire"),
        City("dundee", "Dundee", 56.4691, -2.9749, "uk", "Scotland", "Tayside"),
        City("blackburn", "Blackburn", 53.7500, -2.4833, "uk", "England", "Lancashire"),
        City("cambridge", "Cambridge", 52.2000, 0.1167, "uk", "England", "Cambridgeshire"),
        City("blackpool", "Blackpool", 53.8167, -3.0500, "uk", "England", "Lancashire"),
        City("basildon", "Basildon", 51.5684, 0.4578, "uk", "England", "Essex"),
        City("norwich", "Norwich", 52.6278, 1.2983, "uk", "England", "Norfolk"),
        City("middlesbrough", "Middlesbrough", 54.5762, -1.2348, "uk", "England", "North Yorkshire"),
        City("bolton", "Bolton", 53.5833, -2.4333, "uk", "England", "Greater Manchester"),
        City("stockport", "Stockport", 53.4098, -2.1576, "uk", "England", "Greater Manchester"),
        City("high-wycombe", "High Wycombe", 51.6291, -0.7493, "uk", "England", "Buckinghamshire"),
        City("gloucester", "Gloucester", 51.8657, -2.2431, "uk", "England", "Gloucestershire"),
        City("exeter", "Exeter", 50.7236, -3.5275, "uk", "England", "Devon"),
        City("colchester", "Colchester", 51.8892, 0.9042, "uk", "England", "Essex"),
        City("salford", "Salford", 53.4877, -2.2904, "uk", "England", "Greater Manchester"),
        City("newcastle-under-lyme", "Newcastle-under-Lyme", 53.0000, -2.2333, "uk", "England", "Staffordshire"),
        City("solihull", "Solihull", 52.4143, -1.7809, "uk", "England", "West Midlands"),
        City("watford", "Watford", 51.6553, -0.3960, "uk", "England", "Hertfordshire"),
        City("crawley", "Crawley", 51.1130, -0.1831, "uk", "England", "West Sussex"),
        City("burton-upon-trent", "Burton upon Trent", 52.8073, -1.6426, "uk", "England", "Staffordshire"),
        City("cheltenham", "Cheltenham", 51.9001, -2.0797, "uk", "England", "Gloucestershire"),
        City("rotherham", "Rotherham", 53.4301, -1.3568, "uk", "England", "South Yorkshire"),
        City("worthing", "Worthing", 50.8180, -0.3754, "uk", "England", "West Sussex"),
        City("doncaster", "Doncaster", 53.5228, -1.1312, "uk", "England", "South Yorkshire"),
        City("chesterfield", "Chesterfield", 53.2500, -1.4167, "uk", "England", "Derbyshire"),
        City("chelmsford", "Chelmsford", 51.7358, 0.4696, "uk", "England", "Essex"),
        City("sutton-coldfield", "Sutton Coldfield", 52.5667, -1.8167, "uk", "England", "West Midlands"),
        City("wakefield", "Wakefield", 53.6833, -1.4977, "uk", "England", "West Yorkshire"),
        City("basingstoke", "Basingstoke", 51.2625, -1.0871, "uk", "England", "Hampshire"),
        City("maidstone", "Maidstone", 51.2667, 0.5167, "uk", "England", "Kent"),
        City("bedford", "Bedford", 52.1346, -0.4663, "uk", "England", "Bedfordshire"),
        City("woking", "Woking", 51.3190, -0.5589, "uk", "England", "Surrey"),
        City("lincoln", "Lincoln", 53.2268, -0.5379, "uk", "England", "Lincolnshire"),
        City("west-bromwich", "West Bromwich", 52.5187, -1.9945, "uk", "England", "West Midlands"),
        City("eastbourne", "Eastbourne", 50.7687, 0.2845, "uk", "England", "East Sussex"),
        City("worcester", "Worcester", 52.1893, -2.2200, "uk", "England", "Worcestershire"),
        City("bath", "Bath", 51.3751, -2.3617, "uk", "England", "Somerset"),
        City("aberfeldy", "Aberfeldy", 56.6220, -3.8669, "uk", "Scotland", "Tayside"),
        City("aberystwyth", "Aberystwyth area", 52.4155, -4.0829, "uk", "Wales", "Dyfed"),
        City("abingdon", "Abingdon area", 51.6711, -1.2828, "uk", "England", "Oxfordshire"),
        City("accrington", "Accrington area", 53.7538, -2.3586, "uk", "England", "Lancashire"),
        City("altrincham", "Altrincham area", 53.3875, -2.3485, "uk", "England", "Greater Manchester"),
        City("arbroath", "Arbroath area", 56.5632, -2.5874, "uk", "Scotland", "Tayside"),
        City("ashford", "Ashford", 51.1465, 0.8738, "uk", "England", "Kent"),
        City("aviemore", "Aviemore", 57.1955, -3.8259, "uk", "Scotland", "Highland"),
        City("aylesbury", "Aylesbury", 51.8167, -0.8146, "uk", "England", "Buckinghamshire"),
        City("ayr", "Ayr", 55.4627, -4.6339, "uk", "Scotland"),
        City("bala", "Bala", 52.9111, -3.5972, "uk", "Wales", "Gwynedd"),
        City("ballymena", "Ballymena", 54.8636, -6.2763, "uk", "Northern Ireland"),
        City("banbridge", "Banbridge", 54.3500, -6.2833, "uk", "Northern Ireland"),
        City("banbury", "Banbury area", 52.0632, -1.3422, "uk", "England", "Oxfordshire"),
        City("banchory", "Banchory", 57.0517, -2.4882, "uk", "Scotland", "Aberdeenshire"),
        City("bangor", "Bangor area", 54.6608, -5.6680, "uk", "Northern Ireland"),
        City("bangor-gwynedd", "Bangor area", 53.2275, -4.1294, "uk", "Wales", "Gwynedd"),
        City("barnsley", "Barnsley", 53.5500, -1.4833, "uk", "England", "South Yorkshire"),
        City("barnstaple", "Barnstaple area", 51.0802, -4.0581, "uk", "England", "Devon"),
        City("barrow-in-furness", "Barrow-in-Furness area", 54.1109, -3.2276, "uk", "England", "Cumbria"),
        City("berwick-upon-tweed", "Berwick-upon-Tweed", 55.7687, -2.0054, "uk", "England"),
        City("beverley", "Beverley area", 53.8459, -0.4233, "uk", "England", "East Riding of Yorkshire"),
        City("bishop-s-stortford", "Bishop's Stortford area", 51.8711, 0.1587, "uk", "England", "Hertfordshire"),
        City("boston", "Boston area", 52.9763, -0.0266, "uk", "England", "Lincolnshire"),
        City("bracknell", "Bracknell area", 51.4136, -0.7505, "uk", "England", "Berkshire"),
        City("braintree", "Braintree area", 51.8782, 0.5529, "uk", "England", "Essex"),
        City("bridgend", "Bridgend area", 51.5058, -3.5772, "uk", "Wales", "Glamorgan"),
        City("bridgwater", "Bridgwater area", 51.1284, -3.0036, "uk", "England", "Somerset"),
        City("bridlington", "Bridlington", 54.0831, -0.1919, "uk", "England", "East Riding of Yorkshire"),
        City("broadstairs", "Broadstairs area", 51.3584, 1.4419, "uk", "England", "Kent"),
        City("bude", "Bude", 50.8244, -4.5413, "uk", "England", "Cornwall"),
        City("builth-wells", "Builth Wells area", 52.1494, -3.4047, "uk", "Wales"),
        City("burgess-hill", "Burgess Hill area", 50.9584, -0.1329, "uk", "England", "West Sussex"),
        City("bury-st-edmunds", "Bury St Edmunds", 52.2463, 0.7111, "uk", "England", "Suffolk"),
        City("campbeltown", "Campbeltown", 55.4258, -5.6076, "uk", "Scotland", "Argyll and Bute"),
        City("cannock", "Cannock area", 52.6904, -2.0309, "uk", "England", "Staffordshire"),
        City("canterbury", "Canterbury", 51.2790, 1.0799, "uk", "England", "Kent"),
        City("cardigan", "Cardigan", 52.0837, -4.6623, "uk", "Wales", "Dyfed"),
        City("carlisle", "Carlisle area", 54.8951, -2.9382, "uk", "England", "Cumbria"),
        City("castleford", "Castleford area", 53.7259, -1.3626, "uk", "England", "West Yorkshire"),
        City("chatham", "Chatham area", 51.3789, 0.5279, "uk", "England", "Kent"),
        City("chester", "Chester area", 53.1905, -2.8919, "uk", "England", "Cheshire"),
        City("chichester", "Chichester area", 50.8367, -0.7800, "uk", "England", "West Sussex"),
        City("christchurch", "Christchurch area", 50.7358, -1.7813, "uk", "England", "Dorset"),
        City("clacton-on-sea", "Clacton-on-Sea area", 51.7897, 1.1560, "uk", "England", "Essex"),
        City("cleethorpes", "Cleethorpes area", 53.5605, -0.0323, "uk", "England", "Lincolnshire"),
        City("coleraine", "Coleraine", 55.1333, -6.6667, "uk", "Northern Ireland"),
        City("colwyn-bay", "Colwyn Bay area", 53.2948, -3.7267, "uk", "Wales", "Clwyd"),
        City("consett", "Consett area", 54.8540, -1.8316, "uk", "England", "County Durham"),
        City("corby", "Corby area", 52.4964, -0.6894, "uk", "England", "Northamptonshire"),
        City("crewe", "Crewe area", 53.0979, -2.4416, "uk", "England", "Cheshire"),
        City("cromarty", "Cromarty", 57.6776, -4.0416, "uk", "Scotland", "Highland"),
        City("cromer", "Cromer area", 52.9312, 1.2989, "uk", "England", "Norfolk"),
        City("cwmbran", "Cwmbran area", 51.6545, -3.0228, "uk", "Wales", "Gwent"),
        City("darlington", "Darlington area", 54.5243, -1.5504, "uk", "England", "County Durham"),
        City("dereham", "Dereham area", 52.6833, 0.9333, "uk", "England", "Norfolk"),
        City("dorchester", "Dorchester", 50.7167, -2.4333, "uk", "England", "Dorset"),
        City("douglas", "Douglas", 54.1500, -4.4833, "uk", "Crown Dependencies"),
        City("dumfries", "Dumfries area", 55.0696, -3.6114, "uk", "Scotland", "Dumfries and Galloway"),
        City("dungannon", "Dungannon area", 54.5034, -6.7672, "uk", "Northern Ireland", "Mid Ulster"),
        City("durham", "Durham", 54.7768, -1.5757, "uk", "England", "County Durham"),
        City("eastleigh", "Eastleigh area", 50.9667, -1.3500, "uk", "England", "Hampshire"),
        City("elgin", "Elgin", 57.6495, -3.3184, "uk", "Scotland"),
        City("ely", "Ely", 52.3996, 0.2620, "uk", "England", "Cambridgeshire"),
        City("enniskillen", "Enniskillen", 54.3462, -7.6413, "uk", "Northern Ireland", "Fermanagh and Omagh"),
        City("epsom", "Epsom area", 51.3305, -0.2701, "uk", "England", "Surrey"),
        City("falmouth", "Falmouth", 50.1544, -5.0711, "uk", "England", "Cornwall"),
        City("fareham", "Fareham", 50.8516, -1.1793, "uk", "England", "Hampshire"),
        City("folkestone", "Folkestone area", 51.0817, 1.1673, "uk", "England", "Kent"),
        City("fort-william", "Fort William", 56.8165, -5.1121, "uk", "Scotland", "Highland"),
        City("gateshead", "Gateshead area", 54.9621, -1.6017, "uk", "England", "Tyne and Wear"),
        City("grantham", "Grantham area", 52.9115, -0.6418, "uk", "England", "Lincolnshire"),
        City("great-yarmouth", "Great Yarmouth area", 52.6083, 1.7305, "uk", "England", "Norfolk"),
        City("guildford", "Guildford area", 51.2354, -0.5743, "uk", "England", "Surrey"),
        City("halifax", "Halifax area", 53.7167, -1.8500, "uk", "England", "West Yorkshire"),
        City("hamilton", "Hamilton area", 55.7667, -4.0333, "uk", "Scotland"),
        City("harlow", "Harlow", 51.7766, 0.1116, "uk", "England", "Essex"),
        City("harrogate", "Harrogate", 53.9908, -1.5373, "uk", "England", "North Yorkshire"),
        City("hartlepool", "Hartlepool area", 54.6855, -1.2103, "uk", "England", "County Durham"),
        City("hastings", "Hastings area", 50.8557, 0.5801, "uk", "England", "East Sussex"),
        City("haverfordwest", "Haverfordwest area", 51.8017, -4.9691, "uk", "Wales", "Dyfed"),
        City("haverhill", "Haverhill", 52.0823, 0.4389, "uk", "England", "Suffolk"),
        City("hawick", "Hawick area", 55.4227, -2.7867, "uk", "Scotland"),
        City("helensburgh", "Helensburgh area", 56.0061, -4.7265, "uk", "Scotland", "Argyll and Bute"),
        City("hemel-hempstead", "Hemel Hempstead area", 51.7537, -0.4497, "uk", "England", "Hertfordshire"),
        City("hereford", "Hereford", 52.0568, -2.7148, "uk", "England", "Herefordshire"),
        City("herne-bay", "Herne Bay area", 51.3730, 1.1286, "uk", "England", "Kent"),
        City("horsham", "Horsham", 51.0631, -0.3276, "uk", "England", "West Sussex"),
        City("huntly", "Huntly", 57.4474, -2.7861, "uk", "Scotland", "Aberdeenshire"),
        City("ilkeston", "Ilkeston area", 52.9706, -1.3095, "uk", "England", "Derbyshire"),
        City("inverness", "Inverness", 57.4791, -4.2240, "uk", "Scotland", "Highland"),
        City("ivybridge", "Ivybridge area", 50.3904, -3.9191, "uk", "England", "Devon"),
        City("kendal", "Kendal area", 54.3268, -2.7476, "uk", "England", "Cumbria"),
        City("keswick", "Keswick", 54.5995, -3.1326, "uk", "England", "Cumbria"),
        City("king-s-lynn", "King's Lynn", 52.7517, 0.3952, "uk", "England", "Norfolk"),
        City("kirkcaldy", "Kirkcaldy area", 56.1168, -3.1600, "uk", "Scotland"),
        City("kirkwall", "Kirkwall area", 58.9848, -2.9587, "uk", "Scotland"),
        City("lancaster", "Lancaster area", 54.0465, -2.7999, "uk", "England", "Lancashire"),
        City("leamington-spa", "Leamington Spa", 52.2852, -1.5200, "uk", "England", "Warwickshire"),
        City("leiston", "Leiston area", 52.2061, 1.5776, "uk", "England", "Suffolk"),
        City("lerwick", "Lerwick", 60.1534, -1.1443, "uk", "Scotland"),
        City("letchworth-garden-city", "Letchworth Garden City area", 51.9794, -0.2266, "uk", "England", "Hertfordshire"),
        City("lisburn", "Lisburn area", 54.5234, -6.0353, "uk", "Northern Ireland"),
        City("livingston", "Livingston area", 55.9029, -3.5226, "uk", "Scotland", "Lothian"),
        City("llanelli", "Llanelli area", 51.6820, -4.1619, "uk", "Wales", "Dyfed"),
        City("londonderry", "Londonderry", 54.9981, -7.3093, "uk", "Northern Ireland"),
        City("loughborough", "Loughborough", 52.7667, -1.2000, "uk", "England", "Leicestershire"),
        City("lowestoft", "Lowestoft", 52.4752, 1.7517, "uk", "England", "Suffolk"),
        City("ludlow", "Ludlow area", 52.3743, -2.7131, "uk", "England", "Shropshire"),
        City("lytham-st-annes", "Lytham St Annes", 53.7426, -2.9970, "uk", "England", "Lancashire"),
        City("macclesfield", "Macclesfield area", 53.2602, -2.1256, "uk", "England", "Cheshire"),
        City("maghera", "Maghera", 54.8439, -6.6715, "uk", "Northern Ireland", "Mid Ulster"),
        City("maidenhead", "Maidenhead area", 51.5228, -0.7199, "uk", "England", "Berkshire"),
        City("malton", "Malton", 54.1369, -0.7996, "uk", "England", "North Yorkshire"),
        City("malvern", "Malvern", 52.1116, -2.3251, "uk", "England", "Worcestershire"),
        City("marlborough", "Marlborough", 51.4203, -1.7295, "uk", "England", "Wiltshire"),
        City("merthyr-tydfil", "Merthyr Tydfil area", 51.7479, -3.3778, "uk", "Wales", "Glamorgan"),
        City("newbury", "Newbury", 51.4015, -1.3247, "uk", "England", "Berkshire"),
        City("newmarket", "Newmarket", 52.2447, 0.4042, "uk", "England", "Suffolk"),
        City("newport-isle-of-wight", "Newport area", 50.7015, -1.2912, "uk", "England"),
        City("newquay", "Newquay", 50.4156, -5.0732, "uk", "England", "Cornwall"),
        City("newry", "Newry", 54.1784, -6.3374, "uk", "Northern Ireland"),
        City("newton-stewart", "Newton Stewart", 54.9578, -4.4832, "uk", "Scotland", "Dumfries and Galloway"),
        City("northallerton", "Northallerton", 54.3390, -1.4324, "uk", "England", "North Yorkshire"),
        City("nuneaton", "Nuneaton area", 52.5232, -1.4652, "uk", "England", "Warwickshire"),
        City("okehampton", "Okehampton", 50.7384, -4.0016, "uk", "England", "Devon"),
        City("omagh", "Omagh", 54.6000, -7.3000, "uk", "Northern Ireland", "Fermanagh and Omagh"),
        City("paisley", "Paisley area", 55.8317, -4.4325, "uk", "Scotland"),
        City("penzance", "Penzance", 50.1186, -5.5371, "uk", "England", "Cornwall"),
        City("perth", "Perth", 56.3952, -3.4314, "uk", "Scotland", "Tayside"),
        City("peterhead", "Peterhead area", 57.5052, -1.7844, "uk", "Scotland", "Aberdeenshire"),
        City("portree", "Portree", 57.4129, -6.1942, "uk", "Scotland", "Highland"),
        City("pwllheli", "Pwllheli area", 52.8899, -4.4145, "uk", "Wales", "Gwynedd"),
        City("redditch", "Redditch area", 52.3065, -1.9457, "uk", "England", "Worcestershire"),
        City("redhill", "Redhill area", 51.2405, -0.1704, "uk", "England", "Surrey"),
        City("redruth", "Redruth area", 50.2332, -5.2243, "uk", "England", "Cornwall"),
        City("rochdale", "Rochdale area", 53.6177, -2.1552, "uk", "England", "Greater Manchester"),
        City("ross-on-wye", "Ross-on-Wye area", 51.9167, -2.5667, "uk", "England", "Herefordshire"),
        City("rothesay", "Rothesay", 55.8365, -5.0551, "uk", "Scotland", "Argyll and Bute"),
        City("salisbury", "Salisbury", 51.0693, -1.7957, "uk", "England", "Wiltshire"),
        City("scarborough", "Scarborough", 54.2797, -0.4044, "uk", "England", "North Yorkshire"),
        City("scunthorpe", "Scunthorpe area", 53.5791, -0.6544, "uk", "England", "Lincolnshire"),
        City("shaftesbury", "Shaftesbury area", 51.0053, -2.1933, "uk", "England", "Dorset"),
        City("shrewsbury", "Shrewsbury", 52.7101, -2.7521, "uk", "England", "Shropshire"),
        City("skegness", "Skegness area", 53.1436, 0.3363, "uk", "England", "Lincolnshire"),
        City("south-shields", "South Shields area", 54.9986, -1.4323, "uk", "England", "Tyne and Wear"),
        City("southport", "Southport", 53.6458, -3.0101, "uk", "England", "Merseyside"),
        City("st-austell", "St Austell area", 50.3425, -4.7744, "uk", "England", "Cornwall"),
        City("st-helier", "St Helier", 49.1880, -2.1049, "uk", "Crown Dependencies"),
        City("st-neots", "St Neots area", 52.2167, -0.2667, "uk", "England", "Cambridgeshire"),
        City("st-peter-port", "St Peter Port area", 49.4598, -2.5353, "uk", "Crown Dependencies"),
        City("stirling", "Stirling area", 56.1190, -3.9368, "uk", "Scotland"),
        City("stockton-on-tees", "Stockton-on-Tees", 54.5685, -1.3187, "uk", "England", "County Durham"),
        City("stornoway", "Stornoway", 58.2092, -6.3865, "uk", "Scotland"),
        City("stroud", "Stroud area", 51.7500, -2.2000, "uk", "England", "Gloucestershire"),
        City("swadlincote", "Swadlincote area", 52.7740, -1.5574, "uk", "England", "Derbyshire"),
        City("taunton", "Taunton area", 51.0149, -3.1029, "uk", "England", "Somerset"),
        City("thetford", "Thetford area", 52.4167, 0.7500, "uk", "England", "Norfolk"),
        City("thirsk", "Thirsk", 54.2330, -1.3441, "uk", "England", "North Yorkshire"),
        City("thurso", "Thurso", 58.5927, -3.5259, "uk", "Scotland", "Highland"),
        City("tiverton", "Tiverton area", 50.9024, -3.4923, "uk", "England", "Devon"),
        City("torquay", "Torquay area", 50.4620, -3.5252, "uk", "England", "Devon"),
        City("totnes", "Totnes area", 50.4311, -3.6843, "uk", "England", "Devon"),
        City("trowbridge", "Trowbridge area", 51.3189, -2.2086, "uk", "England", "Wiltshire"),
        City("tunbridge-wells", "Tunbridge Wells area", 51.1332, 0.2626, "uk", "England", "Kent"),
        City("wellington", "Wellington area", 52.7000, -2.5167, "uk", "England", "Shropshire"),
        City("weston-super-mare", "Weston-super-Mare", 51.3460, -2.9766, "uk", "England", "Somerset"),
        City("whitby", "Whitby", 54.4877, -0.6150, "uk", "England", "North Yorkshire"),
        City("whitehaven", "Whitehaven", 54.5490, -3.5841, "uk", "England", "Cumbria"),
        City("widnes", "Widnes area", 53.3618, -2.7341, "uk", "England", "Cheshire"),
        City("wisbech", "Wisbech", 52.6662, 0.1594, "uk", "England", "Cambridgeshire"),
        City("woodbridge", "Woodbridge", 52.0933, 1.3204, "uk", "England", "Suffolk"),
        City("workington", "Workington", 54.6425, -3.5441, "uk", "England", "Cumbria"),
        City("worksop", "Worksop", 53.3018, -1.1240, "uk", "England", "Nottinghamshire"),
        City("wrexham", "Wrexham area", 53.0466, -2.9913, "uk", "Wales", "Clwyd"),
        City("yeovil", "Yeovil area", 50.9416, -2.6321, "uk", "England", "Somerset"),
        // ── end United Kingdom (generated)
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

/** [inCountry] ordered alphabetically under that country's collation (Polish for
 *  `pl`, so `Ł` sorts after `L`; English elsewhere) — what the pickers show. */
fun List<City>.sortedForPicker(countryCode: String): List<City> {
    val collator = java.text.Collator.getInstance(collationLocale(countryCode))
    return inCountry(countryCode).sortedWith(compareBy(collator) { it.name })
}

/** The regions [countryCode]'s cities are grouped under — US states, German
 *  Bundesländer, UK nations — in the catalog's own order, which is the order the
 *  web picker lists them in, so the two read alike. Empty for a country that does
 *  not group its cities (Poland, Spain), and that emptiness is what the picker
 *  reads as "show one flat list".
 *
 *  The TOP level, even where a region nests a `subregion` below it (see
 *  [secondLevelRows]): the UK's nation names every one of its cities' `region`,
 *  whether or not the county underneath went on to earn a third step of its
 *  own. */
fun List<City>.regionsIn(countryCode: String): List<String> =
    inCountry(countryCode).mapNotNull { it.region }.distinct()

/** An abstract group-level identifier — never the localized noun itself, so
 *  [searchLevelsFor] stays a pure JVM function the caller maps to its own
 *  `R.string` (`CitySearchLevel.labelRes` in `CityChoiceScreen.kt`). Mirrors
 *  `PICKER_GROUP_LABELS`' two entries per nesting country in landing.scala.html. */
enum class CitySearchLevel { UkRegion, UkSubregion, UsRegion, DeRegion }

/** [CitySearchLevel]s for a country that groups its cities, deepest first —
 *  the UK nests two ([UkRegion] then [UkSubregion]); the US and Germany nest
 *  one. A country absent here (Poland, Spain) is flat. */
private val countrySearchLevels: Map<String, Pair<CitySearchLevel, CitySearchLevel?>> = mapOf(
    "uk" to (CitySearchLevel.UkRegion to CitySearchLevel.UkSubregion),
    "us" to (CitySearchLevel.UsRegion to null),
    "de" to (CitySearchLevel.DeRegion to null),
)

/** The group level(s) a search from the CURRENT step can still reach, deepest
 *  first — mirrors `pickerSearchLevels()` in landing.scala.html exactly, so
 *  the web and the app phrase the search box's prompt identically: two at
 *  the root of a country that nests two deep (the UK), one once a region is
 *  picked or for a country with only one level (US/Germany), none once fully
 *  drilled into a subregion or for a flat country (Poland, Spain). */
fun searchLevelsFor(countryCode: String, region: String?, subregion: String?): List<CitySearchLevel> {
    val levels = countrySearchLevels[countryCode] ?: return emptyList()
    if (region == null) {
        return levels.second?.let { listOf(levels.first, it) } ?: listOf(levels.first)
    }
    if (subregion == null) {
        return levels.second?.let { listOf(it) } ?: emptyList()
    }
    return emptyList()
}

/** [matching] confined to one [region] — used by [matchingInSubregion] below.
 *  A null [region] leaves the country-wide list alone, so the same call serves
 *  both a grouped country's third screen and an ungrouped country's only one. */
fun List<City>.matchingInRegion(query: String, countryCode: String, region: String?): List<City> =
    matching(query, countryCode).let { cities ->
        if (region == null) cities else cities.filter { it.region == region }
    }

/** [matchingInRegion] narrowed to one [subregion] — the THIRD step, reached
 *  only where [secondLevelRows] holds a heading for it. */
fun List<City>.matchingInSubregion(query: String, countryCode: String, region: String, subregion: String): List<City> =
    matchingInRegion(query, countryCode, region).filter { it.subregion == subregion }

/** One row of a grouped picker level: a group heading (drills further) or a
 *  city that carries no group at this level, either because its top group
 *  collapsed onto it alone (Berlin, Hamburg; Delaware, Vermont) or because the
 *  country/region simply isn't split any further here. */
sealed class CityPickerRow {
    data class Heading(val label: String) : CityPickerRow()
    data class CityRow(val city: City) : CityPickerRow()
}

/** A stable `LazyColumn` item key — the heading's own label, or the city's
 *  slug, either way unique within one picker level. */
val CityPickerRow.rowKey: String
    get() = when (this) {
        is CityPickerRow.Heading -> "heading:$label"
        is CityPickerRow.CityRow -> "city:${city.slug}"
    }

/** One pass over [cities] — ALREADY in the catalog's own canonical picker
 *  order (`Catalog.scala` emits each country's cities in its picker tree's own
 *  traversal order; never re-sorted here) — interleaving each group's
 *  heading, emitted at its FIRST occurrence, with every city that carries no
 *  group at this level. Listing every heading before every direct city (the
 *  old shape) stranded a heading like "West Midlands" ahead of every direct
 *  city in its region, instead of beside "West Sussex" where it
 *  alphabetically belongs. */
private fun mergedPickerRows(cities: List<City>, groupField: (City) -> String?, query: String): List<CityPickerRow> {
    val q = Cities.searchFold(query.trim())
    val seen = mutableSetOf<String>()
    val rows = mutableListOf<CityPickerRow>()
    for (city in cities) {
        val group = groupField(city)
        if (group != null) {
            if (seen.add(group) && (q.isEmpty() || Cities.searchFold(group).contains(q))) {
                rows.add(CityPickerRow.Heading(group))
            }
        } else if (q.isEmpty() || city.matchesFolded(q)) {
            rows.add(CityPickerRow.CityRow(city))
        }
    }
    return rows
}

/** The FIRST picker step for a grouped country: each region's heading,
 *  interleaved with any city whose top group collapsed onto it alone (Berlin,
 *  Hamburg; Delaware, Vermont) — narrowed to [query]. Empty for a country that
 *  does not group its cities (Poland, Spain). */
fun List<City>.topLevelRows(query: String, countryCode: String): List<CityPickerRow> =
    mergedPickerRows(inCountry(countryCode), groupField = { it.region }, query = query)

/** The SECOND picker step, within one [region]: each subregion's heading
 *  (reached only for the UK's counties today: Glamorgan, West Midlands…)
 *  interleaved with that region's own directly-listed cities — narrowed to
 *  [query]. */
fun List<City>.secondLevelRows(query: String, countryCode: String, region: String): List<CityPickerRow> =
    mergedPickerRows(inCountry(countryCode).filter { it.region == region }, groupField = { it.subregion }, query = query)

/** One row of a FLATTENED, whole-country search result — see [searchRows].
 *  Unlike [CityPickerRow], a subregion here carries its parent [Subregion.region]:
 *  picking it from a flat list has to land two drill-steps deep at once, not
 *  just one, so the caller can't recover the region from its own current
 *  state the way [CityPickerRow]'s single-level callers can. */
sealed class CitySearchRow {
    abstract val label: String
    data class Region(override val label: String) : CitySearchRow()
    data class Subregion(override val label: String, val region: String) : CitySearchRow()
    data class CityRow(val city: City) : CitySearchRow() {
        override val label: String get() = city.name
    }
}

/** Every region, subregion and city in [countryCode] matching [query], merged
 *  into ONE alphabetical list — a query for "kent" must surface the Kent
 *  COUNTY heading even from the UK's top level, not only once England is
 *  already open. Empty for a blank [query]: [topLevelRows]/[secondLevelRows]
 *  above handle that case, and a country with no grouping (Poland, Spain) has
 *  nothing to flatten in the first place — its own [matching] already reaches
 *  every city. */
fun List<City>.searchRows(query: String, countryCode: String): List<CitySearchRow> {
    val q = Cities.searchFold(query.trim())
    if (q.isEmpty()) return emptyList()
    val seenRegion = mutableSetOf<String>()
    val seenSubregion = mutableSetOf<String>()
    val rows = mutableListOf<CitySearchRow>()
    for (city in inCountry(countryCode)) {
        val region = city.region
        if (region != null && seenRegion.add(region) && Cities.searchFold(region).contains(q)) {
            rows.add(CitySearchRow.Region(region))
        }
        val subregion = city.subregion
        if (region != null && subregion != null && seenSubregion.add("$region\u0000$subregion") &&
            Cities.searchFold(subregion).contains(q)
        ) {
            rows.add(CitySearchRow.Subregion(subregion, region))
        }
        if (city.matchesFolded(q)) {
            rows.add(CitySearchRow.CityRow(city))
        }
    }
    val collator = java.text.Collator.getInstance(collationLocale(countryCode))
    return rows.sortedWith(compareBy(collator) { it.label })
}

/** A stable `LazyColumn` item key — unique within one flattened search result,
 *  same role as [rowKey] for [CityPickerRow]. A subregion's key includes its
 *  region since two different regions could in principle share a subregion
 *  name. */
val CitySearchRow.rowKey: String
    get() = when (this) {
        is CitySearchRow.Region -> "region:$label"
        is CitySearchRow.Subregion -> "subregion:$region:$label"
        is CitySearchRow.CityRow -> "city:${city.slug}"
    }

/** [sortedForPicker] narrowed to the cities matching [query] (case- and
 *  diacritic-insensitive substring). A blank query yields the whole country list. */
fun List<City>.matching(query: String, countryCode: String): List<City> {
    val q = Cities.searchFold(query.trim())
    val cities = sortedForPicker(countryCode)
    return if (q.isEmpty()) cities else cities.filter { it.matchesFolded(q) }
}

/** The default city for [countryCode] — its first entry in this list, or null. */
fun List<City>.defaultCity(countryCode: String): City? = inCountry(countryCode).firstOrNull()

/** The city IN [countryCode] nearest ([lat], [lon]), or null beyond 100 km.
 *  Scoped so a Polish fix never resolves to a UK region, or vice versa. */
fun List<City>.nearestWithin100km(lat: Double, lon: Double, countryCode: String): City? =
    inCountry(countryCode).minByOrNull { haversineKm(lat, lon, it.lat, it.lon) }
        ?.takeIf { haversineKm(lat, lon, it.lat, it.lon) <= 100.0 }

/** The city nearest ([lat], [lon]) across EVERY country this list carries, or
 *  null beyond 100 km. Unlike [nearestWithin100km]'s country-scoped overload,
 *  this is deliberately unscoped — the manual picker's "use my location"
 *  button uses it to find the right city even when the country tab currently
 *  open isn't the one the device is actually in (a UK visitor who hasn't
 *  switched off the default Poland tab yet). */
fun List<City>.nearestWithin100km(lat: Double, lon: Double): City? =
    minByOrNull { haversineKm(lat, lon, it.lat, it.lon) }
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
