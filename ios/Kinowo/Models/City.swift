import Foundation

/// A city the app can show a repertoire for. The server hosts every page
/// and API under a `/{slug}/…` prefix, so the slug is what the networking
/// layer splices into the repertoire / details URLs; `name` is the display
/// label; `lat`/`lon` drive the nearest-city pick on first launch.
///
/// Pure Foundation — no SwiftUI / CoreLocation — so it lives in `KinowoCore`
/// and the haversine pick is unit-testable cross-platform (`swift test`
/// on Linux CI).
struct City: Codable, Hashable {
    let slug: String
    let name: String
    let lat: Double
    let lon: Double
    /// ISO country code (`"PL"`, `"GB"`) matching `Country.code`. The app is
    /// multi-country: every city picker, the nearest-city gate, and the default
    /// city are scoped to the SELECTED country's cities, so a UK user browses UK
    /// regions and a Polish user browses Polish cities — never a mix.
    let country: String
    /// The group this city is picked under, where its country groups them at all
    /// — a US state ("California"), a UK nation ("Scotland") — and `nil` in the
    /// countries that group nothing. Neither 2,000 US places nor 260 UK pages in
    /// one A-to-Z is a list anybody reads, so the picker asks for the group
    /// first; a country without regions keeps the single flat list.
    ///
    /// Carried by `/api/catalog` (and the bundled seed). A `var` with a default
    /// so the memberwise init stays usable by the hand-written `all` rows below,
    /// and so a catalog from an older server still decodes.
    var region: String? = nil

    /// The SECOND-level group `region` sits under a further split, where that
    /// split actually holds more than one city — a UK county holding
    /// several pages (Glamorgan: Cardiff, Swansea…).
    /// `nil` everywhere else: a county that collapsed onto its
    /// one place reads correctly through `region` alone (Shetland needs no
    /// third tap to reach Lerwick), and Germany/the US never nest this deep.
    ///
    /// Carried by `/api/catalog` (and the bundled seed), same shape as `region`.
    var subregion: String? = nil

    /// Other names search should find this city by: the group it was pulled up
    /// out of when that group held only it (Guam over "Dededo Village area",
    /// Merseyside over Liverpool), and pages folded into it. The web's picker
    /// reads the same list (`data-alias`). `nil` almost everywhere.
    ///
    /// Carried by `/api/catalog` (and the bundled seed), same shape as `region`.
    var aliases: [String]? = nil

    /// This city's own IANA zone, where it differs from its country's; `nil`
    /// otherwise — which is every city of the four countries that keep one zone
    /// throughout, so the field costs nothing there.
    ///
    /// The US spans SIX zones and fifteen of its states straddle a boundary, so a
    /// single country zone is wrong for most of it: pruning a Knoxville showtime
    /// on the country's Pacific drops it three hours early. Resolve through
    /// [`zone(fallback:)`], never by reading this directly.
    ///
    /// Carried by `/api/catalog` (and the bundled seed), same shape as `region`:
    /// a `var` with a default so the memberwise init stays usable by the
    /// hand-written `all` rows below, and so a catalog from an older server —
    /// or any city of a single-zone country — still decodes.
    var timezone: String? = nil

    /// Slugs this city used to answer at, which the web now 301s onto it — a
    /// retired page's (`miedzyrzec-podlaski` → `biala-podlaska`), a renamed
    /// metro's. Universal Links hand those URLs to the app without ever reaching
    /// that redirect, so `DeepLink.parse` resolves them through here instead.
    /// Carried by `/api/catalog` (and the bundled seed), same shape as `region`.
    var formerSlugs: [String]? = nil

    /// The wall-clock this city's showtimes are listed on: its own zone when the
    /// catalog gave it one, else the country's. Past-showtime pruning and the
    /// Dziś/Jutro buckets reason in this, so it has to be the CITY's — a country
    /// only has one to offer and the US needs six.
    func zone(fallback: TimeZone) -> TimeZone {
        timezone.flatMap { TimeZone(identifier: $0) } ?? fallback
    }

    /// Every city the app knows about across ALL countries, in the same order
    /// the web `City.all` lists them (Polish cities first, then the UK regions)
    /// so the pickers read identically across platforms. The data-driven rosters
    /// — Germany's 158 regions and the US's 55 states/territories — are NOT
    /// mirrored here: they arrive with the catalog (bundled seed, then the live
    /// `/api/catalog`), so there is one copy of them rather than two that drift. This is the GLOBAL
    /// union; the pickers and the nearest-city gate scope it to the selected
    /// country via `cities(in:)` / `matching(_:in:)` / `nearestWithin100km(...in:)`.
    /// Adding a row here (with its `country`) is the only model-side change a new
    /// city needs.
    static let all: [City] = [
        // ── Poland: GENERATED by data/pl/scripts/generate_polish_pages.py — do not edit by hand.
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252, country: "pl", region: "Wielkopolskie"),
        City(slug: "wroclaw", name: "Wrocław", lat: 51.1079, lon: 17.0385, country: "pl", region: "Dolnośląskie"),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122, country: "pl", region: "Mazowieckie"),
        City(slug: "krakow", name: "Kraków", lat: 50.0647, lon: 19.9450, country: "pl", region: "Małopolskie"),
        City(slug: "lodz", name: "Łódź", lat: 51.7592, lon: 19.4560, country: "pl", region: "Łódzkie"),
        City(slug: "katowice", name: "Katowice", lat: 50.2649, lon: 19.0238, country: "pl", region: "Śląskie"),
        City(slug: "szczecin", name: "Szczecin", lat: 53.4285, lon: 14.5528, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "bialystok", name: "Białystok", lat: 53.1325, lon: 23.1688, country: "pl", region: "Podlaskie"),
        City(slug: "trojmiasto", name: "Trójmiasto", lat: 54.4416, lon: 18.5601, country: "pl", region: "Pomorskie"),
        City(slug: "bydgoszcz", name: "Bydgoszcz", lat: 53.1235, lon: 18.0084, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "lublin", name: "Lublin", lat: 51.2465, lon: 22.5684, country: "pl", region: "Lubelskie"),
        City(slug: "czestochowa", name: "Częstochowa", lat: 50.8118, lon: 19.1203, country: "pl", region: "Śląskie"),
        City(slug: "radom", name: "Radom", lat: 51.4027, lon: 21.1471, country: "pl", region: "Mazowieckie"),
        City(slug: "sosnowiec", name: "Sosnowiec", lat: 50.2863, lon: 19.1041, country: "pl", region: "Śląskie"),
        City(slug: "torun", name: "Toruń", lat: 53.0138, lon: 18.5984, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "kielce", name: "Kielce", lat: 50.8661, lon: 20.6286, country: "pl", region: "Świętokrzyskie"),
        City(slug: "rzeszow", name: "Rzeszów", lat: 50.0413, lon: 21.9990, country: "pl", region: "Podkarpackie"),
        City(slug: "gliwice", name: "Gliwice", lat: 50.2945, lon: 18.6714, country: "pl", region: "Śląskie"),
        City(slug: "zabrze", name: "Zabrze", lat: 50.3249, lon: 18.7857, country: "pl", region: "Śląskie"),
        City(slug: "olsztyn", name: "Olsztyn", lat: 53.7784, lon: 20.4801, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "bielsko-biala", name: "Bielsko-Biała", lat: 49.8224, lon: 19.0584, country: "pl", region: "Śląskie"),
        City(slug: "opole", name: "Opole", lat: 50.6751, lon: 17.9213, country: "pl", region: "Opolskie"),
        City(slug: "rybnik", name: "Rybnik", lat: 50.0971, lon: 18.5416, country: "pl", region: "Śląskie"),
        City(slug: "gorzow-wielkopolski", name: "Gorzów Wielkopolski", lat: 52.7368, lon: 15.2288, country: "pl", region: "Lubuskie"),
        City(slug: "elblag", name: "Elbląg", lat: 54.1522, lon: 19.4088, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "koszalin", name: "Koszalin", lat: 54.1943, lon: 16.1722, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "kalisz", name: "Kalisz", lat: 51.7611, lon: 18.0911, country: "pl", region: "Wielkopolskie"),
        City(slug: "zielona-gora", name: "Zielona Góra", lat: 51.9356, lon: 15.5062, country: "pl", region: "Lubuskie"),
        City(slug: "tychy", name: "Tychy", lat: 50.1357, lon: 18.9985, country: "pl", region: "Śląskie"),
        City(slug: "walbrzych", name: "Wałbrzych", lat: 50.7714, lon: 16.2845, country: "pl", region: "Dolnośląskie"),
        City(slug: "tarnow", name: "Tarnów", lat: 50.0121, lon: 20.9858, country: "pl", region: "Małopolskie"),
        City(slug: "wloclawek", name: "Włocławek", lat: 52.6483, lon: 19.0677, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "legnica", name: "Legnica", lat: 51.2070, lon: 16.1619, country: "pl", region: "Dolnośląskie"),
        City(slug: "plock", name: "Płock", lat: 52.5468, lon: 19.7064, country: "pl", region: "Mazowieckie"),
        City(slug: "bytom", name: "Bytom", lat: 50.3483, lon: 18.9157, country: "pl", region: "Śląskie"),
        City(slug: "dabrowa-gornicza", name: "Dąbrowa Górnicza", lat: 50.3219, lon: 19.1876, country: "pl", region: "Śląskie"),
        City(slug: "nowy-sacz", name: "Nowy Sącz", lat: 49.6175, lon: 20.7154, country: "pl", region: "Małopolskie"),
        City(slug: "slupsk", name: "Słupsk", lat: 54.4641, lon: 17.0287, country: "pl", region: "Pomorskie"),
        City(slug: "jelenia-gora", name: "Jelenia Góra", lat: 50.9044, lon: 15.7197, country: "pl", region: "Dolnośląskie"),
        City(slug: "przemysl", name: "Przemyśl", lat: 49.7838, lon: 22.7677, country: "pl", region: "Podkarpackie"),
        City(slug: "konin", name: "Konin", lat: 52.2230, lon: 18.2511, country: "pl", region: "Wielkopolskie"),
        City(slug: "augustow", name: "Augustów", lat: 53.8432, lon: 22.9798, country: "pl", region: "Podlaskie"),
        City(slug: "belchatow", name: "Bełchatów", lat: 51.3688, lon: 19.3567, country: "pl", region: "Łódzkie"),
        City(slug: "biala-podlaska", name: "Biała Podlaska i okolice", lat: 52.0324, lon: 23.1165, country: "pl", region: "Lubelskie"),
        City(slug: "bielsk-podlaski", name: "Bielsk Podlaski", lat: 52.7651, lon: 23.1865, country: "pl", region: "Podlaskie"),
        City(slug: "bilgoraj", name: "Biłgoraj", lat: 50.5411, lon: 22.7220, country: "pl", region: "Lubelskie"),
        City(slug: "bochnia", name: "Bochnia i okolice", lat: 49.9691, lon: 20.4303, country: "pl", region: "Małopolskie"),
        City(slug: "boleslawiec", name: "Bolesławiec i okolice", lat: 51.2642, lon: 15.5697, country: "pl", region: "Dolnośląskie"),
        City(slug: "braniewo", name: "Braniewo", lat: 54.3797, lon: 19.8196, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "brzeg", name: "Brzeg", lat: 50.8608, lon: 17.4674, country: "pl", region: "Opolskie"),
        City(slug: "brzeg-dolny", name: "Brzeg Dolny i okolice", lat: 51.2730, lon: 16.7081, country: "pl", region: "Dolnośląskie"),
        City(slug: "busko-zdroj", name: "Busko-Zdrój i okolice", lat: 50.4708, lon: 20.7188, country: "pl", region: "Świętokrzyskie"),
        City(slug: "chelm", name: "Chełm", lat: 51.1431, lon: 23.4716, country: "pl", region: "Lubelskie"),
        City(slug: "chojnice", name: "Chojnice i okolice", lat: 53.6955, lon: 17.5570, country: "pl", region: "Pomorskie"),
        City(slug: "choszczno", name: "Choszczno i okolice", lat: 53.1690, lon: 15.4205, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "ciechanow", name: "Ciechanów i okolice", lat: 52.8814, lon: 20.6200, country: "pl", region: "Mazowieckie"),
        City(slug: "czechowice-dziedzice", name: "Czechowice-Dziedzice i okolice", lat: 49.9134, lon: 19.0048, country: "pl", region: "Śląskie"),
        City(slug: "dabrowa-tarnowska", name: "Dąbrowa Tarnowska i okolice", lat: 50.1746, lon: 20.9863, country: "pl", region: "Małopolskie"),
        City(slug: "elk", name: "Ełk", lat: 53.8282, lon: 22.3647, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "gizycko", name: "Giżycko i okolice", lat: 54.0381, lon: 21.7644, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "glogow", name: "Głogów", lat: 51.6636, lon: 16.0845, country: "pl", region: "Dolnośląskie"),
        City(slug: "gniezno", name: "Gniezno i okolice", lat: 52.5348, lon: 17.5826, country: "pl", region: "Wielkopolskie"),
        City(slug: "goldap", name: "Gołdap", lat: 54.3063, lon: 22.3036, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "grajewo", name: "Grajewo", lat: 53.6473, lon: 22.4554, country: "pl", region: "Podlaskie"),
        City(slug: "grudziadz", name: "Grudziądz i okolice", lat: 53.4841, lon: 18.7537, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "hrubieszow", name: "Hrubieszów", lat: 50.8050, lon: 23.8925, country: "pl", region: "Lubelskie"),
        City(slug: "ilawa", name: "Iława i okolice", lat: 53.5960, lon: 19.5685, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "inowroclaw", name: "Inowrocław i okolice", lat: 52.7989, lon: 18.2639, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "jaroslaw", name: "Jarosław", lat: 50.0162, lon: 22.6778, country: "pl", region: "Podkarpackie"),
        City(slug: "jaslo", name: "Jasło i okolice", lat: 49.7451, lon: 21.4725, country: "pl", region: "Podkarpackie"),
        City(slug: "jastrzebie-zdroj", name: "Jastrzębie-Zdrój i okolice", lat: 49.9554, lon: 18.5748, country: "pl", region: "Śląskie"),
        City(slug: "jaworzno", name: "Jaworzno i okolice", lat: 50.2053, lon: 19.2750, country: "pl", region: "Śląskie"),
        City(slug: "kedzierzyn-kozle", name: "Kędzierzyn-Koźle", lat: 50.3498, lon: 18.2261, country: "pl", region: "Opolskie"),
        City(slug: "kepno", name: "Kępno i okolice", lat: 51.2784, lon: 17.9891, country: "pl", region: "Wielkopolskie"),
        City(slug: "klodzko", name: "Kłodzko i okolice", lat: 50.4349, lon: 16.6614, country: "pl", region: "Dolnośląskie"),
        City(slug: "kluczbork", name: "Kluczbork", lat: 50.9728, lon: 18.2182, country: "pl", region: "Opolskie"),
        City(slug: "kolobrzeg", name: "Kołobrzeg i okolice", lat: 54.1756, lon: 15.5834, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "koscierzyna", name: "Kościerzyna i okolice", lat: 54.1223, lon: 17.9812, country: "pl", region: "Pomorskie"),
        City(slug: "krapkowice", name: "Krapkowice", lat: 50.4751, lon: 17.9654, country: "pl", region: "Opolskie"),
        City(slug: "krasnik", name: "Kraśnik i okolice", lat: 50.9236, lon: 22.2271, country: "pl", region: "Lubelskie"),
        City(slug: "krasnystaw", name: "Krasnystaw", lat: 50.9846, lon: 23.1742, country: "pl", region: "Lubelskie"),
        City(slug: "krosno", name: "Krosno", lat: 49.6887, lon: 21.7706, country: "pl", region: "Podkarpackie"),
        City(slug: "krotoszyn", name: "Krotoszyn i okolice", lat: 51.6987, lon: 17.4374, country: "pl", region: "Wielkopolskie"),
        City(slug: "krynica-zdroj", name: "Krynica-Zdrój i okolice", lat: 49.4222, lon: 20.9594, country: "pl", region: "Małopolskie"),
        City(slug: "kutno", name: "Kutno i okolice", lat: 52.2306, lon: 19.3641, country: "pl", region: "Łódzkie"),
        City(slug: "lapy", name: "Łapy", lat: 52.9911, lon: 22.8842, country: "pl", region: "Podlaskie"),
        City(slug: "leba", name: "Łeba", lat: 54.7610, lon: 17.5555, country: "pl", region: "Pomorskie"),
        City(slug: "legionowo", name: "Legionowo i okolice", lat: 52.4015, lon: 20.9266, country: "pl", region: "Mazowieckie"),
        City(slug: "leszno", name: "Leszno", lat: 51.8403, lon: 16.5749, country: "pl", region: "Wielkopolskie"),
        City(slug: "lidzbark-warminski", name: "Lidzbark Warmiński", lat: 54.1259, lon: 20.5795, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "lipno", name: "Lipno i okolice", lat: 52.8444, lon: 19.1785, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "lomza", name: "Łomża i okolice", lat: 53.1781, lon: 22.0593, country: "pl", region: "Podlaskie"),
        City(slug: "lubin", name: "Lubin", lat: 51.4009, lon: 16.2015, country: "pl", region: "Dolnośląskie"),
        City(slug: "lubliniec", name: "Lubliniec", lat: 50.6690, lon: 18.6844, country: "pl", region: "Śląskie"),
        City(slug: "lukow", name: "Łuków", lat: 51.9290, lon: 22.3796, country: "pl", region: "Lubelskie"),
        City(slug: "miechow", name: "Miechów i okolice", lat: 50.3565, lon: 20.0279, country: "pl", region: "Małopolskie"),
        City(slug: "miedzyrzecz", name: "Międzyrzecz", lat: 52.4446, lon: 15.5780, country: "pl", region: "Lubuskie"),
        City(slug: "miedzyzdroje", name: "Międzyzdroje", lat: 53.9292, lon: 14.4510, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "mielec", name: "Mielec i okolice", lat: 50.2871, lon: 21.4239, country: "pl", region: "Podkarpackie"),
        City(slug: "mlawa", name: "Mława i okolice", lat: 53.1128, lon: 20.3841, country: "pl", region: "Mazowieckie"),
        City(slug: "morag", name: "Morąg", lat: 53.9171, lon: 19.9260, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "myszkow", name: "Myszków", lat: 50.5752, lon: 19.3246, country: "pl", region: "Śląskie"),
        City(slug: "naklo-nad-notecia", name: "Nakło nad Notecią i okolice", lat: 53.1421, lon: 17.6018, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "nowa-sol", name: "Nowa Sól", lat: 51.8033, lon: 15.7170, country: "pl", region: "Lubuskie"),
        City(slug: "nowy-targ", name: "Nowy Targ i okolice", lat: 49.4778, lon: 20.0323, country: "pl", region: "Małopolskie"),
        City(slug: "nowy-tomysl", name: "Nowy Tomyśl i okolice", lat: 52.3195, lon: 16.1284, country: "pl", region: "Wielkopolskie"),
        City(slug: "nysa", name: "Nysa i okolice", lat: 50.4738, lon: 17.3344, country: "pl", region: "Opolskie"),
        City(slug: "olawa", name: "Oława", lat: 50.9466, lon: 17.2926, country: "pl", region: "Dolnośląskie"),
        City(slug: "olecko", name: "Olecko", lat: 54.0337, lon: 22.5070, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "olesnica", name: "Oleśnica", lat: 51.2134, lon: 17.3899, country: "pl", region: "Dolnośląskie"),
        City(slug: "olsztynek", name: "Olsztynek", lat: 53.5837, lon: 20.2847, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "opoczno", name: "Opoczno i okolice", lat: 51.3757, lon: 20.2783, country: "pl", region: "Łódzkie"),
        City(slug: "ostrow-mazowiecka", name: "Ostrów Mazowiecka", lat: 52.8025, lon: 21.8951, country: "pl", region: "Mazowieckie"),
        City(slug: "ostrow-wielkopolski", name: "Ostrów Wielkopolski", lat: 51.6550, lon: 17.8069, country: "pl", region: "Wielkopolskie"),
        City(slug: "ostrowiec-swietokrzyski", name: "Ostrowiec Świętokrzyski i okolice", lat: 50.9294, lon: 21.3852, country: "pl", region: "Świętokrzyskie"),
        City(slug: "oswiecim", name: "Oświęcim i okolice", lat: 50.0344, lon: 19.2104, country: "pl", region: "Małopolskie"),
        City(slug: "otwock", name: "Otwock i okolice", lat: 52.1058, lon: 21.2613, country: "pl", region: "Mazowieckie"),
        City(slug: "pabianice", name: "Pabianice", lat: 51.6645, lon: 19.3547, country: "pl", region: "Łódzkie"),
        City(slug: "parczew", name: "Parczew", lat: 51.6402, lon: 22.9006, country: "pl", region: "Lubelskie"),
        City(slug: "pila", name: "Piła i okolice", lat: 53.1514, lon: 16.7378, country: "pl", region: "Wielkopolskie"),
        City(slug: "piotrkow-trybunalski", name: "Piotrków Trybunalski", lat: 51.4055, lon: 19.7032, country: "pl", region: "Łódzkie"),
        City(slug: "police", name: "Police i okolice", lat: 53.5521, lon: 14.5718, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "polkowice", name: "Polkowice", lat: 51.5039, lon: 16.0726, country: "pl", region: "Dolnośląskie"),
        City(slug: "pruszkow", name: "Pruszków i okolice", lat: 52.1707, lon: 20.8121, country: "pl", region: "Mazowieckie"),
        City(slug: "przeworsk", name: "Przeworsk i okolice", lat: 50.0591, lon: 22.4941, country: "pl", region: "Podkarpackie"),
        City(slug: "pulawy", name: "Puławy i okolice", lat: 51.4166, lon: 21.9694, country: "pl", region: "Lubelskie"),
        City(slug: "pultusk", name: "Pułtusk i okolice", lat: 52.7025, lon: 21.0828, country: "pl", region: "Mazowieckie"),
        City(slug: "radomsko", name: "Radomsko", lat: 51.0671, lon: 19.4448, country: "pl", region: "Łódzkie"),
        City(slug: "rawicz", name: "Rawicz i okolice", lat: 51.6095, lon: 16.8585, country: "pl", region: "Wielkopolskie"),
        City(slug: "ruda-slaska", name: "Ruda Śląska i okolice", lat: 50.2584, lon: 18.8563, country: "pl", region: "Śląskie"),
        City(slug: "rypin", name: "Rypin", lat: 53.0660, lon: 19.4094, country: "pl", region: "Kujawsko-pomorskie"),
        City(slug: "sanok", name: "Sanok i okolice", lat: 49.5557, lon: 22.2056, country: "pl", region: "Podkarpackie"),
        City(slug: "siedlce", name: "Siedlce", lat: 52.1677, lon: 22.2901, country: "pl", region: "Mazowieckie"),
        City(slug: "sierpc", name: "Sierpc", lat: 52.8568, lon: 19.6691, country: "pl", region: "Mazowieckie"),
        City(slug: "skierniewice", name: "Skierniewice", lat: 51.9549, lon: 20.1584, country: "pl", region: "Łódzkie"),
        City(slug: "slawno", name: "Sławno i okolice", lat: 54.3628, lon: 16.6789, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "slubice", name: "Słubice", lat: 52.3509, lon: 14.5607, country: "pl", region: "Lubuskie"),
        City(slug: "slupca", name: "Słupca i okolice", lat: 52.2873, lon: 17.8719, country: "pl", region: "Wielkopolskie"),
        City(slug: "sokolka", name: "Sokółka", lat: 53.4072, lon: 23.5023, country: "pl", region: "Podlaskie"),
        City(slug: "sokolow-podlaski", name: "Sokołów Podlaski i okolice", lat: 52.4068, lon: 22.2531, country: "pl", region: "Mazowieckie"),
        City(slug: "srem", name: "Śrem i okolice", lat: 52.0887, lon: 17.0151, country: "pl", region: "Wielkopolskie"),
        City(slug: "stalowa-wola", name: "Stalowa Wola i okolice", lat: 50.5829, lon: 22.0533, country: "pl", region: "Podkarpackie"),
        City(slug: "stara-blotnica", name: "Stara Błotnica", lat: 51.5468, lon: 20.9748, country: "pl", region: "Mazowieckie"),
        City(slug: "starachowice", name: "Starachowice i okolice", lat: 51.0374, lon: 21.0713, country: "pl", region: "Świętokrzyskie"),
        City(slug: "stargard", name: "Stargard i okolice", lat: 53.3367, lon: 15.0499, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "strzelin", name: "Strzelin", lat: 50.7816, lon: 17.0648, country: "pl", region: "Dolnośląskie"),
        City(slug: "suwalki", name: "Suwałki", lat: 54.1118, lon: 22.9309, country: "pl", region: "Podlaskie"),
        City(slug: "swidnica", name: "Świdnica i okolice", lat: 50.8438, lon: 16.4886, country: "pl", region: "Dolnośląskie"),
        City(slug: "swidnik", name: "Świdnik i okolice", lat: 51.2190, lon: 22.6962, country: "pl", region: "Lubelskie"),
        City(slug: "swiebodzin", name: "Świebodzin", lat: 52.2475, lon: 15.5335, country: "pl", region: "Lubuskie"),
        City(slug: "szamotuly", name: "Szamotuły i okolice", lat: 52.6120, lon: 16.5779, country: "pl", region: "Wielkopolskie"),
        City(slug: "szczecinek", name: "Szczecinek", lat: 53.7079, lon: 16.6994, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "szczytno", name: "Szczytno", lat: 53.5626, lon: 20.9875, country: "pl", region: "Warmińsko-mazurskie"),
        City(slug: "tczew", name: "Tczew i okolice", lat: 54.0924, lon: 18.7779, country: "pl", region: "Pomorskie"),
        City(slug: "tomaszow-lubelski", name: "Tomaszów Lubelski", lat: 50.4477, lon: 23.4162, country: "pl", region: "Lubelskie"),
        City(slug: "tomaszow-mazowiecki", name: "Tomaszów Mazowiecki", lat: 51.5313, lon: 20.0085, country: "pl", region: "Łódzkie"),
        City(slug: "turek", name: "Turek i okolice", lat: 52.0155, lon: 18.5006, country: "pl", region: "Wielkopolskie"),
        City(slug: "wadowice", name: "Wadowice i okolice", lat: 49.8834, lon: 19.4929, country: "pl", region: "Małopolskie"),
        City(slug: "wagrowiec", name: "Wągrowiec i okolice", lat: 52.8084, lon: 17.1996, country: "pl", region: "Wielkopolskie"),
        City(slug: "wejherowo", name: "Wejherowo i okolice", lat: 54.6057, lon: 18.2356, country: "pl", region: "Pomorskie"),
        City(slug: "wielen", name: "Wieleń", lat: 52.8946, lon: 16.1714, country: "pl", region: "Wielkopolskie"),
        City(slug: "wieliczka", name: "Wieliczka i okolice", lat: 49.9874, lon: 20.0647, country: "pl", region: "Małopolskie"),
        City(slug: "wielun", name: "Wieluń", lat: 51.2210, lon: 18.5696, country: "pl", region: "Łódzkie"),
        City(slug: "wlodawa", name: "Włodawa", lat: 51.5500, lon: 23.5500, country: "pl", region: "Lubelskie"),
        City(slug: "wloszczowa", name: "Włoszczowa", lat: 50.8526, lon: 19.9659, country: "pl", region: "Świętokrzyskie"),
        City(slug: "wolomin", name: "Wołomin i okolice", lat: 52.3401, lon: 21.2421, country: "pl", region: "Mazowieckie"),
        City(slug: "wysokie-mazowieckie", name: "Wysokie Mazowieckie", lat: 52.9166, lon: 22.5171, country: "pl", region: "Podlaskie"),
        City(slug: "wyszkow", name: "Wyszków i okolice", lat: 52.5928, lon: 21.4584, country: "pl", region: "Mazowieckie"),
        City(slug: "zakopane", name: "Zakopane", lat: 49.2990, lon: 19.9489, country: "pl", region: "Małopolskie"),
        City(slug: "zamosc", name: "Zamość", lat: 50.7231, lon: 23.2520, country: "pl", region: "Lubelskie"),
        City(slug: "zary", name: "Żary", lat: 51.6420, lon: 15.1373, country: "pl", region: "Lubuskie"),
        City(slug: "zawiercie", name: "Zawiercie", lat: 50.4877, lon: 19.4168, country: "pl", region: "Śląskie"),
        City(slug: "zdunska-wola", name: "Zduńska Wola i okolice", lat: 51.5992, lon: 18.9397, country: "pl", region: "Łódzkie"),
        City(slug: "zgierz", name: "Zgierz i okolice", lat: 51.8556, lon: 19.4062, country: "pl", region: "Łódzkie"),
        City(slug: "zgorzelec", name: "Zgorzelec i okolice", lat: 51.1494, lon: 15.0084, country: "pl", region: "Dolnośląskie"),
        City(slug: "zlocieniec", name: "Złocieniec i okolice", lat: 53.5329, lon: 16.0113, country: "pl", region: "Zachodniopomorskie"),
        City(slug: "zlotow", name: "Złotów i okolice", lat: 53.3635, lon: 17.0408, country: "pl", region: "Wielkopolskie"),
        City(slug: "zory", name: "Żory", lat: 50.0452, lon: 18.7006, country: "pl", region: "Śląskie"),
        City(slug: "zyrardow", name: "Żyrardów i okolice", lat: 52.0488, lon: 20.4460, country: "pl", region: "Mazowieckie"),
        City(slug: "zywiec", name: "Żywiec i okolice", lat: 49.6853, lon: 19.1924, country: "pl", region: "Śląskie"),
        // ── end Poland (generated)
        // ── United Kingdom: GENERATED by data/uk/scripts/generate_roster.py — do not edit by hand.
        City(slug: "london", name: "London", lat: 51.5085, lon: -0.1257, country: "uk", region: "England"),
        City(slug: "birmingham", name: "Birmingham", lat: 52.4814, lon: -1.8998, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "glasgow", name: "Glasgow", lat: 55.8651, lon: -4.2576, country: "uk", region: "Scotland"),
        City(slug: "manchester", name: "Manchester", lat: 53.4809, lon: -2.2374, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "sheffield", name: "Sheffield", lat: 53.3830, lon: -1.4659, country: "uk", region: "England", subregion: "South Yorkshire"),
        City(slug: "leeds", name: "Leeds", lat: 53.7965, lon: -1.5478, country: "uk", region: "England", subregion: "West Yorkshire"),
        City(slug: "edinburgh", name: "Edinburgh", lat: 55.9521, lon: -3.1965, country: "uk", region: "Scotland", subregion: "Lothian"),
        City(slug: "liverpool", name: "Liverpool", lat: 53.4106, lon: -2.9779, country: "uk", region: "England", subregion: "Merseyside"),
        City(slug: "bristol", name: "Bristol", lat: 51.4552, lon: -2.5966, country: "uk", region: "England"),
        City(slug: "cardiff", name: "Cardiff", lat: 51.4800, lon: -3.1800, country: "uk", region: "Wales", subregion: "Glamorgan"),
        City(slug: "leicester", name: "Leicester", lat: 52.6386, lon: -1.1317, country: "uk", region: "England", subregion: "Leicestershire"),
        City(slug: "bradford", name: "Bradford", lat: 53.7939, lon: -1.7521, country: "uk", region: "England", subregion: "West Yorkshire"),
        City(slug: "belfast", name: "Belfast", lat: 54.5968, lon: -5.9254, country: "uk", region: "Northern Ireland"),
        City(slug: "coventry", name: "Coventry", lat: 52.4066, lon: -1.5122, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "birkenhead", name: "Birkenhead", lat: 53.3934, lon: -3.0148, country: "uk", region: "England", subregion: "Merseyside"),
        City(slug: "nottingham", name: "Nottingham", lat: 52.9536, lon: -1.1505, country: "uk", region: "England", subregion: "Nottinghamshire"),
        City(slug: "reading-berkshire", name: "Reading", lat: 51.4562, lon: -0.9711, country: "uk", region: "England", subregion: "Berkshire"),
        City(slug: "hull", name: "Hull", lat: 53.7446, lon: -0.3352, country: "uk", region: "England", subregion: "East Riding of Yorkshire"),
        City(slug: "preston-lancashire", name: "Preston", lat: 53.7628, lon: -2.7045, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "swansea", name: "Swansea", lat: 51.6208, lon: -3.9432, country: "uk", region: "Wales", subregion: "Glamorgan"),
        City(slug: "newcastle-upon-tyne", name: "Newcastle upon Tyne", lat: 54.9733, lon: -1.6140, country: "uk", region: "England", subregion: "Tyne and Wear"),
        City(slug: "southend-on-sea", name: "Southend-on-Sea", lat: 51.5378, lon: 0.7143, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "brighton", name: "Brighton", lat: 50.8284, lon: -0.1395, country: "uk", region: "England", subregion: "East Sussex"),
        City(slug: "derby-derbyshire", name: "Derby", lat: 52.9228, lon: -1.4766, country: "uk", region: "England", subregion: "Derbyshire"),
        City(slug: "southampton", name: "Southampton", lat: 50.9040, lon: -1.4043, country: "uk", region: "England", subregion: "Hampshire"),
        City(slug: "wolverhampton", name: "Wolverhampton", lat: 52.5855, lon: -2.1230, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "plymouth-devon", name: "Plymouth", lat: 50.3715, lon: -4.1430, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "stoke-on-trent", name: "Stoke-on-Trent", lat: 53.0042, lon: -2.1854, country: "uk", region: "England", subregion: "Staffordshire"),
        City(slug: "milton-keynes", name: "Milton Keynes", lat: 52.0417, lon: -0.7558, country: "uk", region: "England", subregion: "Buckinghamshire"),
        City(slug: "northampton", name: "Northampton", lat: 52.2500, lon: -0.8833, country: "uk", region: "England", subregion: "Northamptonshire"),
        City(slug: "oldham", name: "Oldham", lat: 53.5405, lon: -2.1183, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "luton", name: "Luton", lat: 51.8797, lon: -0.4175, country: "uk", region: "England", subregion: "Bedfordshire"),
        City(slug: "portsmouth-hampshire", name: "Portsmouth", lat: 50.7990, lon: -1.0913, country: "uk", region: "England", subregion: "Hampshire"),
        City(slug: "swindon", name: "Swindon", lat: 51.5580, lon: -1.7812, country: "uk", region: "England", subregion: "Wiltshire"),
        City(slug: "dudley", name: "Dudley", lat: 52.5000, lon: -2.0833, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "aberdeen-aberdeenshire", name: "Aberdeen", lat: 57.1437, lon: -2.0981, country: "uk", region: "Scotland", subregion: "Aberdeenshire"),
        City(slug: "st-helens", name: "St Helens", lat: 53.4500, lon: -2.7333, country: "uk", region: "England", subregion: "Merseyside"),
        City(slug: "ipswich", name: "Ipswich", lat: 52.0592, lon: 1.1555, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "wigan", name: "Wigan", lat: 53.5430, lon: -2.6371, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "warrington", name: "Warrington", lat: 53.3925, lon: -2.5802, country: "uk", region: "England", subregion: "Cheshire"),
        City(slug: "walsall", name: "Walsall", lat: 52.5853, lon: -1.9840, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "mansfield-nottinghamshire", name: "Mansfield", lat: 53.1333, lon: -1.2000, country: "uk", region: "England", subregion: "Nottinghamshire"),
        City(slug: "sunderland", name: "Sunderland", lat: 54.9046, lon: -1.3822, country: "uk", region: "England", subregion: "Tyne and Wear"),
        City(slug: "bournemouth", name: "Bournemouth", lat: 50.7205, lon: -1.8795, country: "uk", region: "England", subregion: "Dorset"),
        City(slug: "peterborough", name: "Peterborough", lat: 52.5736, lon: -0.2478, country: "uk", region: "England", subregion: "Cambridgeshire"),
        City(slug: "oxford-oxfordshire", name: "Oxford", lat: 51.7522, lon: -1.2560, country: "uk", region: "England", subregion: "Oxfordshire"),
        City(slug: "newport-gwent", name: "Newport", lat: 51.5877, lon: -2.9983, country: "uk", region: "Wales", subregion: "Gwent"),
        City(slug: "york-north-yorkshire", name: "York", lat: 53.9576, lon: -1.0827, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "telford", name: "Telford", lat: 52.6766, lon: -2.4493, country: "uk", region: "England", subregion: "Shropshire"),
        City(slug: "poole", name: "Poole", lat: 50.7143, lon: -1.9846, country: "uk", region: "England", subregion: "Dorset"),
        City(slug: "burnley", name: "Burnley", lat: 53.8000, lon: -2.2333, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "huddersfield", name: "Huddersfield", lat: 53.6490, lon: -1.7842, country: "uk", region: "England", subregion: "West Yorkshire"),
        City(slug: "dundee", name: "Dundee", lat: 56.4691, lon: -2.9749, country: "uk", region: "Scotland", subregion: "Tayside"),
        City(slug: "blackburn", name: "Blackburn", lat: 53.7500, lon: -2.4833, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "cambridge-cambridgeshire", name: "Cambridge", lat: 52.2000, lon: 0.1167, country: "uk", region: "England", subregion: "Cambridgeshire"),
        City(slug: "blackpool", name: "Blackpool", lat: 53.8167, lon: -3.0500, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "basildon", name: "Basildon", lat: 51.5684, lon: 0.4578, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "norwich", name: "Norwich", lat: 52.6278, lon: 1.2983, country: "uk", region: "England", subregion: "Norfolk"),
        City(slug: "middlesbrough", name: "Middlesbrough", lat: 54.5762, lon: -1.2348, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "bolton", name: "Bolton", lat: 53.5833, lon: -2.4333, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "stockport", name: "Stockport", lat: 53.4098, lon: -2.1576, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "high-wycombe", name: "High Wycombe", lat: 51.6291, lon: -0.7493, country: "uk", region: "England", subregion: "Buckinghamshire"),
        City(slug: "gloucester", name: "Gloucester", lat: 51.8657, lon: -2.2431, country: "uk", region: "England", subregion: "Gloucestershire"),
        City(slug: "exeter", name: "Exeter", lat: 50.7236, lon: -3.5275, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "colchester", name: "Colchester", lat: 51.8892, lon: 0.9042, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "salford", name: "Salford", lat: 53.4877, lon: -2.2904, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "newcastle-under-lyme", name: "Newcastle-under-Lyme", lat: 53.0000, lon: -2.2333, country: "uk", region: "England", subregion: "Staffordshire"),
        City(slug: "solihull", name: "Solihull", lat: 52.4143, lon: -1.7809, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "watford", name: "Watford", lat: 51.6553, lon: -0.3960, country: "uk", region: "England", subregion: "Hertfordshire"),
        City(slug: "crawley", name: "Crawley", lat: 51.1130, lon: -0.1831, country: "uk", region: "England", subregion: "West Sussex"),
        City(slug: "burton-upon-trent", name: "Burton upon Trent", lat: 52.8073, lon: -1.6426, country: "uk", region: "England", subregion: "Staffordshire"),
        City(slug: "cheltenham", name: "Cheltenham", lat: 51.9001, lon: -2.0797, country: "uk", region: "England", subregion: "Gloucestershire"),
        City(slug: "rotherham", name: "Rotherham", lat: 53.4301, lon: -1.3568, country: "uk", region: "England", subregion: "South Yorkshire"),
        City(slug: "worthing", name: "Worthing", lat: 50.8180, lon: -0.3754, country: "uk", region: "England", subregion: "West Sussex"),
        City(slug: "doncaster", name: "Doncaster", lat: 53.5228, lon: -1.1312, country: "uk", region: "England", subregion: "South Yorkshire"),
        City(slug: "chesterfield", name: "Chesterfield", lat: 53.2500, lon: -1.4167, country: "uk", region: "England", subregion: "Derbyshire"),
        City(slug: "chelmsford", name: "Chelmsford", lat: 51.7358, lon: 0.4696, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "sutton-coldfield", name: "Sutton Coldfield", lat: 52.5667, lon: -1.8167, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "wakefield", name: "Wakefield", lat: 53.6833, lon: -1.4977, country: "uk", region: "England", subregion: "West Yorkshire"),
        City(slug: "basingstoke", name: "Basingstoke", lat: 51.2625, lon: -1.0871, country: "uk", region: "England", subregion: "Hampshire"),
        City(slug: "maidstone", name: "Maidstone", lat: 51.2667, lon: 0.5167, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "bedford-bedfordshire", name: "Bedford", lat: 52.1346, lon: -0.4663, country: "uk", region: "England", subregion: "Bedfordshire"),
        City(slug: "woking", name: "Woking", lat: 51.3190, lon: -0.5589, country: "uk", region: "England", subregion: "Surrey"),
        City(slug: "lincoln-lincolnshire", name: "Lincoln", lat: 53.2268, lon: -0.5379, country: "uk", region: "England", subregion: "Lincolnshire"),
        City(slug: "west-bromwich", name: "West Bromwich", lat: 52.5187, lon: -1.9945, country: "uk", region: "England", subregion: "West Midlands"),
        City(slug: "eastbourne", name: "Eastbourne", lat: 50.7687, lon: 0.2845, country: "uk", region: "England", subregion: "East Sussex"),
        City(slug: "worcester-worcestershire", name: "Worcester", lat: 52.1893, lon: -2.2200, country: "uk", region: "England", subregion: "Worcestershire"),
        City(slug: "bath", name: "Bath", lat: 51.3751, lon: -2.3617, country: "uk", region: "England", subregion: "Somerset"),
        City(slug: "aberfeldy", name: "Aberfeldy", lat: 56.6220, lon: -3.8669, country: "uk", region: "Scotland", subregion: "Tayside"),
        City(slug: "aberystwyth", name: "Aberystwyth area", lat: 52.4155, lon: -4.0829, country: "uk", region: "Wales", subregion: "Dyfed"),
        City(slug: "abingdon-oxfordshire", name: "Abingdon area", lat: 51.6711, lon: -1.2828, country: "uk", region: "England", subregion: "Oxfordshire"),
        City(slug: "accrington", name: "Accrington area", lat: 53.7538, lon: -2.3586, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "altrincham", name: "Altrincham area", lat: 53.3875, lon: -2.3485, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "arbroath", name: "Arbroath area", lat: 56.5632, lon: -2.5874, country: "uk", region: "Scotland", subregion: "Tayside"),
        City(slug: "ashford", name: "Ashford", lat: 51.1465, lon: 0.8738, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "aviemore", name: "Aviemore", lat: 57.1955, lon: -3.8259, country: "uk", region: "Scotland", subregion: "Highland"),
        City(slug: "aylesbury", name: "Aylesbury", lat: 51.8167, lon: -0.8146, country: "uk", region: "England", subregion: "Buckinghamshire"),
        City(slug: "ayr", name: "Ayr", lat: 55.4627, lon: -4.6339, country: "uk", region: "Scotland"),
        City(slug: "bala", name: "Bala", lat: 52.9111, lon: -3.5972, country: "uk", region: "Wales", subregion: "Gwynedd"),
        City(slug: "ballymena", name: "Ballymena", lat: 54.8636, lon: -6.2763, country: "uk", region: "Northern Ireland"),
        City(slug: "banbridge", name: "Banbridge", lat: 54.3500, lon: -6.2833, country: "uk", region: "Northern Ireland"),
        City(slug: "banbury", name: "Banbury area", lat: 52.0632, lon: -1.3422, country: "uk", region: "England", subregion: "Oxfordshire"),
        City(slug: "banchory", name: "Banchory", lat: 57.0517, lon: -2.4882, country: "uk", region: "Scotland", subregion: "Aberdeenshire"),
        City(slug: "bangor-ards-and-north-down", name: "Bangor area", lat: 54.6608, lon: -5.6680, country: "uk", region: "Northern Ireland"),
        City(slug: "bangor-gwynedd", name: "Bangor area", lat: 53.2275, lon: -4.1294, country: "uk", region: "Wales", subregion: "Gwynedd"),
        City(slug: "barnsley", name: "Barnsley", lat: 53.5500, lon: -1.4833, country: "uk", region: "England", subregion: "South Yorkshire"),
        City(slug: "barnstaple", name: "Barnstaple area", lat: 51.0802, lon: -4.0581, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "barrow-in-furness", name: "Barrow-in-Furness area", lat: 54.1109, lon: -3.2276, country: "uk", region: "England", subregion: "Cumbria"),
        City(slug: "berwick-upon-tweed", name: "Berwick-upon-Tweed", lat: 55.7687, lon: -2.0054, country: "uk", region: "England"),
        City(slug: "beverley", name: "Beverley area", lat: 53.8459, lon: -0.4233, country: "uk", region: "England", subregion: "East Riding of Yorkshire"),
        City(slug: "bishop-s-stortford", name: "Bishop's Stortford area", lat: 51.8711, lon: 0.1587, country: "uk", region: "England", subregion: "Hertfordshire"),
        City(slug: "boston-lincolnshire", name: "Boston area", lat: 52.9763, lon: -0.0266, country: "uk", region: "England", subregion: "Lincolnshire"),
        City(slug: "bracknell", name: "Bracknell area", lat: 51.4136, lon: -0.7505, country: "uk", region: "England", subregion: "Berkshire"),
        City(slug: "braintree", name: "Braintree area", lat: 51.8782, lon: 0.5529, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "bridgend", name: "Bridgend area", lat: 51.5058, lon: -3.5772, country: "uk", region: "Wales", subregion: "Glamorgan"),
        City(slug: "bridgwater", name: "Bridgwater area", lat: 51.1284, lon: -3.0036, country: "uk", region: "England", subregion: "Somerset"),
        City(slug: "bridlington", name: "Bridlington", lat: 54.0831, lon: -0.1919, country: "uk", region: "England", subregion: "East Riding of Yorkshire"),
        City(slug: "broadstairs", name: "Broadstairs area", lat: 51.3584, lon: 1.4419, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "bude", name: "Bude", lat: 50.8244, lon: -4.5413, country: "uk", region: "England", subregion: "Cornwall"),
        City(slug: "builth-wells", name: "Builth Wells area", lat: 52.1494, lon: -3.4047, country: "uk", region: "Wales"),
        City(slug: "burgess-hill", name: "Burgess Hill area", lat: 50.9584, lon: -0.1329, country: "uk", region: "England", subregion: "West Sussex"),
        City(slug: "bury-st-edmunds", name: "Bury St Edmunds", lat: 52.2463, lon: 0.7111, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "campbeltown", name: "Campbeltown", lat: 55.4258, lon: -5.6076, country: "uk", region: "Scotland", subregion: "Argyll and Bute"),
        City(slug: "cannock", name: "Cannock area", lat: 52.6904, lon: -2.0309, country: "uk", region: "England", subregion: "Staffordshire"),
        City(slug: "canterbury", name: "Canterbury", lat: 51.2790, lon: 1.0799, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "cardigan", name: "Cardigan", lat: 52.0837, lon: -4.6623, country: "uk", region: "Wales", subregion: "Dyfed"),
        City(slug: "carlisle-cumbria", name: "Carlisle area", lat: 54.8951, lon: -2.9382, country: "uk", region: "England", subregion: "Cumbria"),
        City(slug: "castleford", name: "Castleford area", lat: 53.7259, lon: -1.3626, country: "uk", region: "England", subregion: "West Yorkshire"),
        City(slug: "chatham", name: "Chatham area", lat: 51.3789, lon: 0.5279, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "chester-cheshire", name: "Chester area", lat: 53.1905, lon: -2.8919, country: "uk", region: "England", subregion: "Cheshire"),
        City(slug: "chichester", name: "Chichester area", lat: 50.8367, lon: -0.7800, country: "uk", region: "England", subregion: "West Sussex"),
        City(slug: "christchurch", name: "Christchurch area", lat: 50.7358, lon: -1.7813, country: "uk", region: "England", subregion: "Dorset"),
        City(slug: "clacton-on-sea", name: "Clacton-on-Sea area", lat: 51.7897, lon: 1.1560, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "cleethorpes", name: "Cleethorpes area", lat: 53.5605, lon: -0.0323, country: "uk", region: "England", subregion: "Lincolnshire"),
        City(slug: "coleraine", name: "Coleraine", lat: 55.1333, lon: -6.6667, country: "uk", region: "Northern Ireland"),
        City(slug: "colwyn-bay", name: "Colwyn Bay area", lat: 53.2948, lon: -3.7267, country: "uk", region: "Wales", subregion: "Clwyd"),
        City(slug: "consett", name: "Consett area", lat: 54.8540, lon: -1.8316, country: "uk", region: "England", subregion: "County Durham"),
        City(slug: "corby", name: "Corby area", lat: 52.4964, lon: -0.6894, country: "uk", region: "England", subregion: "Northamptonshire"),
        City(slug: "crewe", name: "Crewe area", lat: 53.0979, lon: -2.4416, country: "uk", region: "England", subregion: "Cheshire"),
        City(slug: "cromarty", name: "Cromarty", lat: 57.6776, lon: -4.0416, country: "uk", region: "Scotland", subregion: "Highland"),
        City(slug: "cromer", name: "Cromer area", lat: 52.9312, lon: 1.2989, country: "uk", region: "England", subregion: "Norfolk"),
        City(slug: "cwmbran", name: "Cwmbran area", lat: 51.6545, lon: -3.0228, country: "uk", region: "Wales", subregion: "Gwent"),
        City(slug: "darlington", name: "Darlington area", lat: 54.5243, lon: -1.5504, country: "uk", region: "England", subregion: "County Durham"),
        City(slug: "dereham", name: "Dereham area", lat: 52.6833, lon: 0.9333, country: "uk", region: "England", subregion: "Norfolk"),
        City(slug: "dorchester", name: "Dorchester", lat: 50.7167, lon: -2.4333, country: "uk", region: "England", subregion: "Dorset"),
        City(slug: "douglas-isle-of-man", name: "Douglas", lat: 54.1500, lon: -4.4833, country: "uk", region: "Crown Dependencies"),
        City(slug: "dumfries", name: "Dumfries area", lat: 55.0696, lon: -3.6114, country: "uk", region: "Scotland", subregion: "Dumfries and Galloway"),
        City(slug: "dungannon", name: "Dungannon area", lat: 54.5034, lon: -6.7672, country: "uk", region: "Northern Ireland", subregion: "Mid Ulster"),
        City(slug: "durham-county-durham", name: "Durham", lat: 54.7768, lon: -1.5757, country: "uk", region: "England", subregion: "County Durham"),
        City(slug: "eastleigh", name: "Eastleigh area", lat: 50.9667, lon: -1.3500, country: "uk", region: "England", subregion: "Hampshire"),
        City(slug: "elgin-moray", name: "Elgin", lat: 57.6495, lon: -3.3184, country: "uk", region: "Scotland"),
        City(slug: "ely-cambridgeshire", name: "Ely", lat: 52.3996, lon: 0.2620, country: "uk", region: "England", subregion: "Cambridgeshire"),
        City(slug: "enniskillen", name: "Enniskillen", lat: 54.3462, lon: -7.6413, country: "uk", region: "Northern Ireland", subregion: "Fermanagh and Omagh"),
        City(slug: "epsom", name: "Epsom area", lat: 51.3305, lon: -0.2701, country: "uk", region: "England", subregion: "Surrey"),
        City(slug: "falmouth", name: "Falmouth", lat: 50.1544, lon: -5.0711, country: "uk", region: "England", subregion: "Cornwall"),
        City(slug: "fareham", name: "Fareham", lat: 50.8516, lon: -1.1793, country: "uk", region: "England", subregion: "Hampshire"),
        City(slug: "folkestone", name: "Folkestone area", lat: 51.0817, lon: 1.1673, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "fort-william", name: "Fort William", lat: 56.8165, lon: -5.1121, country: "uk", region: "Scotland", subregion: "Highland"),
        City(slug: "gateshead", name: "Gateshead area", lat: 54.9621, lon: -1.6017, country: "uk", region: "England", subregion: "Tyne and Wear"),
        City(slug: "grantham", name: "Grantham area", lat: 52.9115, lon: -0.6418, country: "uk", region: "England", subregion: "Lincolnshire"),
        City(slug: "great-yarmouth", name: "Great Yarmouth area", lat: 52.6083, lon: 1.7305, country: "uk", region: "England", subregion: "Norfolk"),
        City(slug: "guildford", name: "Guildford area", lat: 51.2354, lon: -0.5743, country: "uk", region: "England", subregion: "Surrey"),
        City(slug: "halifax", name: "Halifax area", lat: 53.7167, lon: -1.8500, country: "uk", region: "England", subregion: "West Yorkshire"),
        City(slug: "hamilton-lanarkshire", name: "Hamilton area", lat: 55.7667, lon: -4.0333, country: "uk", region: "Scotland"),
        City(slug: "harlow", name: "Harlow", lat: 51.7766, lon: 0.1116, country: "uk", region: "England", subregion: "Essex"),
        City(slug: "harrogate", name: "Harrogate", lat: 53.9908, lon: -1.5373, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "hartlepool", name: "Hartlepool area", lat: 54.6855, lon: -1.2103, country: "uk", region: "England", subregion: "County Durham"),
        City(slug: "hastings-east-sussex", name: "Hastings area", lat: 50.8557, lon: 0.5801, country: "uk", region: "England", subregion: "East Sussex"),
        City(slug: "haverfordwest", name: "Haverfordwest area", lat: 51.8017, lon: -4.9691, country: "uk", region: "Wales", subregion: "Dyfed"),
        City(slug: "haverhill", name: "Haverhill", lat: 52.0823, lon: 0.4389, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "hawick", name: "Hawick area", lat: 55.4227, lon: -2.7867, country: "uk", region: "Scotland"),
        City(slug: "helensburgh", name: "Helensburgh area", lat: 56.0061, lon: -4.7265, country: "uk", region: "Scotland", subregion: "Argyll and Bute"),
        City(slug: "hemel-hempstead", name: "Hemel Hempstead area", lat: 51.7537, lon: -0.4497, country: "uk", region: "England", subregion: "Hertfordshire"),
        City(slug: "hereford-herefordshire", name: "Hereford", lat: 52.0568, lon: -2.7148, country: "uk", region: "England", subregion: "Herefordshire"),
        City(slug: "herne-bay", name: "Herne Bay area", lat: 51.3730, lon: 1.1286, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "horsham", name: "Horsham", lat: 51.0631, lon: -0.3276, country: "uk", region: "England", subregion: "West Sussex"),
        City(slug: "huntly", name: "Huntly", lat: 57.4474, lon: -2.7861, country: "uk", region: "Scotland", subregion: "Aberdeenshire"),
        City(slug: "ilkeston", name: "Ilkeston area", lat: 52.9706, lon: -1.3095, country: "uk", region: "England", subregion: "Derbyshire"),
        City(slug: "inverness-highland", name: "Inverness", lat: 57.4791, lon: -4.2240, country: "uk", region: "Scotland", subregion: "Highland"),
        City(slug: "ivybridge", name: "Ivybridge area", lat: 50.3904, lon: -3.9191, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "kendal", name: "Kendal area", lat: 54.3268, lon: -2.7476, country: "uk", region: "England", subregion: "Cumbria"),
        City(slug: "keswick", name: "Keswick", lat: 54.5995, lon: -3.1326, country: "uk", region: "England", subregion: "Cumbria"),
        City(slug: "king-s-lynn", name: "King's Lynn", lat: 52.7517, lon: 0.3952, country: "uk", region: "England", subregion: "Norfolk"),
        City(slug: "kirkcaldy", name: "Kirkcaldy area", lat: 56.1168, lon: -3.1600, country: "uk", region: "Scotland"),
        City(slug: "kirkwall", name: "Kirkwall area", lat: 58.9848, lon: -2.9587, country: "uk", region: "Scotland"),
        City(slug: "lancaster-lancashire", name: "Lancaster area", lat: 54.0465, lon: -2.7999, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "leamington-spa", name: "Leamington Spa", lat: 52.2852, lon: -1.5200, country: "uk", region: "England", subregion: "Warwickshire"),
        City(slug: "leiston", name: "Leiston area", lat: 52.2061, lon: 1.5776, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "lerwick", name: "Lerwick", lat: 60.1534, lon: -1.1443, country: "uk", region: "Scotland"),
        City(slug: "letchworth-garden-city", name: "Letchworth Garden City area", lat: 51.9794, lon: -0.2266, country: "uk", region: "England", subregion: "Hertfordshire"),
        City(slug: "lisburn", name: "Lisburn area", lat: 54.5234, lon: -6.0353, country: "uk", region: "Northern Ireland"),
        City(slug: "livingston-lothian", name: "Livingston area", lat: 55.9029, lon: -3.5226, country: "uk", region: "Scotland", subregion: "Lothian"),
        City(slug: "llanelli", name: "Llanelli area", lat: 51.6820, lon: -4.1619, country: "uk", region: "Wales", subregion: "Dyfed"),
        City(slug: "londonderry", name: "Londonderry", lat: 54.9981, lon: -7.3093, country: "uk", region: "Northern Ireland"),
        City(slug: "loughborough", name: "Loughborough", lat: 52.7667, lon: -1.2000, country: "uk", region: "England", subregion: "Leicestershire"),
        City(slug: "lowestoft", name: "Lowestoft", lat: 52.4752, lon: 1.7517, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "ludlow", name: "Ludlow area", lat: 52.3743, lon: -2.7131, country: "uk", region: "England", subregion: "Shropshire"),
        City(slug: "lytham-st-annes", name: "Lytham St Annes", lat: 53.7426, lon: -2.9970, country: "uk", region: "England", subregion: "Lancashire"),
        City(slug: "macclesfield", name: "Macclesfield area", lat: 53.2602, lon: -2.1256, country: "uk", region: "England", subregion: "Cheshire"),
        City(slug: "maghera", name: "Maghera", lat: 54.8439, lon: -6.6715, country: "uk", region: "Northern Ireland", subregion: "Mid Ulster"),
        City(slug: "maidenhead", name: "Maidenhead area", lat: 51.5228, lon: -0.7199, country: "uk", region: "England", subregion: "Berkshire"),
        City(slug: "malton", name: "Malton", lat: 54.1369, lon: -0.7996, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "malvern", name: "Malvern", lat: 52.1116, lon: -2.3251, country: "uk", region: "England", subregion: "Worcestershire"),
        City(slug: "marlborough", name: "Marlborough", lat: 51.4203, lon: -1.7295, country: "uk", region: "England", subregion: "Wiltshire"),
        City(slug: "merthyr-tydfil", name: "Merthyr Tydfil area", lat: 51.7479, lon: -3.3778, country: "uk", region: "Wales", subregion: "Glamorgan"),
        City(slug: "newbury", name: "Newbury", lat: 51.4015, lon: -1.3247, country: "uk", region: "England", subregion: "Berkshire"),
        City(slug: "newmarket", name: "Newmarket", lat: 52.2447, lon: 0.4042, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "newport-isle-of-wight", name: "Newport area", lat: 50.7015, lon: -1.2912, country: "uk", region: "England"),
        City(slug: "newquay", name: "Newquay", lat: 50.4156, lon: -5.0732, country: "uk", region: "England", subregion: "Cornwall"),
        City(slug: "newry", name: "Newry", lat: 54.1784, lon: -6.3374, country: "uk", region: "Northern Ireland"),
        City(slug: "newton-stewart", name: "Newton Stewart", lat: 54.9578, lon: -4.4832, country: "uk", region: "Scotland", subregion: "Dumfries and Galloway"),
        City(slug: "northallerton", name: "Northallerton", lat: 54.3390, lon: -1.4324, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "nuneaton", name: "Nuneaton area", lat: 52.5232, lon: -1.4652, country: "uk", region: "England", subregion: "Warwickshire"),
        City(slug: "okehampton", name: "Okehampton", lat: 50.7384, lon: -4.0016, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "omagh", name: "Omagh", lat: 54.6000, lon: -7.3000, country: "uk", region: "Northern Ireland", subregion: "Fermanagh and Omagh"),
        City(slug: "paisley", name: "Paisley area", lat: 55.8317, lon: -4.4325, country: "uk", region: "Scotland"),
        City(slug: "penzance", name: "Penzance", lat: 50.1186, lon: -5.5371, country: "uk", region: "England", subregion: "Cornwall"),
        City(slug: "perth", name: "Perth", lat: 56.3952, lon: -3.4314, country: "uk", region: "Scotland", subregion: "Tayside"),
        City(slug: "peterhead", name: "Peterhead area", lat: 57.5052, lon: -1.7844, country: "uk", region: "Scotland", subregion: "Aberdeenshire"),
        City(slug: "portree", name: "Portree", lat: 57.4129, lon: -6.1942, country: "uk", region: "Scotland", subregion: "Highland"),
        City(slug: "pwllheli", name: "Pwllheli area", lat: 52.8899, lon: -4.4145, country: "uk", region: "Wales", subregion: "Gwynedd"),
        City(slug: "redditch", name: "Redditch area", lat: 52.3065, lon: -1.9457, country: "uk", region: "England", subregion: "Worcestershire"),
        City(slug: "redhill", name: "Redhill area", lat: 51.2405, lon: -0.1704, country: "uk", region: "England", subregion: "Surrey"),
        City(slug: "redruth", name: "Redruth area", lat: 50.2332, lon: -5.2243, country: "uk", region: "England", subregion: "Cornwall"),
        City(slug: "rochdale", name: "Rochdale area", lat: 53.6177, lon: -2.1552, country: "uk", region: "England", subregion: "Greater Manchester"),
        City(slug: "ross-on-wye", name: "Ross-on-Wye area", lat: 51.9167, lon: -2.5667, country: "uk", region: "England", subregion: "Herefordshire"),
        City(slug: "rothesay", name: "Rothesay", lat: 55.8365, lon: -5.0551, country: "uk", region: "Scotland", subregion: "Argyll and Bute"),
        City(slug: "salisbury-wiltshire", name: "Salisbury", lat: 51.0693, lon: -1.7957, country: "uk", region: "England", subregion: "Wiltshire"),
        City(slug: "scarborough", name: "Scarborough", lat: 54.2797, lon: -0.4044, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "scunthorpe", name: "Scunthorpe area", lat: 53.5791, lon: -0.6544, country: "uk", region: "England", subregion: "Lincolnshire"),
        City(slug: "shaftesbury", name: "Shaftesbury area", lat: 51.0053, lon: -2.1933, country: "uk", region: "England", subregion: "Dorset"),
        City(slug: "shrewsbury", name: "Shrewsbury", lat: 52.7101, lon: -2.7521, country: "uk", region: "England", subregion: "Shropshire"),
        City(slug: "skegness", name: "Skegness area", lat: 53.1436, lon: 0.3363, country: "uk", region: "England", subregion: "Lincolnshire"),
        City(slug: "south-shields", name: "South Shields area", lat: 54.9986, lon: -1.4323, country: "uk", region: "England", subregion: "Tyne and Wear"),
        City(slug: "southport-merseyside", name: "Southport", lat: 53.6458, lon: -3.0101, country: "uk", region: "England", subregion: "Merseyside"),
        City(slug: "st-austell", name: "St Austell area", lat: 50.3425, lon: -4.7744, country: "uk", region: "England", subregion: "Cornwall"),
        City(slug: "st-helier", name: "St Helier", lat: 49.1880, lon: -2.1049, country: "uk", region: "Crown Dependencies"),
        City(slug: "st-neots", name: "St Neots area", lat: 52.2167, lon: -0.2667, country: "uk", region: "England", subregion: "Cambridgeshire"),
        City(slug: "st-peter-port", name: "St Peter Port area", lat: 49.4598, lon: -2.5353, country: "uk", region: "Crown Dependencies"),
        City(slug: "stirling", name: "Stirling area", lat: 56.1190, lon: -3.9368, country: "uk", region: "Scotland"),
        City(slug: "stockton-on-tees", name: "Stockton-on-Tees", lat: 54.5685, lon: -1.3187, country: "uk", region: "England", subregion: "County Durham"),
        City(slug: "stornoway", name: "Stornoway", lat: 58.2092, lon: -6.3865, country: "uk", region: "Scotland"),
        City(slug: "stroud", name: "Stroud area", lat: 51.7500, lon: -2.2000, country: "uk", region: "England", subregion: "Gloucestershire"),
        City(slug: "swadlincote", name: "Swadlincote area", lat: 52.7740, lon: -1.5574, country: "uk", region: "England", subregion: "Derbyshire"),
        City(slug: "taunton", name: "Taunton area", lat: 51.0149, lon: -3.1029, country: "uk", region: "England", subregion: "Somerset"),
        City(slug: "thetford", name: "Thetford area", lat: 52.4167, lon: 0.7500, country: "uk", region: "England", subregion: "Norfolk"),
        City(slug: "thirsk", name: "Thirsk", lat: 54.2330, lon: -1.3441, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "thurso", name: "Thurso", lat: 58.5927, lon: -3.5259, country: "uk", region: "Scotland", subregion: "Highland"),
        City(slug: "tiverton", name: "Tiverton area", lat: 50.9024, lon: -3.4923, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "torquay", name: "Torquay area", lat: 50.4620, lon: -3.5252, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "totnes", name: "Totnes area", lat: 50.4311, lon: -3.6843, country: "uk", region: "England", subregion: "Devon"),
        City(slug: "trowbridge", name: "Trowbridge area", lat: 51.3189, lon: -2.2086, country: "uk", region: "England", subregion: "Wiltshire"),
        City(slug: "tunbridge-wells", name: "Tunbridge Wells area", lat: 51.1332, lon: 0.2626, country: "uk", region: "England", subregion: "Kent"),
        City(slug: "wellington-shropshire", name: "Wellington area", lat: 52.7000, lon: -2.5167, country: "uk", region: "England", subregion: "Shropshire"),
        City(slug: "weston-super-mare", name: "Weston-super-Mare", lat: 51.3460, lon: -2.9766, country: "uk", region: "England", subregion: "Somerset"),
        City(slug: "whitby", name: "Whitby", lat: 54.4877, lon: -0.6150, country: "uk", region: "England", subregion: "North Yorkshire"),
        City(slug: "whitehaven", name: "Whitehaven", lat: 54.5490, lon: -3.5841, country: "uk", region: "England", subregion: "Cumbria"),
        City(slug: "widnes", name: "Widnes area", lat: 53.3618, lon: -2.7341, country: "uk", region: "England", subregion: "Cheshire"),
        City(slug: "wisbech", name: "Wisbech", lat: 52.6662, lon: 0.1594, country: "uk", region: "England", subregion: "Cambridgeshire"),
        City(slug: "woodbridge-suffolk", name: "Woodbridge", lat: 52.0933, lon: 1.3204, country: "uk", region: "England", subregion: "Suffolk"),
        City(slug: "workington", name: "Workington", lat: 54.6425, lon: -3.5441, country: "uk", region: "England", subregion: "Cumbria"),
        City(slug: "worksop", name: "Worksop", lat: 53.3018, lon: -1.1240, country: "uk", region: "England", subregion: "Nottinghamshire"),
        City(slug: "wrexham", name: "Wrexham area", lat: 53.0466, lon: -2.9913, country: "uk", region: "Wales", subregion: "Clwyd"),
        City(slug: "yeovil", name: "Yeovil area", lat: 50.9416, lon: -2.6321, country: "uk", region: "England", subregion: "Somerset"),
        // ── end United Kingdom (generated)
    ]

    /// The collation locale for a country's city names: Polish for `pl` (so the
    /// diacritic letters sort in their alphabet positions), English elsewhere.
    static func collationLocale(for countryCode: String) -> Locale {
        Locale(identifier: countryCode == "pl" ? "pl_PL" : "en_GB")
    }

    /// Ultimate fallback city — the first bundled city overall (Poznań). Only the
    /// compile-time default for the networking stores before the gate sets a real
    /// city; live flows pick a country-scoped default from the catalog instead.
    static let `default` = all[0]

    /// A "you're nearer another city — switch?" prompt the app should
    /// surface: the city to offer (`target`) and a stable de-dupe `key`
    /// for the `chosen → nearest` pair so the prompt fires at most once
    /// per pair.
    struct CitySwitchSuggestion: Equatable {
        let target: City
        let key: String
    }

    /// The stable de-dupe key for a `chosen → nearest` pair. Built in one place
    /// so the value `switchSuggestion` compares against is the same one the
    /// first-launch gate pre-records via `initialChoiceSuppressKey`.
    static func switchPromptKey(chosenSlug: String, nearestSlug: String) -> String {
        chosenSlug + "→" + nearestSlug
    }

    /// The prompt key to pre-record when the user *deliberately* picks
    /// `chosenSlug` at the first-launch gate while location placed them nearest
    /// `nearestSlug`. Seeding it means `switchSuggestion` won't immediately turn
    /// around and offer to switch back to the city they just chose against —
    /// the choice was intentional. Returns `nil` when there's nothing to
    /// suppress: no location fix (`nearestSlug == nil`), or the chosen city *is*
    /// the nearest. Only this one pair is suppressed, so travelling to a
    /// different city later still re-arms the prompt.
    static func initialChoiceSuppressKey(chosenSlug: String, nearestSlug: String?) -> String? {
        guard let nearestSlug, nearestSlug != chosenSlug else { return nil }
        return switchPromptKey(chosenSlug: chosenSlug, nearestSlug: nearestSlug)
    }

    /// What `CityChoiceView.choose(_:)` should do about the "nearer city"
    /// prompt when adopting `chosenSlug`, given `nearestSlug` — the
    /// location-detected nearest on the true first-launch flow, or `nil` when
    /// this pick has none (Filtry's "Choose another city", or a country
    /// switch, neither of which resolved a location fix before landing here).
    enum SwitchPromptSuppression: Equatable {
        /// A real, DIFFERENT detected nearest: seed the precise
        /// `chosen→nearest` pair, so `switchSuggestion` recognises and stays
        /// quiet for exactly it.
        case seedKey(String)
        /// No detected nearest to build a pair from — there's nothing precise
        /// to key on, so skip the ONE check `ContentView.onAppear` fires right
        /// after this pick instead (`CitySwitchSuppressor`, the same
        /// mechanism a web sign-in's Custom Tab resume uses).
        case suppressNextCheck
        /// Nothing to do: the chosen city already equals the detected
        /// nearest, so `switchSuggestion` will stay quiet on its own.
        case none
    }

    static func switchPromptSuppression(chosenSlug: String, nearestSlug: String?) -> SwitchPromptSuppression {
        if let key = initialChoiceSuppressKey(chosenSlug: chosenSlug, nearestSlug: nearestSlug) {
            return .seedKey(key)
        }
        return nearestSlug == nil ? .suppressNextCheck : .none
    }

    /// The city-prefixed API URL for `endpoint` (`"repertoire"` /
    /// `"details"`) against `base` — e.g. `…/poznan/api/repertoire`. The
    /// repertoire/details stores build their fetch URL through this so the
    /// `/{slug}/api/…` shape lives in one tested place. Auth endpoints stay
    /// unprefixed and never call this.
    static func apiURL(base: URL, slug: String, endpoint: String) -> URL {
        base
            .appendingPathComponent(slug)
            .appendingPathComponent("api")
            .appendingPathComponent(endpoint)
    }

    /// Fold a Polish string to its diacritic-free, lower-case form for search
    /// matching, so a query typed without Polish letters still finds the city
    /// ("lodz" → "Łódź", "krakow" → "Kraków"). `.diacriticInsensitive` folding
    /// doesn't map ł/ą/ę/ń and isn't identical on Linux swift-corelibs, so map
    /// the Polish letters explicitly for cross-platform-stable results.
    static func searchFold(_ s: String) -> String {
        let map: [Character: Character] = [
            "ą": "a", "ć": "c", "ę": "e", "ł": "l", "ń": "n",
            "ó": "o", "ś": "s", "ź": "z", "ż": "z"
        ]
        return String(s.lowercased().map { map[$0] ?? $0 })
    }

    /// Whether this city matches `query` under case- and diacritic-insensitive
    /// substring matching. A blank query matches every city (an empty search
    /// box shows the whole list).
    func matches(_ query: String) -> Bool {
        let q = City.searchFold(query.trimmingCharacters(in: .whitespaces))
        return q.isEmpty || matches(folded: q)
    }

    /// Whether `name` or any of `aliases` contains the already-folded query.
    /// Every search over cities goes through here, so an alias is found
    /// wherever the name would be.
    func matches(folded q: String) -> Bool {
        ([name] + (aliases ?? [])).contains { City.searchFold($0).contains(q) }
    }

    /// Great-circle distance in kilometres between two coordinates.
    static func haversineKm(latitude1: Double, longitude1: Double, latitude2: Double, longitude2: Double) -> Double {
        let earthRadiusKm = 6_371.0
        let deltaLatitude = (latitude2 - latitude1) * .pi / 180
        let deltaLongitude = (longitude2 - longitude1) * .pi / 180
        let latitude1Radians = latitude1 * .pi / 180
        let latitude2Radians = latitude2 * .pi / 180
        let squareHalfChord = sin(deltaLatitude / 2) * sin(deltaLatitude / 2)
            + sin(deltaLongitude / 2) * sin(deltaLongitude / 2) * cos(latitude1Radians) * cos(latitude2Radians)
        let angularDistance = 2 * atan2(sqrt(squareHalfChord), sqrt(1 - squareHalfChord))
        return earthRadiusKm * angularDistance
    }
}

extension City {
    /// The RAW localization key(s) for the group level(s) a search box's
    /// prompt should name from a country that groups its cities — the UK's
    /// nation/county, the US's/Germany's single state level. Not
    /// `LocalizedStringKey` itself: this file is pure Foundation (see the
    /// struct's own doc comment) so `swift test` still runs it on Linux CI;
    /// `CityGate.searchPrompt` is what turns a key into `Text`. Mirrors
    /// `PICKER_GROUP_LABELS` in landing.scala.html.
    static let groupSearchLevelKeys: [String: (region: String, subregion: String?)] = [
        "uk": ("citygate.level.uk.region", "citygate.level.uk.subregion"),
        "us": ("citygate.level.us.region", nil),
        "de": ("citygate.level.de.region", nil),
    ]

    /// The localization key(s) a search from THIS level can still reach,
    /// deepest first — mirrors `pickerSearchLevels()` in landing.scala.html
    /// exactly, so web and the app phrase the search box's prompt
    /// identically: two at the root of a country that nests two deep (the
    /// UK), one once a region is picked or for a country with only one level
    /// (US/Germany), none once fully drilled into a subregion or for a flat
    /// country (Poland, Spain).
    static func searchLevelKeys(country: String, region: String?, subregion: String?) -> [String] {
        guard let levels = groupSearchLevelKeys[country] else { return [] }
        if region == nil {
            if let sub = levels.subregion { return [levels.region, sub] }
            return [levels.region]
        }
        if subregion == nil, let sub = levels.subregion { return [sub] }
        return []
    }

    /// One row of a grouped picker level: a group heading (drills further) or
    /// a city that carries no group at this level, either because its top
    /// group collapsed onto it alone (`CityGroup.soleCity` — Berlin, Hamburg,
    /// Delaware, Vermont) or because the country/region simply isn't split
    /// any further here.
    enum PickerRow: Hashable {
        case heading(String)
        case city(City)
    }

    /// One row of a FLATTENED, whole-country search result — see
    /// `Array<City>.searchRows(matching:inCountry:)`. Unlike `PickerRow`, a
    /// subregion here carries its parent `region`: picking it from a flat
    /// list has to land two drill-steps deep at once, not just one, so the
    /// caller can't recover the region from its own current state the way
    /// `PickerRow`'s single-level callers can.
    enum SearchRow: Hashable {
        case region(String)
        case subregion(String, region: String)
        case city(City)

        var label: String {
            switch self {
            case .region(let name): return name
            case .subregion(let name, _): return name
            case .city(let city): return city.name
            }
        }
    }

    /// One pass over `cities` — ALREADY in the catalog's own canonical picker
    /// order (`Catalog.scala` emits each country's cities in its picker
    /// tree's own traversal order; never re-sorted here) — interleaving each
    /// group's heading, emitted at its FIRST occurrence, with every city that
    /// carries no group at this level. Listing every heading before every
    /// direct city (the old shape) stranded a heading like "West Midlands"
    /// ahead of every direct city in its region, instead of beside "West
    /// Sussex" where it alphabetically belongs.
    fileprivate static func mergedRows(_ cities: [City], groupField: (City) -> String?, query: String) -> [PickerRow] {
        let q = City.searchFold(query.trimmingCharacters(in: .whitespaces))
        var seen = Set<String>()
        var rows: [PickerRow] = []
        for city in cities {
            if let group = groupField(city) {
                guard seen.insert(group).inserted else { continue }
                if q.isEmpty || City.searchFold(group).contains(q) { rows.append(.heading(group)) }
            } else if q.isEmpty || city.matches(folded: q) {
                rows.append(.city(city))
            }
        }
        return rows
    }
}

/// Per-country queries over a catalog's city list. `self` is the live catalog's
/// cities — the server-fetched list, the bundled seed, or (as a fallback) the
/// compile-time `City.all`. Kept pure (no I/O) so they're unit-tested in
/// `KinowoCore` by passing a fixed list; the app's `CatalogStore` calls them
/// with its current `cities`.
extension Array where Element == City {

    /// The subset belonging to `countryCode` (`"pl"`, `"uk"`), in this list's order.
    func inCountry(_ countryCode: String) -> [City] { filter { $0.country == countryCode } }

    /// The country code of the city with `slug`, or `nil` when no such city.
    /// Lets a deep link that lands on another country's city (a
    /// `showtimes-uk` / `showtimes-de` link) switch the app to the right
    /// deployment before the repertoire loads.
    func country(ofSlug slug: String) -> String? { first { $0.slug == slug }?.country }

    /// Each former slug (`City.formerSlugs`) → the city that answers at it now.
    var currentSlugByFormer: [String: String] {
        Dictionary(flatMap { city in (city.formerSlugs ?? []).map { ($0, city.slug) } },
                   uniquingKeysWith: { first, _ in first })
    }

    /// The zone to reason about `slug`'s showtimes in — that city's own where the
    /// catalog gave it one, else `fallback` (its country's). Also the answer for a
    /// slug this catalog does not know, which is what a stale saved selection or a
    /// deep link into another deployment looks like.
    func zone(ofSlug slug: String?, fallback: TimeZone) -> TimeZone {
        guard let slug else { return fallback }
        return first { $0.slug == slug }?.zone(fallback: fallback) ?? fallback
    }

    /// [inCountry] ordered alphabetically under that country's collation (Polish
    /// for `pl`, so `Ł` sorts after `L`; English elsewhere) — what the pickers show.
    func sortedForPicker(inCountry countryCode: String) -> [City] {
        let locale = City.collationLocale(for: countryCode)
        return inCountry(countryCode).sorted {
            $0.name.compare($1.name, options: .caseInsensitive, range: nil, locale: locale) == .orderedAscending
        }
    }

    /// [sortedForPicker] narrowed to the cities matching `query` (case- and
    /// diacritic-insensitive substring). A blank query yields the whole country list.
    func matching(_ query: String, inCountry countryCode: String) -> [City] {
        sortedForPicker(inCountry: countryCode).filter { $0.matches(query) }
    }

    /// The regions this country's cities are grouped under — US states, German
    /// Bundesländer, UK nations — in the catalog's own order, which is the order
    /// the web picker lists them in, so the two read alike. Empty for a country
    /// that does not group its cities (Poland, Spain), and that emptiness is what
    /// the picker reads as "show one flat list".
    ///
    /// The TOP level, even where a region nests a `subregion` below it (see
    /// `secondLevelRows(matching:inCountry:region:)`): the UK's nation names every one of its
    /// cities' `region`, whether or not the county underneath went on to earn a
    /// third step of its own.
    func regions(inCountry countryCode: String) -> [String] {
        var seen = Set<String>()
        return inCountry(countryCode).compactMap(\.region).filter { seen.insert($0).inserted }
    }

    /// `matching(_:inCountry:)` confined to one `region` — used by
    /// `matching(_:inCountry:region:subregion:)` below. A `nil` region leaves
    /// the country-wide list alone, so the same call serves both a grouped
    /// country's third screen and an ungrouped country's only one.
    func matching(_ query: String, inCountry countryCode: String, region: String?) -> [City] {
        let cities = matching(query, inCountry: countryCode)
        guard let region else { return cities }
        return cities.filter { $0.region == region }
    }

    /// The FIRST picker step for a grouped country: each region's heading,
    /// interleaved with any city whose top group collapsed onto it alone
    /// (Berlin, Hamburg; Delaware, Vermont) — narrowed to `query`. Empty for a
    /// country that does not group its cities (Poland, Spain).
    func topLevelRows(matching query: String, inCountry countryCode: String) -> [City.PickerRow] {
        City.mergedRows(inCountry(countryCode), groupField: { $0.region }, query: query)
    }

    /// The SECOND picker step, within one `region`: each subregion's heading
    /// (reached only for the UK's Glamorgan / Antrim today)
    /// interleaved with that region's own directly-listed cities — narrowed
    /// to `query`.
    func secondLevelRows(matching query: String, inCountry countryCode: String, region: String) -> [City.PickerRow] {
        City.mergedRows(inCountry(countryCode).filter { $0.region == region }, groupField: { $0.subregion }, query: query)
    }

    /// Every region, subregion and city in `countryCode` matching `query`,
    /// merged into ONE alphabetical list — a query for "kent" must surface the
    /// Kent COUNTY heading even from the UK's top level, not only once
    /// England is already open. Empty for a blank query: the per-level
    /// `topLevelRows`/`secondLevelRows` above handle that case, and a country
    /// with no grouping (Poland, Spain) has nothing to flatten in the first
    /// place — its own `matching(_:inCountry:)` already reaches every city.
    func searchRows(matching query: String, inCountry countryCode: String) -> [City.SearchRow] {
        let q = City.searchFold(query.trimmingCharacters(in: .whitespaces))
        guard !q.isEmpty else { return [] }
        var seenRegion = Set<String>()
        var seenSubregion = Set<String>()
        var rows: [City.SearchRow] = []
        for city in inCountry(countryCode) {
            if let region = city.region, seenRegion.insert(region).inserted, City.searchFold(region).contains(q) {
                rows.append(.region(region))
            }
            if let region = city.region, let subregion = city.subregion, seenSubregion.insert("\(region)\u{0}\(subregion)").inserted,
               City.searchFold(subregion).contains(q) {
                rows.append(.subregion(subregion, region: region))
            }
            if city.matches(folded: q) {
                rows.append(.city(city))
            }
        }
        let locale = City.collationLocale(for: countryCode)
        return rows.sorted { $0.label.compare($1.label, options: .caseInsensitive, range: nil, locale: locale) == .orderedAscending }
    }

    /// `matching(_:inCountry:region:)` narrowed to one `subregion` — the THIRD
    /// step, reached only where `secondLevelRows(matching:inCountry:region:)`
    /// holds a heading for it.
    func matching(_ query: String, inCountry countryCode: String, region: String, subregion: String) -> [City] {
        matching(query, inCountry: countryCode, region: region).filter { $0.subregion == subregion }
    }

    /// The default city for `countryCode` — its first entry in this list, or `nil`.
    func defaultCity(inCountry countryCode: String) -> City? { inCountry(countryCode).first }

    /// The city IN `countryCode` nearest the coordinate, or `nil` beyond 100 km.
    /// Scoped so a Polish fix never resolves to a UK region, or vice versa.
    func nearestWithin100km(lat: Double, lon: Double, inCountry countryCode: String) -> City? {
        inCountry(countryCode).nearestWithin100km(lat: lat, lon: lon)
    }

    /// The city nearest the coordinate across EVERY country this list carries,
    /// or `nil` beyond 100 km. Unlike the country-scoped overload above, this
    /// is deliberately unscoped — the manual picker's "use my location" button
    /// uses it to find the right city even when the country tab currently open
    /// isn't the one the device is actually in (a UK visitor who hasn't
    /// switched off the default Poland tab yet).
    func nearestWithin100km(lat: Double, lon: Double) -> City? {
        let ranked = map { (city: $0, km: City.haversineKm(latitude1: lat, longitude1: lon, latitude2: $0.lat, longitude2: $0.lon)) }
            .min { $0.km < $1.km }
        guard let nearest = ranked, nearest.km <= 100 else { return nil }
        return nearest.city
    }

    /// The "you're nearer another city — switch?" suggestion for a device at the
    /// coordinate, scoped to `countryCode`; `nil` when already nearest, out of
    /// range, or this pair was the `lastPromptKey`.
    func switchSuggestion(chosenSlug: String, lat: Double, lon: Double, lastPromptKey: String?, inCountry countryCode: String) -> City.CitySwitchSuggestion? {
        guard let nearest = nearestWithin100km(lat: lat, lon: lon, inCountry: countryCode),
              nearest.slug != chosenSlug else { return nil }
        let key = City.switchPromptKey(chosenSlug: chosenSlug, nearestSlug: nearest.slug)
        guard key != lastPromptKey else { return nil }
        return City.CitySwitchSuggestion(target: nearest, key: key)
    }
}
