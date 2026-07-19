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

    /// Every city the app knows about across ALL countries, in the same order
    /// the web `City.all` lists them (Polish cities first, then the UK regions)
    /// so the pickers read identically across platforms. This is the GLOBAL
    /// union; the pickers and the nearest-city gate scope it to the selected
    /// country via `cities(in:)` / `matching(_:in:)` / `nearestWithin100km(...in:)`.
    /// Adding a row here (with its `country`) is the only model-side change a new
    /// city needs.
    static let all: [City] = [
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252, country: "pl"),
        City(slug: "wroclaw", name: "Wrocław", lat: 51.1079, lon: 17.0385, country: "pl"),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122, country: "pl"),
        City(slug: "krakow", name: "Kraków", lat: 50.0647, lon: 19.9450, country: "pl"),
        City(slug: "lodz", name: "Łódź", lat: 51.7592, lon: 19.4560, country: "pl"),
        City(slug: "katowice", name: "Katowice", lat: 50.2649, lon: 19.0238, country: "pl"),
        City(slug: "szczecin", name: "Szczecin", lat: 53.4285, lon: 14.5528, country: "pl"),
        City(slug: "bialystok", name: "Białystok", lat: 53.1325, lon: 23.1688, country: "pl"),
        City(slug: "trojmiasto", name: "Trójmiasto", lat: 54.4416, lon: 18.5601, country: "pl"),
        City(slug: "bydgoszcz", name: "Bydgoszcz", lat: 53.1235, lon: 18.0084, country: "pl"),
        City(slug: "lublin", name: "Lublin", lat: 51.2465, lon: 22.5684, country: "pl"),
        City(slug: "czestochowa", name: "Częstochowa", lat: 50.8118, lon: 19.1203, country: "pl"),
        City(slug: "radom", name: "Radom", lat: 51.4027, lon: 21.1471, country: "pl"),
        City(slug: "sosnowiec", name: "Sosnowiec", lat: 50.2863, lon: 19.1041, country: "pl"),
        City(slug: "torun", name: "Toruń", lat: 53.0138, lon: 18.5984, country: "pl"),
        City(slug: "kielce", name: "Kielce", lat: 50.8661, lon: 20.6286, country: "pl"),
        City(slug: "rzeszow", name: "Rzeszów", lat: 50.0413, lon: 21.9990, country: "pl"),
        City(slug: "gliwice", name: "Gliwice", lat: 50.2945, lon: 18.6714, country: "pl"),
        City(slug: "zabrze", name: "Zabrze", lat: 50.3249, lon: 18.7857, country: "pl"),
        City(slug: "olsztyn", name: "Olsztyn", lat: 53.7784, lon: 20.4801, country: "pl"),
        City(slug: "bielsko-biala", name: "Bielsko-Biała", lat: 49.8224, lon: 19.0584, country: "pl"),
        City(slug: "opole", name: "Opole", lat: 50.6751, lon: 17.9213, country: "pl"),
        City(slug: "rybnik", name: "Rybnik", lat: 50.0971, lon: 18.5416, country: "pl"),
        City(slug: "gorzow-wielkopolski", name: "Gorzów Wielkopolski", lat: 52.7368, lon: 15.2288, country: "pl"),
        City(slug: "elblag", name: "Elbląg", lat: 54.1522, lon: 19.4088, country: "pl"),
        City(slug: "koszalin", name: "Koszalin", lat: 54.1943, lon: 16.1722, country: "pl"),
        City(slug: "kalisz", name: "Kalisz", lat: 51.7611, lon: 18.0911, country: "pl"),
        City(slug: "zielona-gora", name: "Zielona Góra", lat: 51.9356, lon: 15.5062, country: "pl"),
        City(slug: "tychy", name: "Tychy", lat: 50.1357, lon: 18.9985, country: "pl"),
        City(slug: "walbrzych", name: "Wałbrzych", lat: 50.7714, lon: 16.2845, country: "pl"),
        City(slug: "tarnow", name: "Tarnów", lat: 50.0121, lon: 20.9858, country: "pl"),
        City(slug: "wloclawek", name: "Włocławek", lat: 52.6483, lon: 19.0677, country: "pl"),
        City(slug: "legnica", name: "Legnica", lat: 51.2070, lon: 16.1619, country: "pl"),
        City(slug: "plock", name: "Płock", lat: 52.5468, lon: 19.7064, country: "pl"),
        City(slug: "bytom", name: "Bytom", lat: 50.3483, lon: 18.9157, country: "pl"),
        City(slug: "dabrowa-gornicza", name: "Dąbrowa Górnicza", lat: 50.3219, lon: 19.1876, country: "pl"),
        City(slug: "nowy-sacz", name: "Nowy Sącz", lat: 49.6175, lon: 20.7154, country: "pl"),
        City(slug: "slupsk", name: "Słupsk", lat: 54.4641, lon: 17.0287, country: "pl"),
        City(slug: "jelenia-gora", name: "Jelenia Góra", lat: 50.9044, lon: 15.7197, country: "pl"),
        City(slug: "przemysl", name: "Przemyśl", lat: 49.7838, lon: 22.7677, country: "pl"),
        City(slug: "konin", name: "Konin", lat: 52.2230, lon: 18.2511, country: "pl"),
        // ── United Kingdom (79 Flicks regions; English labels). ──────────────
        City(slug: "london", name: "London", lat: 51.5074, lon: -0.1278, country: "uk"),
        City(slug: "manchester", name: "Manchester", lat: 53.4808, lon: -2.2426, country: "uk"),
        City(slug: "norwich", name: "Norwich", lat: 52.6309, lon: 1.2974, country: "uk"),
        City(slug: "aberdeenshire", name: "Aberdeenshire", lat: 57.308, lon: -2.3393, country: "uk"),
        City(slug: "antrim", name: "Antrim", lat: 54.762, lon: -6.0127, country: "uk"),
        City(slug: "armagh", name: "Armagh", lat: 54.4492, lon: -6.398, country: "uk"),
        City(slug: "ayrshire-and-arran", name: "Ayrshire and Arran", lat: 55.5093, lon: -4.581, country: "uk"),
        City(slug: "bedfordshire", name: "Bedfordshire", lat: 52.0082, lon: -0.4435, country: "uk"),
        City(slug: "belfast", name: "Belfast", lat: 54.5857, lon: -5.9428, country: "uk"),
        City(slug: "berkshire", name: "Berkshire", lat: 51.4268, lon: -0.9169, country: "uk"),
        City(slug: "birmingham", name: "Birmingham", lat: 52.4581, lon: -1.9041, country: "uk"),
        City(slug: "bristol", name: "Bristol", lat: 51.4659, lon: -2.5805, country: "uk"),
        City(slug: "buckinghamshire", name: "Buckinghamshire", lat: 51.7582, lon: -0.7609, country: "uk"),
        City(slug: "cambridgeshire", name: "Cambridgeshire", lat: 52.4301, lon: -0.0137, country: "uk"),
        City(slug: "cardiff", name: "Cardiff", lat: 51.4892, lon: -3.1939, country: "uk"),
        City(slug: "central-scotland", name: "Central Scotland", lat: 56.08, lon: -3.8066, country: "uk"),
        City(slug: "cheshire", name: "Cheshire", lat: 53.2917, lon: -2.4966, country: "uk"),
        City(slug: "clwyd", name: "Clwyd", lat: 53.3083, lon: -3.6072, country: "uk"),
        City(slug: "cornwall", name: "Cornwall", lat: 50.317, lon: -4.9211, country: "uk"),
        City(slug: "county-durham", name: "County Durham", lat: 54.7289, lon: -1.5139, country: "uk"),
        City(slug: "cumbria", name: "Cumbria", lat: 54.4593, lon: -3.1119, country: "uk"),
        City(slug: "derbyshire", name: "Derbyshire", lat: 52.9886, lon: -1.5219, country: "uk"),
        City(slug: "devon", name: "Devon", lat: 50.6651, lon: -3.687, country: "uk"),
        City(slug: "dorset", name: "Dorset", lat: 50.7664, lon: -2.1122, country: "uk"),
        City(slug: "down", name: "Down", lat: 54.4293, lon: -5.9704, country: "uk"),
        City(slug: "dudley", name: "Dudley", lat: 52.497, lon: -2.0918, country: "uk"),
        City(slug: "dumfries-and-galloway", name: "Dumfries and Galloway", lat: 54.9881, lon: -3.8232, country: "uk"),
        City(slug: "dunbartonshire-argyll-bute", name: "Dunbartonshire and Argyll & Bute", lat: 55.7795, lon: -4.9973, country: "uk"),
        City(slug: "dyfed", name: "Dyfed", lat: 51.9892, lon: -4.3329, country: "uk"),
        City(slug: "east-sussex", name: "East Sussex", lat: 50.8499, lon: 0.2215, country: "uk"),
        City(slug: "east-yorkshire", name: "East Yorkshire", lat: 53.8685, lon: -0.3985, country: "uk"),
        City(slug: "edinburgh-and-lothians", name: "Edinburgh & Lothians", lat: 55.9404, lon: -3.2039, country: "uk"),
        City(slug: "essex", name: "Essex", lat: 51.7621, lon: 0.5901, country: "uk"),
        City(slug: "fermanagh", name: "Fermanagh", lat: 54.3499, lon: -7.6316, country: "uk"),
        City(slug: "fife", name: "Fife", lat: 56.1287, lon: -3.2424, country: "uk"),
        City(slug: "glamorgan", name: "Glamorgan", lat: 51.6388, lon: -3.7535, country: "uk"),
        City(slug: "glasgow", name: "Glasgow", lat: 55.8682, lon: -4.2316, country: "uk"),
        City(slug: "gloucestershire", name: "Gloucestershire", lat: 51.8387, lon: -2.2712, country: "uk"),
        City(slug: "guernsey", name: "Guernsey", lat: 49.4446, lon: -2.5695, country: "uk"),
        City(slug: "gwent", name: "Gwent", lat: 51.6882, lon: -3.0066, country: "uk"),
        City(slug: "gwynedd", name: "Gwynedd", lat: 53.0098, lon: -4.153, country: "uk"),
        City(slug: "hampshire", name: "Hampshire", lat: 50.9234, lon: -1.165, country: "uk"),
        City(slug: "herefordshire", name: "Herefordshire", lat: 52.031, lon: -2.7825, country: "uk"),
        City(slug: "hertfordshire", name: "Hertfordshire", lat: 51.7791, lon: -0.3102, country: "uk"),
        City(slug: "highlands-and-islands", name: "Highlands and Islands", lat: 58.086, lon: -4.0855, country: "uk"),
        City(slug: "isle-of-man", name: "Isle of Man", lat: 54.1578, lon: -4.4775, country: "uk"),
        City(slug: "isle-of-wight", name: "Isle of Wight", lat: 50.7118, lon: -1.2248, country: "uk"),
        City(slug: "jersey", name: "Jersey", lat: 49.1839, lon: -2.1144, country: "uk"),
        City(slug: "kent", name: "Kent", lat: 51.2682, lon: 0.8631, country: "uk"),
        City(slug: "lanarkshire", name: "Lanarkshire", lat: 55.7953, lon: -4.0904, country: "uk"),
        City(slug: "lancashire", name: "Lancashire", lat: 53.7367, lon: -2.6625, country: "uk"),
        City(slug: "leicestershire", name: "Leicestershire", lat: 52.6656, lon: -1.1514, country: "uk"),
        City(slug: "lincolnshire", name: "Lincolnshire", lat: 53.2194, lon: -0.2916, country: "uk"),
        City(slug: "londonderry", name: "Londonderry", lat: 54.9949, lon: -7.0636, country: "uk"),
        City(slug: "liverpool", name: "Liverpool", lat: 53.4084, lon: -2.9916, country: "uk"),
        City(slug: "north-yorkshire", name: "North Yorkshire", lat: 54.2402, lon: -1.156, country: "uk"),
        City(slug: "northamptonshire", name: "Northamptonshire", lat: 52.288, lon: -0.8653, country: "uk"),
        City(slug: "northumberland", name: "Northumberland", lat: 55.2158, lon: -1.7422, country: "uk"),
        City(slug: "nottinghamshire", name: "Nottinghamshire", lat: 53.0236, lon: -1.15, country: "uk"),
        City(slug: "oxfordshire", name: "Oxfordshire", lat: 51.7572, lon: -1.2545, country: "uk"),
        City(slug: "powys", name: "Powys", lat: 52.3806, lon: -3.26, country: "uk"),
        City(slug: "renfrewshire", name: "Renfrewshire", lat: 55.9204, lon: -4.5838, country: "uk"),
        City(slug: "roxburgh-ettrick-and-lauderdale", name: "Roxburgh, Ettrick and Lauderdale", lat: 55.5183, lon: -2.7969, country: "uk"),
        City(slug: "sandwell", name: "Sandwell", lat: 52.5175, lon: -1.9932, country: "uk"),
        City(slug: "shropshire", name: "Shropshire", lat: 52.6813, lon: -2.6215, country: "uk"),
        City(slug: "somerset", name: "Somerset", lat: 51.2159, lon: -2.824, country: "uk"),
        City(slug: "south-yorkshire", name: "South Yorkshire", lat: 53.5141, lon: -1.3109, country: "uk"),
        City(slug: "staffordshire", name: "Staffordshire", lat: 52.7942, lon: -1.9887, country: "uk"),
        City(slug: "suffolk", name: "Suffolk", lat: 52.1492, lon: 1.0262, country: "uk"),
        City(slug: "surrey", name: "Surrey", lat: 51.2269, lon: -0.5354, country: "uk"),
        City(slug: "tayside", name: "Tayside", lat: 56.5061, lon: -3.0128, country: "uk"),
        City(slug: "tyne-and-wear", name: "Tyne and Wear", lat: 54.9749, lon: -1.5397, country: "uk"),
        City(slug: "tyrone", name: "Tyrone", lat: 54.5255, lon: -6.8664, country: "uk"),
        City(slug: "warwickshire", name: "Warwickshire", lat: 52.3602, lon: -1.5034, country: "uk"),
        City(slug: "west-sussex", name: "West Sussex", lat: 50.9492, lon: -0.3262, country: "uk"),
        City(slug: "west-yorkshire", name: "West Yorkshire", lat: 53.7878, lon: -1.665, country: "uk"),
        City(slug: "wiltshire", name: "Wiltshire", lat: 51.2955, lon: -1.8505, country: "uk"),
        City(slug: "worcestershire", name: "Worcestershire", lat: 52.1923, lon: -2.2079, country: "uk"),
        City(slug: "yorkshire", name: "Yorkshire", lat: 53.4082, lon: -1.4756, country: "uk"),
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
        return q.isEmpty || City.searchFold(name).contains(q)
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

    /// The default city for `countryCode` — its first entry in this list, or `nil`.
    func defaultCity(inCountry countryCode: String) -> City? { inCountry(countryCode).first }

    /// The city IN `countryCode` nearest the coordinate, or `nil` beyond 100 km.
    /// Scoped so a Polish fix never resolves to a UK region, or vice versa.
    func nearestWithin100km(lat: Double, lon: Double, inCountry countryCode: String) -> City? {
        let ranked = inCountry(countryCode)
            .map { (city: $0, km: City.haversineKm(latitude1: lat, longitude1: lon, latitude2: $0.lat, longitude2: $0.lon)) }
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
