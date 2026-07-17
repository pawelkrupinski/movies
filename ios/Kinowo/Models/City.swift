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
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252, country: "PL"),
        City(slug: "wroclaw", name: "Wrocław", lat: 51.1079, lon: 17.0385, country: "PL"),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122, country: "PL"),
        City(slug: "krakow", name: "Kraków", lat: 50.0647, lon: 19.9450, country: "PL"),
        City(slug: "lodz", name: "Łódź", lat: 51.7592, lon: 19.4560, country: "PL"),
        City(slug: "katowice", name: "Katowice", lat: 50.2649, lon: 19.0238, country: "PL"),
        City(slug: "szczecin", name: "Szczecin", lat: 53.4285, lon: 14.5528, country: "PL"),
        City(slug: "bialystok", name: "Białystok", lat: 53.1325, lon: 23.1688, country: "PL"),
        City(slug: "trojmiasto", name: "Trójmiasto", lat: 54.4416, lon: 18.5601, country: "PL"),
        City(slug: "bydgoszcz", name: "Bydgoszcz", lat: 53.1235, lon: 18.0084, country: "PL"),
        City(slug: "lublin", name: "Lublin", lat: 51.2465, lon: 22.5684, country: "PL"),
        City(slug: "czestochowa", name: "Częstochowa", lat: 50.8118, lon: 19.1203, country: "PL"),
        City(slug: "radom", name: "Radom", lat: 51.4027, lon: 21.1471, country: "PL"),
        City(slug: "sosnowiec", name: "Sosnowiec", lat: 50.2863, lon: 19.1041, country: "PL"),
        City(slug: "torun", name: "Toruń", lat: 53.0138, lon: 18.5984, country: "PL"),
        City(slug: "kielce", name: "Kielce", lat: 50.8661, lon: 20.6286, country: "PL"),
        City(slug: "rzeszow", name: "Rzeszów", lat: 50.0413, lon: 21.9990, country: "PL"),
        City(slug: "gliwice", name: "Gliwice", lat: 50.2945, lon: 18.6714, country: "PL"),
        City(slug: "zabrze", name: "Zabrze", lat: 50.3249, lon: 18.7857, country: "PL"),
        City(slug: "olsztyn", name: "Olsztyn", lat: 53.7784, lon: 20.4801, country: "PL"),
        City(slug: "bielsko-biala", name: "Bielsko-Biała", lat: 49.8224, lon: 19.0584, country: "PL"),
        City(slug: "opole", name: "Opole", lat: 50.6751, lon: 17.9213, country: "PL"),
        City(slug: "rybnik", name: "Rybnik", lat: 50.0971, lon: 18.5416, country: "PL"),
        City(slug: "gorzow-wielkopolski", name: "Gorzów Wielkopolski", lat: 52.7368, lon: 15.2288, country: "PL"),
        City(slug: "elblag", name: "Elbląg", lat: 54.1522, lon: 19.4088, country: "PL"),
        City(slug: "koszalin", name: "Koszalin", lat: 54.1943, lon: 16.1722, country: "PL"),
        City(slug: "kalisz", name: "Kalisz", lat: 51.7611, lon: 18.0911, country: "PL"),
        City(slug: "zielona-gora", name: "Zielona Góra", lat: 51.9356, lon: 15.5062, country: "PL"),
        City(slug: "tychy", name: "Tychy", lat: 50.1357, lon: 18.9985, country: "PL"),
        City(slug: "walbrzych", name: "Wałbrzych", lat: 50.7714, lon: 16.2845, country: "PL"),
        City(slug: "tarnow", name: "Tarnów", lat: 50.0121, lon: 20.9858, country: "PL"),
        City(slug: "wloclawek", name: "Włocławek", lat: 52.6483, lon: 19.0677, country: "PL"),
        City(slug: "legnica", name: "Legnica", lat: 51.2070, lon: 16.1619, country: "PL"),
        City(slug: "plock", name: "Płock", lat: 52.5468, lon: 19.7064, country: "PL"),
        City(slug: "bytom", name: "Bytom", lat: 50.3483, lon: 18.9157, country: "PL"),
        City(slug: "dabrowa-gornicza", name: "Dąbrowa Górnicza", lat: 50.3219, lon: 19.1876, country: "PL"),
        City(slug: "nowy-sacz", name: "Nowy Sącz", lat: 49.6175, lon: 20.7154, country: "PL"),
        City(slug: "slupsk", name: "Słupsk", lat: 54.4641, lon: 17.0287, country: "PL"),
        City(slug: "jelenia-gora", name: "Jelenia Góra", lat: 50.9044, lon: 15.7197, country: "PL"),
        City(slug: "przemysl", name: "Przemyśl", lat: 49.7838, lon: 22.7677, country: "PL"),
        City(slug: "konin", name: "Konin", lat: 52.2230, lon: 18.2511, country: "PL"),
        // ── United Kingdom (79 Flicks regions; English labels). ──────────────
        City(slug: "london", name: "London", lat: 51.5074, lon: -0.1278, country: "GB"),
        City(slug: "manchester", name: "Manchester", lat: 53.4808, lon: -2.2426, country: "GB"),
        City(slug: "norwich", name: "Norwich", lat: 52.6309, lon: 1.2974, country: "GB"),
        City(slug: "aberdeenshire", name: "Aberdeenshire", lat: 57.308, lon: -2.3393, country: "GB"),
        City(slug: "antrim", name: "Antrim", lat: 54.762, lon: -6.0127, country: "GB"),
        City(slug: "armagh", name: "Armagh", lat: 54.4492, lon: -6.398, country: "GB"),
        City(slug: "ayrshire-and-arran", name: "Ayrshire and Arran", lat: 55.5093, lon: -4.581, country: "GB"),
        City(slug: "bedfordshire", name: "Bedfordshire", lat: 52.0082, lon: -0.4435, country: "GB"),
        City(slug: "belfast", name: "Belfast", lat: 54.5857, lon: -5.9428, country: "GB"),
        City(slug: "berkshire", name: "Berkshire", lat: 51.4268, lon: -0.9169, country: "GB"),
        City(slug: "birmingham", name: "Birmingham", lat: 52.4581, lon: -1.9041, country: "GB"),
        City(slug: "bristol", name: "Bristol", lat: 51.4659, lon: -2.5805, country: "GB"),
        City(slug: "buckinghamshire", name: "Buckinghamshire", lat: 51.7582, lon: -0.7609, country: "GB"),
        City(slug: "cambridgeshire", name: "Cambridgeshire", lat: 52.4301, lon: -0.0137, country: "GB"),
        City(slug: "cardiff", name: "Cardiff", lat: 51.4892, lon: -3.1939, country: "GB"),
        City(slug: "central-scotland", name: "Central Scotland", lat: 56.08, lon: -3.8066, country: "GB"),
        City(slug: "cheshire", name: "Cheshire", lat: 53.2917, lon: -2.4966, country: "GB"),
        City(slug: "clwyd", name: "Clwyd", lat: 53.3083, lon: -3.6072, country: "GB"),
        City(slug: "cornwall", name: "Cornwall", lat: 50.317, lon: -4.9211, country: "GB"),
        City(slug: "county-durham", name: "County Durham", lat: 54.7289, lon: -1.5139, country: "GB"),
        City(slug: "cumbria", name: "Cumbria", lat: 54.4593, lon: -3.1119, country: "GB"),
        City(slug: "derbyshire", name: "Derbyshire", lat: 52.9886, lon: -1.5219, country: "GB"),
        City(slug: "devon", name: "Devon", lat: 50.6651, lon: -3.687, country: "GB"),
        City(slug: "dorset", name: "Dorset", lat: 50.7664, lon: -2.1122, country: "GB"),
        City(slug: "down", name: "Down", lat: 54.4293, lon: -5.9704, country: "GB"),
        City(slug: "dudley", name: "Dudley", lat: 52.497, lon: -2.0918, country: "GB"),
        City(slug: "dumfries-and-galloway", name: "Dumfries and Galloway", lat: 54.9881, lon: -3.8232, country: "GB"),
        City(slug: "dunbartonshire-argyll-bute", name: "Dunbartonshire and Argyll & Bute", lat: 55.7795, lon: -4.9973, country: "GB"),
        City(slug: "dyfed", name: "Dyfed", lat: 51.9892, lon: -4.3329, country: "GB"),
        City(slug: "east-sussex", name: "East Sussex", lat: 50.8499, lon: 0.2215, country: "GB"),
        City(slug: "east-yorkshire", name: "East Yorkshire", lat: 53.8685, lon: -0.3985, country: "GB"),
        City(slug: "edinburgh-and-lothians", name: "Edinburgh & Lothians", lat: 55.9404, lon: -3.2039, country: "GB"),
        City(slug: "essex", name: "Essex", lat: 51.7621, lon: 0.5901, country: "GB"),
        City(slug: "fermanagh", name: "Fermanagh", lat: 54.3499, lon: -7.6316, country: "GB"),
        City(slug: "fife", name: "Fife", lat: 56.1287, lon: -3.2424, country: "GB"),
        City(slug: "glamorgan", name: "Glamorgan", lat: 51.6388, lon: -3.7535, country: "GB"),
        City(slug: "glasgow", name: "Glasgow", lat: 55.8682, lon: -4.2316, country: "GB"),
        City(slug: "gloucestershire", name: "Gloucestershire", lat: 51.8387, lon: -2.2712, country: "GB"),
        City(slug: "guernsey", name: "Guernsey", lat: 49.4446, lon: -2.5695, country: "GB"),
        City(slug: "gwent", name: "Gwent", lat: 51.6882, lon: -3.0066, country: "GB"),
        City(slug: "gwynedd", name: "Gwynedd", lat: 53.0098, lon: -4.153, country: "GB"),
        City(slug: "hampshire", name: "Hampshire", lat: 50.9234, lon: -1.165, country: "GB"),
        City(slug: "herefordshire", name: "Herefordshire", lat: 52.031, lon: -2.7825, country: "GB"),
        City(slug: "hertfordshire", name: "Hertfordshire", lat: 51.7791, lon: -0.3102, country: "GB"),
        City(slug: "highlands-and-islands", name: "Highlands and Islands", lat: 58.086, lon: -4.0855, country: "GB"),
        City(slug: "isle-of-man", name: "Isle of Man", lat: 54.1578, lon: -4.4775, country: "GB"),
        City(slug: "isle-of-wight", name: "Isle of Wight", lat: 50.7118, lon: -1.2248, country: "GB"),
        City(slug: "jersey", name: "Jersey", lat: 49.1839, lon: -2.1144, country: "GB"),
        City(slug: "kent", name: "Kent", lat: 51.2682, lon: 0.8631, country: "GB"),
        City(slug: "lanarkshire", name: "Lanarkshire", lat: 55.7953, lon: -4.0904, country: "GB"),
        City(slug: "lancashire", name: "Lancashire", lat: 53.7367, lon: -2.6625, country: "GB"),
        City(slug: "leicestershire", name: "Leicestershire", lat: 52.6656, lon: -1.1514, country: "GB"),
        City(slug: "lincolnshire", name: "Lincolnshire", lat: 53.2194, lon: -0.2916, country: "GB"),
        City(slug: "londonderry", name: "Londonderry", lat: 54.9949, lon: -7.0636, country: "GB"),
        City(slug: "liverpool", name: "Liverpool", lat: 53.4084, lon: -2.9916, country: "GB"),
        City(slug: "north-yorkshire", name: "North Yorkshire", lat: 54.2402, lon: -1.156, country: "GB"),
        City(slug: "northamptonshire", name: "Northamptonshire", lat: 52.288, lon: -0.8653, country: "GB"),
        City(slug: "northumberland", name: "Northumberland", lat: 55.2158, lon: -1.7422, country: "GB"),
        City(slug: "nottinghamshire", name: "Nottinghamshire", lat: 53.0236, lon: -1.15, country: "GB"),
        City(slug: "oxfordshire", name: "Oxfordshire", lat: 51.7572, lon: -1.2545, country: "GB"),
        City(slug: "powys", name: "Powys", lat: 52.3806, lon: -3.26, country: "GB"),
        City(slug: "renfrewshire", name: "Renfrewshire", lat: 55.9204, lon: -4.5838, country: "GB"),
        City(slug: "roxburgh-ettrick-and-lauderdale", name: "Roxburgh, Ettrick and Lauderdale", lat: 55.5183, lon: -2.7969, country: "GB"),
        City(slug: "sandwell", name: "Sandwell", lat: 52.5175, lon: -1.9932, country: "GB"),
        City(slug: "shropshire", name: "Shropshire", lat: 52.6813, lon: -2.6215, country: "GB"),
        City(slug: "somerset", name: "Somerset", lat: 51.2159, lon: -2.824, country: "GB"),
        City(slug: "south-yorkshire", name: "South Yorkshire", lat: 53.5141, lon: -1.3109, country: "GB"),
        City(slug: "staffordshire", name: "Staffordshire", lat: 52.7942, lon: -1.9887, country: "GB"),
        City(slug: "suffolk", name: "Suffolk", lat: 52.1492, lon: 1.0262, country: "GB"),
        City(slug: "surrey", name: "Surrey", lat: 51.2269, lon: -0.5354, country: "GB"),
        City(slug: "tayside", name: "Tayside", lat: 56.5061, lon: -3.0128, country: "GB"),
        City(slug: "tyne-and-wear", name: "Tyne and Wear", lat: 54.9749, lon: -1.5397, country: "GB"),
        City(slug: "tyrone", name: "Tyrone", lat: 54.5255, lon: -6.8664, country: "GB"),
        City(slug: "warwickshire", name: "Warwickshire", lat: 52.3602, lon: -1.5034, country: "GB"),
        City(slug: "west-sussex", name: "West Sussex", lat: 50.9492, lon: -0.3262, country: "GB"),
        City(slug: "west-yorkshire", name: "West Yorkshire", lat: 53.7878, lon: -1.665, country: "GB"),
        City(slug: "wiltshire", name: "Wiltshire", lat: 51.2955, lon: -1.8505, country: "GB"),
        City(slug: "worcestershire", name: "Worcestershire", lat: 52.1923, lon: -2.2079, country: "GB"),
        City(slug: "yorkshire", name: "Yorkshire", lat: 53.4082, lon: -1.4756, country: "GB"),
    ]

    /// The subset of [all] belonging to `countryCode` (`"PL"`, `"GB"`), in
    /// [all]'s hand-tuned order — the per-country roster the app scopes every
    /// picker and the nearest-city pick to.
    static func cities(in countryCode: String) -> [City] {
        all.filter { $0.country == countryCode }
    }

    /// [cities(in:)] ordered alphabetically by display name under that country's
    /// collation (Polish for PL, so `Ł` sorts after `L`, `Ó` after `O`; English
    /// elsewhere). This is the list the UI pickers iterate; [all] keeps its
    /// hand-tuned order for the default/nearest picks, where order is semantic.
    static func sorted(in countryCode: String) -> [City] {
        let locale = collationLocale(for: countryCode)
        return cities(in: countryCode).sorted {
            $0.name.compare($1.name, options: .caseInsensitive, range: nil, locale: locale) == .orderedAscending
        }
    }

    /// The collation locale for a country's city names: Polish for PL (so the
    /// diacritic letters sort in their alphabet positions), English elsewhere.
    private static func collationLocale(for countryCode: String) -> Locale {
        Locale(identifier: countryCode == "PL" ? "pl_PL" : "en_GB")
    }

    /// Ultimate fallback city — the first modelled city overall (Poznań, the
    /// default country's first). Flows that know the country pick a
    /// country-scoped default via [defaultCity(in:)]; this is only the compile-
    /// time default for the networking stores before the gate sets a real city.
    static let `default` = all[0]

    /// The default city for `countryCode`: the first entry of that country's
    /// hand-ordered roster (Poznań for PL, London for GB). Falls back to the
    /// global [default] if the country lists no cities.
    static func defaultCity(in countryCode: String) -> City {
        cities(in: countryCode).first ?? `default`
    }

    /// The closest city IN `countryCode` to the given coordinate, or `nil` when
    /// even the nearest is more than 100 km away (the user is outside every city
    /// that country serves — let them pick instead of dropping them on a far-off
    /// one). Scoped to the country so a fix in Poland never resolves to a UK
    /// region, or vice versa.
    static func nearestWithin100km(lat: Double, lon: Double, in countryCode: String) -> City? {
        let ranked = cities(in: countryCode)
            .map { (city: $0, km: haversineKm(latitude1: lat, longitude1: lon, latitude2: $0.lat, longitude2: $0.lon)) }
            .min { $0.km < $1.km }
        guard let nearest = ranked, nearest.km <= 100 else { return nil }
        return nearest.city
    }

    /// A "you're nearer another city — switch?" prompt the app should
    /// surface: the city to offer (`target`) and a stable de-dupe `key`
    /// for the `chosen → nearest` pair so the prompt fires at most once
    /// per pair.
    struct CitySwitchSuggestion: Equatable {
        let target: City
        let key: String
    }

    /// Decide whether to nudge a user who has chosen `chosenSlug` toward a
    /// nearer supported city, given their current coordinate. Returns `nil`
    /// when they're already in (or nearest to) the chosen city, when no city
    /// is within range, or when this exact `chosen → nearest` pair was the
    /// most recently prompted one (`lastPromptKey`). Remembering only the
    /// single most-recent key means travelling back to a previously-declined
    /// city can re-ask.
    ///
    /// Pure — no CoreLocation, no persistence — so the rule is unit-tested
    /// in `KinowoCore`; the app feeds it a fix + the remembered key and acts
    /// on the result.
    static func switchSuggestion(
        chosenSlug: String,
        lat: Double,
        lon: Double,
        lastPromptKey: String?,
        in countryCode: String
    ) -> CitySwitchSuggestion? {
        guard let nearest = nearestWithin100km(lat: lat, lon: lon, in: countryCode),
              nearest.slug != chosenSlug else { return nil }
        let key = switchPromptKey(chosenSlug: chosenSlug, nearestSlug: nearest.slug)
        guard key != lastPromptKey else { return nil }
        return CitySwitchSuggestion(target: nearest, key: key)
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

    /// [sorted(in:)] narrowed to the cities matching `query`. Drives the search
    /// box on the manual city picker for `countryCode`.
    static func matching(_ query: String, in countryCode: String) -> [City] {
        sorted(in: countryCode).filter { $0.matches(query) }
    }

    /// Great-circle distance in kilometres between two coordinates.
    private static func haversineKm(latitude1: Double, longitude1: Double, latitude2: Double, longitude2: Double) -> Double {
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
