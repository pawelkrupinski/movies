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

    /// Every city the app knows about, in the same order the web `City.all`
    /// lists them so the "Miasto" picker reads identically across platforms.
    /// The first-launch gate and the Filtry "Miasto" picker iterate this list,
    /// so adding a row here is the only change a new city needs on the model side.
    static let all: [City] = [
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252),
        City(slug: "wroclaw", name: "Wrocław", lat: 51.1079, lon: 17.0385),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122),
        City(slug: "krakow", name: "Kraków", lat: 50.0647, lon: 19.9450),
        City(slug: "lodz", name: "Łódź", lat: 51.7592, lon: 19.4560),
        City(slug: "katowice", name: "Katowice", lat: 50.2649, lon: 19.0238),
        City(slug: "szczecin", name: "Szczecin", lat: 53.4285, lon: 14.5528),
        City(slug: "bialystok", name: "Białystok", lat: 53.1325, lon: 23.1688),
        City(slug: "trojmiasto", name: "Trójmiasto", lat: 54.4416, lon: 18.5601),
        City(slug: "bydgoszcz", name: "Bydgoszcz", lat: 53.1235, lon: 18.0084),
        City(slug: "lublin", name: "Lublin", lat: 51.2465, lon: 22.5684),
        City(slug: "czestochowa", name: "Częstochowa", lat: 50.8118, lon: 19.1203),
        City(slug: "radom", name: "Radom", lat: 51.4027, lon: 21.1471),
        City(slug: "sosnowiec", name: "Sosnowiec", lat: 50.2863, lon: 19.1041),
        City(slug: "torun", name: "Toruń", lat: 53.0138, lon: 18.5984),
        City(slug: "kielce", name: "Kielce", lat: 50.8661, lon: 20.6286),
        City(slug: "rzeszow", name: "Rzeszów", lat: 50.0413, lon: 21.9990),
        City(slug: "gliwice", name: "Gliwice", lat: 50.2945, lon: 18.6714),
        City(slug: "zabrze", name: "Zabrze", lat: 50.3249, lon: 18.7857),
        // The 22 mid-size cities that round the catalogue out to 41. They serve
        // an empty repertoire until the worker wires their venues, but appear in
        // the nearest-city pick + "Miasto" picker now (mirrors web `City.all`).
        City(slug: "olsztyn", name: "Olsztyn", lat: 53.7784, lon: 20.4801),
        City(slug: "bielsko-biala", name: "Bielsko-Biała", lat: 49.8224, lon: 19.0584),
        City(slug: "opole", name: "Opole", lat: 50.6751, lon: 17.9213),
        City(slug: "rybnik", name: "Rybnik", lat: 50.0971, lon: 18.5416),
        City(slug: "gorzow-wielkopolski", name: "Gorzów Wielkopolski", lat: 52.7368, lon: 15.2288),
        City(slug: "elblag", name: "Elbląg", lat: 54.1522, lon: 19.4088),
        City(slug: "koszalin", name: "Koszalin", lat: 54.1943, lon: 16.1722),
        City(slug: "kalisz", name: "Kalisz", lat: 51.7611, lon: 18.0911),
        City(slug: "zielona-gora", name: "Zielona Góra", lat: 51.9356, lon: 15.5062),
        City(slug: "tychy", name: "Tychy", lat: 50.1357, lon: 18.9985),
        City(slug: "walbrzych", name: "Wałbrzych", lat: 50.7714, lon: 16.2845),
        City(slug: "tarnow", name: "Tarnów", lat: 50.0121, lon: 20.9858),
        City(slug: "wloclawek", name: "Włocławek", lat: 52.6483, lon: 19.0677),
        City(slug: "legnica", name: "Legnica", lat: 51.2070, lon: 16.1619),
        City(slug: "plock", name: "Płock", lat: 52.5468, lon: 19.7064),
        City(slug: "bytom", name: "Bytom", lat: 50.3483, lon: 18.9157),
        City(slug: "dabrowa-gornicza", name: "Dąbrowa Górnicza", lat: 50.3219, lon: 19.1876),
        City(slug: "nowy-sacz", name: "Nowy Sącz", lat: 49.6175, lon: 20.7154),
        City(slug: "slupsk", name: "Słupsk", lat: 54.4641, lon: 17.0287),
        City(slug: "jelenia-gora", name: "Jelenia Góra", lat: 50.9044, lon: 15.7197),
        City(slug: "przemysl", name: "Przemyśl", lat: 49.7838, lon: 22.7677),
        City(slug: "konin", name: "Konin", lat: 52.2230, lon: 18.2511),
    ]

    /// [all] ordered alphabetically by display name under Polish collation, so
    /// the UI city pickers read A→Z with `Ł` after `L`, `Ó` after `O`, etc.
    /// rather than dumping the diacritic letters at the end. This is the list
    /// the pickers iterate; [all] keeps its hand-tuned order for `default` and
    /// the nearest-city pick, where order is semantic.
    static let allSorted: [City] = all.sorted {
        $0.name.compare($1.name, options: .caseInsensitive, range: nil, locale: Locale(identifier: "pl_PL")) == .orderedAscending
    }

    /// Fallback when no fix is available and the user hasn't chosen.
    static let `default` = all[0]

    /// The closest known city to the given coordinate, or `nil` when even
    /// the nearest is more than 100 km away (the user is outside every city
    /// we serve — let them pick instead of dropping them on a far-off one).
    static func nearestWithin100km(lat: Double, lon: Double) -> City? {
        let ranked = all
            .map { (city: $0, km: haversineKm(lat1: lat, lon1: lon, lat2: $0.lat, lon2: $0.lon)) }
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
        lastPromptKey: String?
    ) -> CitySwitchSuggestion? {
        guard let nearest = nearestWithin100km(lat: lat, lon: lon),
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

    /// Great-circle distance in kilometres between two coordinates.
    private static func haversineKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double) -> Double {
        let earthRadiusKm = 6_371.0
        let dLat = (lat2 - lat1) * .pi / 180
        let dLon = (lon2 - lon1) * .pi / 180
        let rLat1 = lat1 * .pi / 180
        let rLat2 = lat2 * .pi / 180
        let a = sin(dLat / 2) * sin(dLat / 2)
            + sin(dLon / 2) * sin(dLon / 2) * cos(rLat1) * cos(rLat2)
        let c = 2 * atan2(sqrt(a), sqrt(1 - a))
        return earthRadiusKm * c
    }
}
