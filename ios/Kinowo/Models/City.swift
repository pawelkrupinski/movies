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

    /// Every city the app knows about. One for now; the first-launch gate
    /// and the Filtry "Miasto" picker iterate this list so adding a row
    /// here is the only change a second city needs on the model side.
    static let all: [City] = [
        City(slug: "poznan", name: "Poznań", lat: 52.4064, lon: 16.9252),
        City(slug: "wroclaw", name: "Wrocław", lat: 51.1079, lon: 17.0385),
        City(slug: "warszawa", name: "Warszawa", lat: 52.2297, lon: 21.0122),
    ]

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
        let key = chosenSlug + "→" + nearest.slug
        guard key != lastPromptKey else { return nil }
        return CitySwitchSuggestion(target: nearest, key: key)
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
