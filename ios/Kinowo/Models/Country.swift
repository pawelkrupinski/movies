import Foundation

/// A country the app can serve. Each country is its own web deployment
/// (`baseURL`) serving its own localized `/{city}/api/repertoire` +
/// `/{city}/api/details`, and carries the UI `languageCode` the app forces when
/// that country is selected — deliberately NOT derived from the device locale,
/// so a Polish phone browsing the UK deployment still reads English and the
/// choice stays deterministic and testable.
///
/// Pure Foundation — no SwiftUI / UIKit — so it lives in `KinowoCore` and the
/// registry logic is unit-tested cross-platform (`swift test` on Linux CI).
/// Mirrors the Android `Country` registry one-for-one so the two apps agree on
/// the set of countries, their base URLs, and their forced languages.
struct Country: Codable, Hashable {
    /// Server country code, e.g. `pl`, `uk` — the single code space the catalog
    /// keys on (cities carry the same code). Also the persisted selection key.
    let code: String
    /// Human-readable label for the country picker.
    let displayName: String
    /// Scheme + host of this country's web deployment; the base every
    /// repertoire/details/auth request is built on.
    let baseURL: URL
    /// BCP-47 primary language subtag forced as the app language when selected.
    let languageCode: String

    /// Compile-time FALLBACK registry, used only until the bundled/fetched
    /// catalog loads (and if that ever fails to decode). The live registry is the
    /// `/api/catalog` payload the `CatalogStore` publishes. Poland is first (the
    /// default). Codes match the server (`pl`/`uk`/`de`).
    static let all: [Country] = [
        Country(
            code: "pl",
            displayName: "Polska",
            baseURL: URL(string: "https://kinowo.fly.dev")!,
            languageCode: "pl"
        ),
        Country(
            code: "uk",
            displayName: "United Kingdom",
            baseURL: URL(string: "https://showtimes-uk.fly.dev")!,
            languageCode: "en"
        ),
        Country(
            code: "de",
            displayName: "Deutschland",
            baseURL: URL(string: "https://showtimes-de.fly.dev")!,
            languageCode: "de"
        ),
    ]

    /// Fallback when the user hasn't picked a country: Poland.
    static let `default` = all[0]

    /// The country for `code` in the fallback registry, or `default` when nil /
    /// unknown. Bootstrap only (e.g. `kinowoBaseURL` at launch, before the
    /// catalog loads); live lookups use the `CatalogStore`'s countries.
    static func byCode(_ code: String?) -> Country {
        all.first { $0.code == normalizeCode(code) } ?? .default
    }

    /// Map a legacy persisted selection code to the current server code space.
    /// Earlier builds stored ISO codes (`PL`/`GB`); the catalog keys on `pl`/`uk`.
    /// Applied wherever a persisted code is read so an upgrade keeps the user's
    /// country without a migration write (the next selection persists the new code).
    static func normalizeCode(_ code: String?) -> String? {
        switch code {
        case "PL": return "pl"
        case "GB": return "uk"
        default:   return code
        }
    }
}

/// Registry lookups over a catalog's country list — the live list the
/// `CatalogStore` holds (fetched or seeded), so a country added server-side
/// appears without an app update.
extension Array where Element == Country {
    /// The country for `code`, or `nil` when absent (e.g. a decommissioned one
    /// the user still has selected — the caller then falls back to the default).
    func withCode(_ code: String?) -> Country? { first { $0.code == code } }

    /// Whether an in-app country switcher is worth showing (more than one
    /// deployed country). With one there's nothing to switch to.
    var isSwitchable: Bool { count > 1 }
}

/// Wire shape of one country in the `/api/catalog` payload (`{code,name,baseUrl,
/// language,brand}`). Decoded then mapped to [Country]; `brand` is ignored (the
/// apps render no brand text). A row with an unparseable `baseUrl` is dropped.
struct CountryDTO: Decodable {
    let code: String
    let name: String
    let baseUrl: String
    let language: String

    func toCountry() -> Country? {
        guard let url = URL(string: baseUrl) else { return nil }
        return Country(code: code, displayName: name, baseURL: url, languageCode: language)
    }
}
