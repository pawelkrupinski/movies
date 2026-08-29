import Foundation

/// The app's historical default zone, and the fallback for a country whose
/// catalog entry carries no `timezone` (an older bundled seed, or a future
/// country the server hasn't tagged yet). File-private here because the SPM
/// `KinowoCore` module — which owns the public `TimeZone.warsaw` used by the
/// pruning helpers — isn't a dependency of the `KinowoAuth` target that compiles
/// this file; in the single-module Xcode app both simply coexist.
private let warsawZone = TimeZone(identifier: "Europe/Warsaw") ?? .current

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
    /// The country's local IANA zone (e.g. `Europe/London`), from the catalog's
    /// per-country `timezone`. Past-showtime pruning and the Dziś/Jutro day
    /// buckets reason in this zone, so a London show disappears on London time,
    /// not Warsaw. Defaults to Warsaw when the source omits it.
    let timeZone: TimeZone

    init(code: String, displayName: String, baseURL: URL, languageCode: String,
         timeZone: TimeZone = warsawZone) {
        self.code = code
        self.displayName = displayName
        self.baseURL = baseURL
        self.languageCode = languageCode
        self.timeZone = timeZone
    }

    /// Compile-time FALLBACK registry, used only until the bundled/fetched
    /// catalog loads (and if that ever fails to decode). The live registry is the
    /// `/api/catalog` payload the `CatalogStore` publishes. Poland is first (the
    /// default). Codes match the server (`pl`/`uk`/`de`).
    static let all: [Country] = [
        Country(
            code: "pl",
            displayName: "Polska",
            baseURL: URL(string: "https://kinowo.fly.dev")!,
            languageCode: "pl",
            timeZone: TimeZone(identifier: "Europe/Warsaw") ?? warsawZone
        ),
        Country(
            code: "uk",
            displayName: "United Kingdom",
            baseURL: URL(string: "https://showtimes-uk.fly.dev")!,
            languageCode: "en",
            timeZone: TimeZone(identifier: "Europe/London") ?? warsawZone
        ),
        Country(
            code: "de",
            displayName: "Deutschland",
            baseURL: URL(string: "https://showtimes-de.fly.dev")!,
            languageCode: "de",
            timeZone: TimeZone(identifier: "Europe/Berlin") ?? warsawZone
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
    /// IANA zone id, e.g. `Europe/London`. Optional so an older bundled seed
    /// (or a server that predates the field) still decodes — it then falls back
    /// to Warsaw, exactly the pre-fix behaviour.
    let timezone: String?

    func toCountry() -> Country? {
        guard let url = URL(string: baseUrl) else { return nil }
        let zone = timezone.flatMap { TimeZone(identifier: $0) } ?? warsawZone
        return Country(code: code, displayName: name, baseURL: url, languageCode: language, timeZone: zone)
    }
}
