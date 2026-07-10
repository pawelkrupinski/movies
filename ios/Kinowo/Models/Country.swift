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
    /// ISO 3166-1 alpha-2, e.g. `PL`, `GB`. The persisted selection key.
    let code: String
    /// Human-readable label for the country picker.
    let displayName: String
    /// Scheme + host of this country's web deployment; the base every
    /// repertoire/details/auth request is built on.
    let baseURL: URL
    /// BCP-47 primary language subtag forced as the app language when selected.
    let languageCode: String

    /// Every country the app knows about. Poland is first (the default): the
    /// current production deployment with a Polish UI.
    static let all: [Country] = [
        Country(
            code: "PL",
            displayName: "Polska",
            baseURL: URL(string: "https://kinowo.fly.dev")!,
            languageCode: "pl"
        ),
        // TODO(§6): swap this placeholder host for the real UK deployment URL
        // once §6 stands the English deployment up.
        Country(
            code: "GB",
            displayName: "United Kingdom",
            baseURL: URL(string: "https://kinowo.co.uk")!,
            languageCode: "en"
        ),
    ]

    /// Fallback when the user hasn't picked a country: Poland.
    static let `default` = all[0]

    /// The country for `code`, or `default` when nil / unknown.
    static func byCode(_ code: String?) -> Country {
        all.first { $0.code == code } ?? .default
    }
}
