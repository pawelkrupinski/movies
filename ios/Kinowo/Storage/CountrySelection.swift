import Foundation

/// The persisted in-app country choice: which deployment the app talks to and
/// which language it forces. Backed by `UserDefaults` so it's readable at app
/// launch (before any view or store is built) — `kinowoBaseURL` reads
/// `current().baseURL`, so routing every API call through the selection is just
/// a matter of the stores/auth using that global.
///
/// The `defaults` parameter defaults to `.standard` for production; tests inject
/// a throwaway suite (via `UserPreferences(store:)`) to round-trip the choice
/// without touching the real defaults.
enum CountrySelection {
    static let key = "selectedCountryCode"

    /// The currently selected country, or `Country.default` (Poland) when the
    /// user hasn't chosen one.
    static func current(_ defaults: UserDefaults = .standard) -> Country {
        Country.byCode(defaults.string(forKey: key))
    }

    /// Persist a new country choice AND force its language for the next launch.
    /// iOS reads `AppleLanguages` at process start to pick the localized
    /// resource bundle, so the language flip fully lands on relaunch; in-session
    /// views additionally read `locale(_:)` via `environment`.
    static func select(_ country: Country, in defaults: UserDefaults = .standard) {
        defaults.set(country.code, forKey: key)
        defaults.set([country.languageCode], forKey: "AppleLanguages")
    }

    /// The `Locale` to inject into the SwiftUI environment so `Text` /
    /// `String(localized:)` follow the selected country immediately, not just
    /// after relaunch.
    static func locale(_ defaults: UserDefaults = .standard) -> Locale {
        Locale(identifier: current(defaults).languageCode)
    }
}
