import Foundation

/// The persisted in-app country choice: which deployment the app talks to.
/// Backed by `UserDefaults` so it's readable at app launch (before any view or
/// store is built) — `kinowoBaseURL` reads `current().baseURL`, so routing
/// every API call through the selection is just a matter of the stores/auth
/// using that global.
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

    /// Persist a new country choice. The UI language is a fully independent
    /// preference (see `LanguageSelection`) — a country switch never changes it.
    static func select(_ country: Country, in defaults: UserDefaults = .standard) {
        defaults.set(country.code, forKey: key)
    }
}
