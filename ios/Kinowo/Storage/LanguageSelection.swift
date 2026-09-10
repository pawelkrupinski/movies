import Foundation

/// The persisted UI-language choice, independent of `CountrySelection` — a
/// user browsing the UK deployment in German stays in German even after
/// switching country. Backed by `UserDefaults` (like `CountrySelection`) so
/// it's readable at app launch, before any view or store is built.
///
/// The `defaults` parameter defaults to `.standard` for production; tests
/// inject a throwaway suite (via `UserPreferences(store:)`) to round-trip the
/// choice without touching the real defaults.
enum LanguageSelection {
    static let key = "selectedLanguageCode"

    /// The four fully-localized languages the app ships (`Localizable.xcstrings`).
    static let supported = ["pl", "en", "de", "es"]

    /// The persisted explicit pick, if any and if it's still one of the
    /// supported languages (a future build could drop one; a stale persisted
    /// code should not survive that).
    static func explicit(_ defaults: UserDefaults = .standard) -> String? {
        guard let code = defaults.string(forKey: key), supported.contains(code) else { return nil }
        return code
    }

    /// Persist an explicit language pick AND force it for the next launch.
    /// iOS reads `AppleLanguages` at process start to pick the localized
    /// resource bundle, so the language flip fully lands on relaunch; in-session
    /// views additionally read the SwiftUI `.environment(\.locale)` set from
    /// `UserPreferences.selectedLanguage`.
    static func select(_ code: String, in defaults: UserDefaults = .standard) {
        defaults.set(code, forKey: key)
        defaults.set([code], forKey: "AppleLanguages")
    }

    /// The 4-step resolution algorithm — shared shape with the web/Android
    /// implementations (Scala/Kotlin can't share code with Swift, so each
    /// platform carries its own copy):
    ///
    /// 1. Explicit persisted pick, if any.
    /// 2. Device-preferred language, if it's one of `supported`.
    /// 3. The App Store storefront's region, mapped to a language, if it hits.
    /// 4. English.
    ///
    /// `storefrontCountry` is an injectable parameter rather than a direct
    /// StoreKit call so this stays a pure, testable function — the caller
    /// (`KinowoApp`) supplies it once the async `Storefront.current` lookup
    /// resolves, since a synchronous initializer can't await it.
    static func resolve(
        storefrontCountry: String?,
        defaults: UserDefaults = .standard,
        preferredLanguages: [String] = Locale.preferredLanguages
    ) -> String {
        if let explicit = explicit(defaults) { return explicit }
        if let devicePreferred = preferredLanguages.first.map(languageSubtag),
           supported.contains(devicePreferred) {
            return devicePreferred
        }
        return StorefrontLanguage.forCountryCode(storefrontCountry)
    }

    /// The BCP-47 primary language subtag of a locale identifier, e.g.
    /// `"en-GB"` → `"en"`, `"de"` → `"de"`.
    private static func languageSubtag(_ identifier: String) -> String {
        Locale(identifier: identifier).language.languageCode?.identifier ?? identifier
    }
}

/// App Store storefront region → UI language, for the case where the device's
/// preferred language isn't one of `LanguageSelection.supported`. A NEW table:
/// `Country` only models the app's 5 deployed countries, not the much wider
/// set of App Store storefronts, so it can't be reused here.
enum StorefrontLanguage {
    private static let german: Set<String> = ["DE", "AT", "CH", "LI"]
    private static let spanish: Set<String> = [
        "ES", "MX", "AR", "CO", "PE", "CL", "VE", "EC", "GT",
        "CU", "BO", "DO", "HN", "PY", "SV", "NI", "CR", "PA", "UY", "PR",
    ]

    static func forCountryCode(_ code: String?) -> String {
        guard let code else { return "en" }
        if code == "PL" { return "pl" }
        if german.contains(code) { return "de" }
        if spanish.contains(code) { return "es" }
        return "en"
    }
}

/// Native display names for the language picker (Filtry sheet), keyed by the
/// same BCP-47 subtags as `LanguageSelection.supported`.
enum LanguageDisplayName {
    private static let native: [String: String] = [
        "pl": "Polski",
        "en": "English",
        "de": "Deutsch",
        "es": "Español",
    ]

    static func native(_ code: String) -> String {
        native[code] ?? code
    }
}
