import XCTest
@testable import KinowoAuth

/// Pins the 4-step language-resolution algorithm (explicit pick → device
/// preferred → storefront region → English) and the storefront→language
/// table. Lives in `KinowoAuthTests` for the same reason `CountryTests` does:
/// `LanguageSelection` is owned by the `KinowoAuth` SPM target because
/// `UserPreferences` routes the resolved/persisted language through it.
final class LanguageSelectionTests: XCTestCase {

    private var defaults: UserDefaults!
    private static let suite = "LanguageSelectionTests"

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    // MARK: - resolve(storefrontCountry:) 4-step algorithm

    func testExplicitPickWinsOverDevicePreferredAndStorefront() {
        LanguageSelection.select("de", in: defaults)
        let resolved = LanguageSelection.resolve(
            storefrontCountry: "ES",
            defaults: defaults,
            preferredLanguages: ["pl-PL"]
        )
        XCTAssertEqual(resolved, "de")
    }

    func testDevicePreferredWinsWhenSupportedAndNoExplicitPick() {
        let resolved = LanguageSelection.resolve(
            storefrontCountry: "ES",
            defaults: defaults,
            preferredLanguages: ["de-DE"]
        )
        XCTAssertEqual(resolved, "de")
    }

    func testStorefrontFallbackFiresWhenDevicePreferredIsNotSupported() {
        let resolved = LanguageSelection.resolve(
            storefrontCountry: "DE",
            defaults: defaults,
            preferredLanguages: ["fr-FR"]
        )
        XCTAssertEqual(resolved, "de")
    }

    func testEnglishWhenNothingMatches() {
        let resolved = LanguageSelection.resolve(
            storefrontCountry: "FR",
            defaults: defaults,
            preferredLanguages: ["fr-FR"]
        )
        XCTAssertEqual(resolved, "en")
    }

    func testEnglishWhenNoStorefrontAndDevicePreferredUnsupported() {
        let resolved = LanguageSelection.resolve(
            storefrontCountry: nil,
            defaults: defaults,
            preferredLanguages: ["fr-FR"]
        )
        XCTAssertEqual(resolved, "en")
    }

    // MARK: - explicit(_:)

    func testExplicitReturnsNilWhenNothingPersisted() {
        XCTAssertNil(LanguageSelection.explicit(defaults))
    }

    func testExplicitReturnsNilForAPersistedButNoLongerSupportedCode() {
        defaults.set("fr", forKey: LanguageSelection.key)
        XCTAssertNil(LanguageSelection.explicit(defaults))
    }

    // MARK: - select(_:in:)

    func testSelectPersistsTheChoiceAndForcesAppleLanguages() {
        LanguageSelection.select("es", in: defaults)
        XCTAssertEqual(LanguageSelection.explicit(defaults), "es")
        XCTAssertEqual(defaults.stringArray(forKey: "AppleLanguages"), ["es"])
    }

    // MARK: - StorefrontLanguage.forCountryCode

    func testStorefrontPolandMapsToPolish() {
        XCTAssertEqual(StorefrontLanguage.forCountryCode("PL"), "pl")
    }

    func testStorefrontGermanSpeakingRegionsMapToGerman() {
        for code in ["DE", "AT", "CH", "LI"] {
            XCTAssertEqual(StorefrontLanguage.forCountryCode(code), "de")
        }
    }

    func testStorefrontSpanishSpeakingRegionsMapToSpanish() {
        for code in ["ES", "MX", "AR", "CO"] {
            XCTAssertEqual(StorefrontLanguage.forCountryCode(code), "es")
        }
    }

    func testStorefrontUnknownRegionMapsToEnglish() {
        XCTAssertEqual(StorefrontLanguage.forCountryCode("JP"), "en")
    }

    func testStorefrontNilMapsToEnglish() {
        XCTAssertEqual(StorefrontLanguage.forCountryCode(nil), "en")
    }
}
