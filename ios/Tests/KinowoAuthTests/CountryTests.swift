import XCTest
@testable import KinowoAuth

/// Pins the static country registry AND the persisted selection round-trip.
/// Lives in `KinowoAuthTests` (not `KinowoCoreTests`) because `Country` +
/// `CountrySelection` are owned by the `KinowoAuth` SPM target — `kinowoBaseURL`
/// and `UserPreferences` route the base URL / language through them, and SPM
/// forbids sharing one file across two targets. Mirrors the Android `CountryTest`
/// so the two apps can't drift.
final class CountryTests: XCTestCase {

    private var defaults: UserDefaults!
    private static let suite = "CountryTests"

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    // MARK: - Registry

    func testDefaultIsPolandOnTheProdDeployment() {
        let pl = Country.default
        XCTAssertEqual(pl.code, "PL")
        XCTAssertEqual(pl.baseURL.absoluteString, "https://kinowo.fly.dev")
        XCTAssertEqual(pl.languageCode, "pl")
    }

    func testUkEntryForcesEnglishOnItsOwnDeployment() {
        let gb = Country.byCode("GB")
        XCTAssertEqual(gb.displayName, "United Kingdom")
        XCTAssertEqual(gb.baseURL.absoluteString, "https://showtimes-uk.fly.dev")
        XCTAssertEqual(gb.languageCode, "en")
    }

    func testUnknownOrNilCodeFallsBackToDefault() {
        XCTAssertEqual(Country.byCode(nil), .default)
        XCTAssertEqual(Country.byCode("ZZ"), .default)
    }

    func testEveryCountryHasADistinctCodeAndBaseUrl() {
        XCTAssertEqual(Country.all.count, Set(Country.all.map(\.code)).count)
        XCTAssertEqual(Country.all.count, Set(Country.all.map(\.baseURL)).count)
    }

    /// The switch is language-forcing, not device-derived.
    func testCountryDeterminesLanguageNotDeviceLocale() {
        XCTAssertEqual(Country.byCode("PL").languageCode, "pl")
        XCTAssertEqual(Country.byCode("GB").languageCode, "en")
    }

    // MARK: - Persistence round-trip (via UserPreferences' store)

    func testSelectedCountryDefaultsToPolandUntilChosen() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertEqual(prefs.selectedCountry, .default)
        XCTAssertEqual(prefs.selectedCountry.code, "PL")
    }

    func testSetCountryPersistsAndSurvivesAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCountry(Country.byCode("GB"))
        XCTAssertEqual(prefs.selectedCountry.code, "GB")

        // A fresh instance over the same store reads the persisted choice —
        // exactly what `kinowoBaseURL` sees at the next launch.
        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCountry.code, "GB")
        XCTAssertEqual(CountrySelection.current(defaults).baseURL.absoluteString, "https://showtimes-uk.fly.dev")
    }

    func testSelectingCountryForcesItsLanguageTag() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCountry(Country.byCode("GB"))
        // iOS reads AppleLanguages at launch to pick the localized bundle.
        XCTAssertEqual(defaults.stringArray(forKey: "AppleLanguages"), ["en"])
        XCTAssertEqual(CountrySelection.locale(defaults).identifier, "en")
    }
}
