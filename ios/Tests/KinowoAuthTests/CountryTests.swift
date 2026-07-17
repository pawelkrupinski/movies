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
        XCTAssertEqual(pl.code, "pl")
        XCTAssertEqual(pl.baseURL.absoluteString, "https://kinowo.fly.dev")
        XCTAssertEqual(pl.languageCode, "pl")
    }

    func testUkEntryForcesEnglishOnItsOwnDeployment() {
        let uk = Country.byCode("uk")
        XCTAssertEqual(uk.code, "uk")
        XCTAssertEqual(uk.displayName, "United Kingdom")
        XCTAssertEqual(uk.baseURL.absoluteString, "https://showtimes-uk.fly.dev")
        XCTAssertEqual(uk.languageCode, "en")
    }

    func testDeEntryForcesGermanOnItsOwnDeployment() {
        let de = Country.byCode("de")
        XCTAssertEqual(de.code, "de")
        XCTAssertEqual(de.displayName, "Deutschland")
        XCTAssertEqual(de.baseURL.absoluteString, "https://showtimes-de.fly.dev")
        XCTAssertEqual(de.languageCode, "de")
    }

    /// Legacy persisted ISO codes (`PL`/`GB` from earlier builds) normalize to
    /// the current server code space so an upgrade keeps the user's country.
    func testLegacyIsoCodesNormalizeToServerCodes() {
        XCTAssertEqual(Country.byCode("PL").code, "pl")
        XCTAssertEqual(Country.byCode("GB").code, "uk")
        XCTAssertEqual(Country.normalizeCode("PL"), "pl")
        XCTAssertEqual(Country.normalizeCode("GB"), "uk")
        XCTAssertEqual(Country.normalizeCode("uk"), "uk")
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
        XCTAssertEqual(Country.byCode("pl").languageCode, "pl")
        XCTAssertEqual(Country.byCode("uk").languageCode, "en")
    }

    /// The in-app "Kraj" section renders only when there's more than one
    /// country to switch between — the visibility rule the Filtry sheet gates on.
    func testMoreThanOneDeployedCountryIsSwitchable() {
        XCTAssertGreaterThan(Country.all.count, 1)
        XCTAssertTrue(Country.all.isSwitchable)
    }

    // MARK: - In-app country switch (Filtry "Kraj" section)

    /// The in-app switch at the model level: picking a country persists its
    /// code, repoints the base URL to that country's deployment, AND clears the
    /// selected city so the gate re-asks (the old city may not exist under the
    /// new host). Exactly what the Filtry "Kraj" picker's `set` closure runs.
    func testInAppCountrySwitchPersistsCodeRepointsBaseAndResetsCity() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        XCTAssertEqual(prefs.selectedCity, "poznan")

        prefs.setCountry(Country.byCode("GB"))
        prefs.clearCity()

        XCTAssertEqual(prefs.selectedCountry.code, "uk")
        XCTAssertEqual(
            CountrySelection.current(defaults).baseURL.absoluteString,
            "https://showtimes-uk.fly.dev"
        )
        XCTAssertNil(prefs.selectedCity)

        // A fresh instance sees the same post-switch state at the next launch.
        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCountry.code, "uk")
        XCTAssertNil(reloaded.selectedCity)
    }

    // MARK: - Persistence round-trip (via UserPreferences' store)

    func testSelectedCountryDefaultsToPolandUntilChosen() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertEqual(prefs.selectedCountry, .default)
        XCTAssertEqual(prefs.selectedCountry.code, "pl")
    }

    func testSetCountryPersistsAndSurvivesAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCountry(Country.byCode("GB"))
        XCTAssertEqual(prefs.selectedCountry.code, "uk")

        // A fresh instance over the same store reads the persisted choice —
        // exactly what `kinowoBaseURL` sees at the next launch.
        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCountry.code, "uk")
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
