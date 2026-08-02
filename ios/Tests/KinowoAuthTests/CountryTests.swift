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

    /// The registry holds ONLY countries a deployment actually serves, so it
    /// never hands out a base URL that can't answer. UK and Germany were stopped
    /// on 2026-08-02 and dropped from it.
    func testRegistryHoldsOnlyTheDeployedCountry() {
        XCTAssertEqual(Country.all.map(\.code), ["pl"])
        XCTAssertFalse(Country.all.contains { $0.baseURL.absoluteString.contains("showtimes-uk") })
        XCTAssertFalse(Country.all.contains { $0.baseURL.absoluteString.contains("showtimes-de") })
    }

    /// The upgrade path for a user who had UK or Germany selected: `byCode`
    /// resolves the PERSISTED code through the registry and `kinowoBaseURL`
    /// sends every request to the result, so a code whose deployment is gone
    /// must land on the live one — not pin the app to a stopped host.
    func testPersistedStoppedCountryFallsBackToTheLiveDeployment() {
        for stopped in ["uk", "de"] {
            let resolved = Country.byCode(stopped)
            XCTAssertEqual(resolved, .default)
            XCTAssertEqual(resolved.baseURL.absoluteString, "https://kinowo.fly.dev")
        }
    }

    /// Legacy persisted ISO codes (`PL`/`GB` from earlier builds) normalize to
    /// the current server code space. `GB` still maps to `uk` — the code space is
    /// unchanged; it's the registry lookup that then falls back to the deployed
    /// country, so the mapping stays correct if the UK deployment ever returns.
    func testLegacyIsoCodesNormalizeToServerCodes() {
        XCTAssertEqual(Country.normalizeCode("PL"), "pl")
        XCTAssertEqual(Country.normalizeCode("GB"), "uk")
        XCTAssertEqual(Country.normalizeCode("uk"), "uk")
        XCTAssertEqual(Country.byCode("PL").code, "pl")
        XCTAssertEqual(Country.byCode("GB"), .default)   // uk is undeployed → Poland
    }

    func testUnknownOrNilCodeFallsBackToDefault() {
        XCTAssertEqual(Country.byCode(nil), .default)
        XCTAssertEqual(Country.byCode("ZZ"), .default)
    }

    func testEveryCountryHasADistinctCodeAndBaseUrl() {
        XCTAssertEqual(Country.all.count, Set(Country.all.map(\.code)).count)
        XCTAssertEqual(Country.all.count, Set(Country.all.map(\.baseURL)).count)
    }

    /// The selected country forces the language; it is not device-derived. With
    /// one deployed country every code resolves to it, so every launch is Polish.
    func testCountryDeterminesLanguageNotDeviceLocale() {
        XCTAssertEqual(Country.byCode("pl").languageCode, "pl")
        XCTAssertEqual(Country.byCode("uk").languageCode, "pl")
    }

    /// The visibility rule BOTH country controls gate on — the first-launch
    /// gate's "Kraj" section and the Filtry sheet's picker. One deployed country
    /// means nothing to switch to, so neither renders.
    func testASingleDeployedCountryIsNotSwitchable() {
        XCTAssertEqual(Country.all.count, 1)
        XCTAssertFalse(Country.all.isSwitchable)
    }

    /// …and the rule is about the LIST, not a hardcoded country count: a catalog
    /// that ever carries two countries again turns both controls back on.
    func testTwoCountriesInTheCatalogAreSwitchableAgain() {
        let second = Country(code: "uk", displayName: "United Kingdom",
                             baseURL: URL(string: "https://example.test")!, languageCode: "en")
        XCTAssertTrue((Country.all + [second]).isSwitchable)
    }

    // MARK: - In-app country switch (Filtry "Kraj" section)

    /// A country the app did NOT compile in — the switch is driven by the live
    /// catalog's list, so the mechanism has to work for a country that isn't in
    /// the one-entry registry. Stands in for the stopped UK deployment.
    private static let undeployedUK = Country(
        code: "uk",
        displayName: "United Kingdom",
        baseURL: URL(string: "https://showtimes-uk.fly.dev")!,
        languageCode: "en"
    )

    /// The in-app switch at the model level: picking a country repoints the base
    /// URL to that country's deployment AND clears the selected city so the gate
    /// re-asks (the old city may not exist under the new host). Exactly what the
    /// Filtry "Kraj" picker's `set` closure runs.
    func testInAppCountrySwitchRepointsTheBaseAndResetsCity() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        XCTAssertEqual(prefs.selectedCity, "poznan")

        prefs.setCountry(Self.undeployedUK)
        prefs.clearCity()

        XCTAssertEqual(prefs.selectedCountry.code, "uk")
        XCTAssertEqual(prefs.selectedCountry.baseURL.absoluteString, "https://showtimes-uk.fly.dev")
        XCTAssertNil(prefs.selectedCity)
    }

    /// …but a selection whose deployment is GONE does not survive the relaunch.
    /// `CountrySelection.current` re-resolves the persisted code through the
    /// registry, so the next launch lands on the country that still answers
    /// instead of pinning `kinowoBaseURL` to a stopped host. This is the upgrade
    /// path for everyone who had UK or Germany selected before 2026-08-02.
    func testASelectionWhoseDeploymentIsGoneFallsBackOnRelaunch() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCountry(Self.undeployedUK)

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCountry, .default)
        XCTAssertEqual(CountrySelection.current(defaults).baseURL.absoluteString, "https://kinowo.fly.dev")
    }

    // MARK: - Persistence round-trip (via UserPreferences' store)

    func testSelectedCountryDefaultsToPolandUntilChosen() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertEqual(prefs.selectedCountry, .default)
        XCTAssertEqual(prefs.selectedCountry.code, "pl")
    }

    func testSetCountryPersistsAndSurvivesAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        prefs.setCountry(.default)
        XCTAssertEqual(prefs.selectedCountry.code, "pl")

        // A fresh instance over the same store reads the persisted choice —
        // exactly what `kinowoBaseURL` sees at the next launch.
        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCountry.code, "pl")
        XCTAssertEqual(CountrySelection.current(defaults).baseURL.absoluteString, "https://kinowo.fly.dev")
    }

    func testSelectingCountryForcesItsLanguageTag() {
        let prefs = UserPreferences(store: defaults)
        // The tag written is the SELECTED country's, not the device's — iOS
        // reads AppleLanguages at launch to pick the localized bundle.
        prefs.setCountry(Self.undeployedUK)
        XCTAssertEqual(defaults.stringArray(forKey: "AppleLanguages"), ["en"])

        prefs.setCountry(.default)
        XCTAssertEqual(defaults.stringArray(forKey: "AppleLanguages"), ["pl"])
        XCTAssertEqual(CountrySelection.locale(defaults).identifier, "pl")
    }
}
