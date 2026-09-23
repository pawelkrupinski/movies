import XCTest
@testable import KinowoAuth

/// hiddenFilms is per country — server-side (`/api/me/{country}/hidden-films`)
/// and so locally too, as on Android. A single device-wide set let one
/// country's hides leak into another on a country switch.
final class UserPreferencesHiddenFilmsPerCountryTests: XCTestCase {

    private static let suite = "UserPreferencesHiddenFilmsPerCountryTests"
    private var defaults: UserDefaults!

    private let poland        = Country.all.first { $0.code == "pl" }!
    private let unitedKingdom = Country.all.first { $0.code == "uk" }!
    private let germany       = Country.all.first { $0.code == "de" }!

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    func testACountrySwitchShowsThatCountrysOwnSetAndSwitchingBackRestoresTheFirst() {
        let prefs = UserPreferences(store: defaults)
        prefs.hide("Film PL")

        prefs.setCountry(unitedKingdom)
        XCTAssertEqual(prefs.hiddenFilms, [])
        prefs.hide("Film UK")

        prefs.setCountry(poland)
        XCTAssertEqual(prefs.hiddenFilms, ["Film PL"])
        XCTAssertEqual(prefs.hiddenFilms(country: "uk"), ["Film UK"])
    }

    func testUnhideAllClearsOnlyTheCurrentCountry() {
        let prefs = UserPreferences(store: defaults)
        prefs.setHiddenFilms(["Film UK"], country: "uk")
        prefs.hide("Film PL")

        prefs.unhideAll()

        XCTAssertEqual(prefs.hiddenFilms, [])
        XCTAssertEqual(prefs.hiddenFilms(country: "uk"), ["Film UK"])
    }

    func testWritingAnotherCountrysSetLeavesTheVisibleOneAlone() {
        let prefs = UserPreferences(store: defaults)
        prefs.hide("Film PL")

        prefs.setHiddenFilms(["Film UK"], country: "uk")

        XCTAssertEqual(prefs.hiddenFilms, ["Film PL"])
    }

    func testEachCountrysSetSurvivesARelaunch() {
        let prefs = UserPreferences(store: defaults)
        prefs.hide("Film PL")
        prefs.setCountry(unitedKingdom)
        prefs.hide("Film UK")

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.hiddenFilms, ["Film UK"])
        XCTAssertEqual(reloaded.hiddenFilms(country: "pl"), ["Film PL"])
    }

    /// An upgrade from the device-wide set: it belongs to the country the
    /// device was browsing, and stays with THAT country after a switch rather
    /// than following the user into the next one.
    func testTheLegacyDeviceWideSetStaysWithTheCountryItWasMadeIn() {
        CountrySelection.select(unitedKingdom, in: defaults)
        defaults.set(["Legacy"], forKey: "hiddenFilms")

        let prefs = UserPreferences(store: defaults)
        XCTAssertEqual(prefs.hiddenFilms, ["Legacy"])

        prefs.setCountry(germany)
        XCTAssertEqual(prefs.hiddenFilms, [])

        prefs.setCountry(unitedKingdom)
        XCTAssertEqual(prefs.hiddenFilms, ["Legacy"])
        XCTAssertEqual(UserPreferences(store: defaults).hiddenFilms(country: "uk"), ["Legacy"])
    }
}
