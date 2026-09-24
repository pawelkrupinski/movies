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

    /// The device-wide set was reconciled against whichever country was
    /// selected, so the per-country validators an older build stored describe
    /// server sets this device never kept apart: replaying them would draw a
    /// 304 and strand the upgraded set (possibly another country's titles, or
    /// an empty never-written bucket) under a "you're current" answer. The
    /// upgrade forgets them, so each country's next reconcile takes a fresh
    /// 200 and adopts the server's set — while staying migrated, so that 200
    /// REPLACES the stale local set rather than being unioned into it.
    func testTheUpgradeForgetsTheValidatorsTheDeviceWideSetWasSyncedUnder() {
        CountrySelection.select(poland, in: defaults)
        defaults.set(["Legacy"], forKey: "hiddenFilms")
        let older = UserPreferences(store: defaults)
        older.setHiddenFilmsMigrated(country: "pl")
        older.setHiddenFilmsMigrated(country: "uk")
        older.setHiddenFilmsValidators(country: "pl", etag: "\"pl\"", lastModified: "lm-pl")
        older.setHiddenFilmsValidators(country: "uk", etag: "\"uk\"", lastModified: "lm-uk")
        defaults.set(["Legacy"], forKey: "hiddenFilms") // the older build's set, still unsettled

        let prefs = UserPreferences(store: defaults)

        XCTAssertNil(prefs.hiddenFilmsValidators(country: "pl").etag)
        XCTAssertNil(prefs.hiddenFilmsValidators(country: "uk").lastModified)
        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: "uk"))
        XCTAssertEqual(prefs.hiddenFilms, ["Legacy"])
    }

    /// Account deletion wipes EVERY country's set, as Android's
    /// `clearAllHiddenFilms` does — `unhideAll` only clears the country being
    /// browsed, which left the deleted account's other-country hides on the
    /// device for the next sign-in's first-login union to upload.
    func testClearAllHiddenFilmsWipesEveryCountry() {
        let prefs = UserPreferences(store: defaults)
        prefs.hide("Film PL")
        prefs.setHiddenFilms(["Film UK"], country: "uk")

        prefs.clearAllHiddenFilms()

        XCTAssertEqual(prefs.hiddenFilms, [])
        XCTAssertEqual(prefs.hiddenFilms(country: "uk"), [])
        XCTAssertEqual(UserPreferences(store: defaults).hiddenFilms(country: "pl"), [])
    }
}
