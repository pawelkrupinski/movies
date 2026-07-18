import XCTest
@testable import KinowoAuth

final class UserPreferencesCityTests: XCTestCase {

    private var defaults: UserDefaults!

    private static let suite = "UserPreferencesCityTests"

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    func testSelectedCityStartsNil() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertNil(prefs.selectedCity)
    }

    func testSetCityPersistsAndSurvivesAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        XCTAssertEqual(prefs.selectedCity, "poznan")

        // A fresh instance over the same store reads the persisted slug —
        // mirrors a relaunch.
        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCity, "poznan")
    }

    func testSetCityOverwritesAPreviousChoice() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        prefs.setCity("warszawa")
        XCTAssertEqual(prefs.selectedCity, "warszawa")

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCity, "warszawa")
    }

    // MARK: - Excluded cinemas

    func testDisabledCinemasStartsEmpty() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertTrue(prefs.disabledCinemas.isEmpty)
    }

    func testDisabledCinemasPersistAndSurviveAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas(["Kino Muza"])
        XCTAssertEqual(prefs.disabledCinemas, ["Kino Muza"])

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.disabledCinemas, ["Kino Muza"])
    }

    func testReEnablingACinemaClearsItFromThePersistedSet() {
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas(["Kino Muza"])
        prefs.setDisabledCinemas([])
        XCTAssertTrue(prefs.disabledCinemas.isEmpty)

        let reloaded = UserPreferences(store: defaults)
        XCTAssertTrue(reloaded.disabledCinemas.isEmpty)
    }

    func testSwitchingCityKeepsTheExcludedSet() {
        // The excluded set is GLOBAL across cities (it mirrors the web's
        // localStorage and syncs to the account), and is scoped to the current
        // city at read time — so a city switch must NOT reset it. Wiping it here
        // would silently re-enable everything the user hid in the other city.
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        prefs.setDisabledCinemas(["Kino Muza"])
        prefs.setCity("warszawa")
        XCTAssertEqual(prefs.disabledCinemas, ["Kino Muza"])

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.disabledCinemas, ["Kino Muza"])
    }

    func testClearCityRegatesButKeepsTheExcludedSet() {
        // Same reasoning as `setCity`: the in-app country switch re-gates to the
        // city chooser, but the user's per-cinema picks stay theirs.
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        prefs.setDisabledCinemas(["Kino Muza"])
        prefs.clearCity()
        XCTAssertNil(prefs.selectedCity)
        XCTAssertEqual(prefs.disabledCinemas, ["Kino Muza"])
    }

    func testClearCityIsANoOpWhenNoCityIsSet() {
        let prefs = UserPreferences(store: defaults)
        prefs.clearCity()
        XCTAssertNil(prefs.selectedCity)
    }

    func testTheWholeSetIsReplacedAsWritten() {
        // `setDisabledCinemas` is the single writer and stores verbatim — the
        // per-cinema / per-area arithmetic (and its cross-city safety) lives in
        // `CinemaFilterSection`, covered by CinemaFilterSectionTests.
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas(["Kino Pod Baranami", "Odeon Camden"])
        XCTAssertEqual(prefs.disabledCinemas, ["Kino Pod Baranami", "Odeon Camden"])

        prefs.setDisabledCinemas(["Kino Pod Baranami"])
        XCTAssertEqual(prefs.disabledCinemas, ["Kino Pod Baranami"])

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.disabledCinemas, ["Kino Pod Baranami"])
    }

    func testCitySwitchPromptKeyStartsNil() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertNil(prefs.citySwitchPromptKey)
    }

    func testCitySwitchPromptKeyPersistsAndSurvivesAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCitySwitchPromptKey("poznan→wroclaw")
        XCTAssertEqual(prefs.citySwitchPromptKey, "poznan→wroclaw")

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.citySwitchPromptKey, "poznan→wroclaw")
    }

    func testCitySwitchPromptKeyOverwritesThePreviousPair() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCitySwitchPromptKey("poznan→wroclaw")
        prefs.setCitySwitchPromptKey("wroclaw→warszawa")
        XCTAssertEqual(prefs.citySwitchPromptKey, "wroclaw→warszawa")

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.citySwitchPromptKey, "wroclaw→warszawa")
    }
}
