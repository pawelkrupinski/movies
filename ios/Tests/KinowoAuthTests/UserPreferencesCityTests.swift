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

    // MARK: - Selected cinema pill

    func testSelectedCinemaStartsNil() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertNil(prefs.selectedCinema)
    }

    func testSetSelectedCinemaPersistsAndSurvivesAReload() {
        let prefs = UserPreferences(store: defaults)
        prefs.setSelectedCinema("Kino Muza")
        XCTAssertEqual(prefs.selectedCinema, "Kino Muza")

        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.selectedCinema, "Kino Muza")
    }

    func testSetSelectedCinemaNilClearsThePersistedValue() {
        let prefs = UserPreferences(store: defaults)
        prefs.setSelectedCinema("Kino Muza")
        prefs.setSelectedCinema(nil)
        XCTAssertNil(prefs.selectedCinema)

        let reloaded = UserPreferences(store: defaults)
        XCTAssertNil(reloaded.selectedCinema)
    }

    func testSwitchingCityClearsTheSelectedCinema() {
        // A cinema pill belongs to one city's cinema list; leaving that city
        // must drop it so the new city can't open on a stale, guarded-away
        // (empty) selection.
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        prefs.setSelectedCinema("Kino Muza")
        prefs.setCity("warszawa")
        XCTAssertNil(prefs.selectedCinema)

        let reloaded = UserPreferences(store: defaults)
        XCTAssertNil(reloaded.selectedCinema)
    }

    func testSettingTheSameCityKeepsTheSelectedCinema() {
        // `setCity` early-returns when the slug is unchanged, so a redundant
        // set must not wipe the current pick.
        let prefs = UserPreferences(store: defaults)
        prefs.setCity("poznan")
        prefs.setSelectedCinema("Kino Muza")
        prefs.setCity("poznan")
        XCTAssertEqual(prefs.selectedCinema, "Kino Muza")
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
