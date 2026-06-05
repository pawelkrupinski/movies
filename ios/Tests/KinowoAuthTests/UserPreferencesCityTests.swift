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
}
