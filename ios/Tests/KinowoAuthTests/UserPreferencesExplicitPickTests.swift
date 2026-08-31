import XCTest
@testable import KinowoAuth

/// The flag that tells the city gate to present a list rather than offer a
/// located city. Picking a country is itself a choice, and the gate owes it
/// that country's cities; the flag has to survive a relaunch and has to stop
/// applying the moment a city is actually chosen.
final class UserPreferencesExplicitPickTests: XCTestCase {

    private var defaults: UserDefaults!

    private static let suite = "UserPreferencesExplicitPickTests"

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    func testNothingToHonourBeforeACountryIsPicked() {
        XCTAssertFalse(UserPreferences(store: defaults).awaitingExplicitCityPick)
    }

    func testPickingACountryAsksTheGateForAnExplicitPick() {
        let prefs = UserPreferences(store: defaults)
        let other = Country.all.first { $0 != prefs.selectedCountry }!

        prefs.setCountry(other)
        XCTAssertTrue(prefs.awaitingExplicitCityPick)

        // Survives a relaunch: the gate may only read this after the app has
        // re-localized and rebuilt its view tree around the new country.
        XCTAssertTrue(UserPreferences(store: defaults).awaitingExplicitCityPick)
    }

    func testChoosingACitySatisfiesTheGate() {
        let prefs = UserPreferences(store: defaults)
        let other = Country.all.first { $0 != prefs.selectedCountry }!
        prefs.setCountry(other)

        prefs.setCity("berlin")
        XCTAssertFalse(prefs.awaitingExplicitCityPick)
        XCTAssertFalse(UserPreferences(store: defaults).awaitingExplicitCityPick)
    }

    /// Re-selecting the country already in force is not a new choice, so it
    /// must not re-gate someone who already has a city.
    func testReselectingTheSameCountryChangesNothing() {
        let prefs = UserPreferences(store: defaults)
        prefs.setCountry(prefs.selectedCountry)
        XCTAssertFalse(prefs.awaitingExplicitCityPick)
    }
}
