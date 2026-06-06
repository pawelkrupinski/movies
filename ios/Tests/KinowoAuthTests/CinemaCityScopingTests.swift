import XCTest
@testable import KinowoAuth

/// `disabledCinemas` is one global set shared by every city. After a city
/// switch a cinema deselected elsewhere lingers in it; the count and the
/// "Wszystkie kina" toggle must be derived from the cinemas that belong to the
/// CURRENT city, else the count reads one short and the master toggle wrongly
/// shows "not all selected" on arrival. These pin that scoping down.
final class CinemaCityScopingTests: XCTestCase {

    private var defaults: UserDefaults!
    private static let suite = "CinemaCityScopingTests"

    // Stand-ins. The "foreign" name belongs to another city; the rest are the
    // current city's cinemas.
    private let foreign = "Kino Pod Baranami"
    private let city    = ["Apollo", "Helios", "Multikino"]

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    func testForeignDeselectionDoesNotAffectThisCity() {
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas([foreign])

        XCTAssertEqual(prefs.disabledCinemas(in: city), [],
                       "a cinema from another city must not be counted here")
        XCTAssertTrue(prefs.allCinemasSelected(in: city),
                      "every cinema of this city is actually selected")
        // The pre-fix bar/toggle read the RAW set (`disabledCinemas.isEmpty`),
        // which is false here — that was the bug. The scoped check is the fix.
        XCTAssertFalse(prefs.disabledCinemas.isEmpty)
    }

    func testInCityDeselectionScopesCorrectly() {
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas([foreign, "Apollo"])

        XCTAssertEqual(prefs.disabledCinemas(in: city), ["Apollo"],
                       "only this city's deselection counts — total − 1, not − 2")
        XCTAssertFalse(prefs.allCinemasSelected(in: city))
    }

    func testSelectAllDropsOnlyThisCityPreservingOthers() {
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas([foreign, "Apollo"])

        prefs.setAllCinemas(in: city, selected: true)

        XCTAssertEqual(prefs.disabledCinemas, [foreign],
                       "select-all clears this city but keeps the other city's deselection")
        XCTAssertTrue(prefs.allCinemasSelected(in: city))

        // Persisted: a relaunch reads the same scoped state.
        let reloaded = UserPreferences(store: defaults)
        XCTAssertEqual(reloaded.disabledCinemas, [foreign])
    }

    func testDeselectAllAddsThisCityOnTopOfOthers() {
        let prefs = UserPreferences(store: defaults)
        prefs.setDisabledCinemas([foreign])

        prefs.setAllCinemas(in: city, selected: false)

        XCTAssertEqual(prefs.disabledCinemas, Set([foreign] + city),
                       "deselect-all adds every cinema of this city, keeping the foreign one")
        XCTAssertFalse(prefs.allCinemasSelected(in: city))
    }
}
