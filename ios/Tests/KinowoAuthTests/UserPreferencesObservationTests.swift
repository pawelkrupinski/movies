import XCTest
import Observation
import KinowoTestSupport
@testable import KinowoAuth

/// `UserPreferences` is `@Observable`: a reader (a view's body) is told about a
/// change only to a property it read. As an `ObservableObject` every
/// `@Published` write re-rendered every view holding the preferences — a
/// swipe-hint stamp redrew each film card.
@MainActor
final class UserPreferencesObservationTests: XCTestCase {

    private var defaults: UserDefaults!

    private static let suite = "UserPreferencesObservationTests"

    override func setUp() async throws {
        try await super.setUp()
        defaults = .scratch(suiteName: Self.suite)
    }

    override func tearDown() async throws {
        defaults.discardScratch(suiteName: Self.suite)
        try await super.tearDown()
    }

    /// How many times a reader of `read` is told about a change while `edit` runs.
    private func changesHeard(reading read: () -> Void, during edit: () -> Void) -> Int {
        let heard = LockedValue(0)
        withObservationTracking(read) { heard.withLock { $0 += 1 } }
        edit()
        return heard.withLock { $0 }
    }

    func testAReaderOfTheCityHearsNoOtherPreference() {
        let prefs = UserPreferences(store: defaults)
        let heard = changesHeard(reading: { _ = prefs.selectedCity }) {
            prefs.markSwiped()
            prefs.markSwipeHintShown("2026-10-24")
            prefs.setDisabledCinemas(["Kino Muza"])
            prefs.hide("Film")
            prefs.setLanguage("de")
            prefs.markAreaPickerSeen("warszawa")
        }
        XCTAssertEqual(heard, 0)
    }

    func testAReaderOfTheCityHearsTheCityChange() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertEqual(changesHeard(reading: { _ = prefs.selectedCity }) { prefs.setCity("poznan") }, 1)
    }

    func testAReaderOfTheHiddenFilmsHearsAHideAndNotALanguagePick() {
        let prefs = UserPreferences(store: defaults)
        XCTAssertEqual(changesHeard(reading: { _ = prefs.hiddenFilms }) { prefs.setLanguage("de") }, 0)
        XCTAssertEqual(changesHeard(reading: { _ = prefs.hiddenFilms }) { prefs.hide("Film") }, 1)
    }

    /// The edits the account must hear about reach the listener synchronously,
    /// inside the call — a language left as it was is not an edit.
    func testTheEditListenerHearsEachEditInsideTheCall() {
        let prefs = UserPreferences(store: defaults)
        let listener = RecordingEditListener()
        prefs.editListener = listener
        prefs.hide("A")
        prefs.unhide("A")
        prefs.unhideAll()
        prefs.setLanguage("de")
        prefs.setLanguage("de")
        XCTAssertEqual(listener.edits, [".hidden(A)", ".unhidden(A)", ".clearedAll", "language de"])
    }
}

@MainActor
private final class RecordingEditListener: PreferencesEditListener {
    var edits: [String] = []
    func hiddenFilmsEdited(_ change: HiddenFilmsChange) {
        switch change {
        case .hidden(let title):   edits.append(".hidden(\(title))")
        case .unhidden(let title): edits.append(".unhidden(\(title))")
        case .clearedAll:          edits.append(".clearedAll")
        }
    }
    func languageChanged(to language: String) { edits.append("language \(language)") }
}
