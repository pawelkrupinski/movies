import XCTest

/// Picking a different day must drop the user back to the TOP of the movie
/// list, not strand them wherever they'd scrolled to on the previous day.
///
/// Driven warm (`KINOWO_UITEST_FIXTURE=1`): the synthetic fixture serves a
/// dozen films ("Film 1"…"Film 12"), all dated today, so "Dziś", "7 dni" and
/// "Wszystkie" all show the same dense, scrollable grid — independent of the
/// live repertoire (empty late at night). The titles are deterministic, so we
/// can read which film sits at the top.
///
/// Before the fix the grid's `ScrollView` kept its scroll offset across a
/// day change, so after scrolling down and switching day the top film was a
/// mid-list one. With `FilmGridView.scrollResetToken` driven by the date, the
/// switch snaps back to row one.
final class DayChangeScrollResetUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1" // warm: dense grid at first paint
        // Pass the first-launch city gate deterministically (so the test stands
        // alone, not relying on a city another test happened to persist):
        // clear any saved city, inject a detected one, then confirm it.
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launch()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 15), "City-confirm screen never showed")
        confirm.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never mounted")
    }

    override func tearDownWithError() throws { app = nil }

    func testSelectingDifferentDayScrollsBackToTop() throws {
        // Fixture lays out "Film 1" first, so the top of the list is Film 1.
        let topLabel = firstFilmCard().label
        XCTAssertTrue(topLabel.contains("Film 1"), "Expected the grid to open on Film 1, got '\(topLabel)'")

        // Scroll deep into the list so the top film is recycled away.
        let grid = app.scrollViews.firstMatch
        XCTAssertTrue(grid.waitForExistence(timeout: 5), "No scroll view")
        for _ in 0..<3 {
            grid.swipeUp(velocity: .fast)
            Thread.sleep(forTimeInterval: 0.4) // let the lazy grid settle
        }
        XCTAssertNotEqual(
            firstFilmCard().label, topLabel,
            "The grid didn't actually scroll — the reset assertion below would be meaningless")

        // Pick a different day. The list must jump back to Film 1 at the top.
        app.buttons[A11y.TopBar.datePillAnytime].tap()

        let backAtTop = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "label CONTAINS %@", "Film 1"),
            object: firstFilmCard())
        XCTAssertEqual(
            XCTWaiter.wait(for: [backAtTop], timeout: 5), .completed,
            "After choosing a different day the grid stayed scrolled (top film was "
            + "'\(firstFilmCard().label)') instead of resetting to Film 1")
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
