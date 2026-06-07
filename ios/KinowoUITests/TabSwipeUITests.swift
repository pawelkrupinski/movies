import XCTest

/// The cinema tab was removed; the horizontal swipe now changes the selected
/// day (date pill) instead of paging to /kina. These tests drive that gesture
/// and assert on the durable post-swipe state — which date pill is selected.
final class DaySwipeUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launch()

        // Start on "Wszystkie" so films are present regardless of time of day —
        // late in the evening today's showings have all passed, leaving the
        // default "Dziś" grid empty (and an empty grid has no area to swipe).
        let allDates = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(allDates.waitForExistence(timeout: 15), "Top bar never appeared")
        allDates.tap()

        XCTAssertTrue(firstFilmCard(app).waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDownWithError() throws {
        app = nil
    }

    /// Swipe left advances to the next day in the pill order. Starting on
    /// "Wszystkie" (the last pill), a left swipe wraps around to "Dziś" (first).
    func testSwipeLeftAdvancesDayWithWrap() throws {
        firstFilmCard(app).swipeLeft()

        let today = app.buttons[A11y.TopBar.datePillToday]
        let selectedToday = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: today)
        XCTAssertEqual(XCTWaiter.wait(for: [selectedToday], timeout: 5), .completed,
                       "Swipe-left from Wszystkie should wrap to Dziś")
    }

    /// Swipe right steps to the previous day. Starting on "Wszystkie" (last),
    /// a right swipe lands on the day immediately before it ("7 dni").
    func testSwipeRightStepsToPreviousDay() throws {
        firstFilmCard(app).swipeRight()

        let week = app.buttons[A11y.TopBar.datePillWeek]
        let selectedWeek = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: week)
        XCTAssertEqual(XCTWaiter.wait(for: [selectedWeek], timeout: 5), .completed,
                       "Swipe-right from Wszystkie should step back to 7 dni")
    }

    private func firstFilmCard(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
