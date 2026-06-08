import XCTest

/// The cinema tab was removed; the horizontal swipe now pages between the four
/// day presets (Dziś / Jutro / 7 dni / Wszystkie) of a native paged `TabView`.
/// These tests drive that gesture and assert on the durable post-swipe state —
/// which date pill is selected.
///
/// Native paging does NOT wrap: swiping past the last day stays on the last day
/// (the old hand-rolled carousel wrapped Wszystkie → Dziś — that's gone now,
/// guarded by `testSwipeLeftAtLastDayDoesNotWrap`).
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

    /// Swipe right pages to the previous day. Starting on "Wszystkie" (last
    /// preset), a right swipe lands on the day immediately before it ("7 dni").
    func testSwipeRightStepsToPreviousDay() throws {
        swipeDay(left: false)

        let week = app.buttons[A11y.TopBar.datePillWeek]
        let selectedWeek = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: week)
        XCTAssertEqual(XCTWaiter.wait(for: [selectedWeek], timeout: 5), .completed,
                       "Swipe-right from Wszystkie should step back to 7 dni")
    }

    /// Swipe left pages to the next day. From "Wszystkie" we first step back to
    /// "7 dni" (swipe right), then a left swipe pages forward again to
    /// "Wszystkie" — proving a left swipe advances the day.
    func testSwipeLeftAdvancesToNextDay() throws {
        swipeDay(left: false)                       // Wszystkie → 7 dni
        let week = app.buttons[A11y.TopBar.datePillWeek]
        XCTAssertTrue(week.waitForSelected(timeout: 5), "Setup: should be on 7 dni")

        swipeDay(left: true)                        // 7 dni → Wszystkie

        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForSelected(timeout: 5),
                      "Swipe-left from 7 dni should advance to Wszystkie")
    }

    /// Native paging doesn't wrap: a left swipe on the LAST day ("Wszystkie")
    /// stays there — it must NOT cycle round to the first day ("Dziś"). This is
    /// the behavioural change from the old wrap-around carousel.
    func testSwipeLeftAtLastDayDoesNotWrap() throws {
        swipeDay(left: true)                        // Wszystkie + swipe forward
        Thread.sleep(forTimeInterval: 1.0)          // let any page settle

        let today = app.buttons[A11y.TopBar.datePillToday]
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertFalse(today.isSelected,
                       "Swipe-left off the last day wrapped to Dziś — native paging must not wrap")
        XCTAssertTrue(anytime.isSelected,
                      "Swipe-left off the last day should stay on Wszystkie")
    }

    // MARK: - Helpers

    /// Drag horizontally across the middle of the screen to page the TabView.
    /// `left == true` swipes the next day in (forward); `false` the previous
    /// (backward). Swiping the window — not a card via `firstMatch` — avoids
    /// grabbing an off-screen neighbour page's card (UIPageViewController keeps
    /// adjacent pages mounted), which has an empty hit frame.
    private func swipeDay(left: Bool) {
        let win = app.windows.firstMatch
        let from = win.coordinate(withNormalizedOffset: CGVector(dx: left ? 0.85 : 0.15, dy: 0.5))
        let to = win.coordinate(withNormalizedOffset: CGVector(dx: left ? 0.15 : 0.85, dy: 0.5))
        from.press(forDuration: 0.05, thenDragTo: to)
    }

    private func firstFilmCard(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

private extension XCUIElement {
    /// Wait until this element reports `isSelected == true`, up to `timeout`.
    func waitForSelected(timeout: TimeInterval) -> Bool {
        let selected = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: self)
        return XCTWaiter.wait(for: [selected], timeout: timeout) == .completed
    }
}
