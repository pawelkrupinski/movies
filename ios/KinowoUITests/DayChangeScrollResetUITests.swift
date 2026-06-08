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
/// The day pages live in a native paged `TabView`. Each page's
/// `FilmGridView.scrollResetToken` is keyed on the selected day, so switching
/// day snaps the grid back to row one. We read the TOP **on-screen** card
/// (never `firstMatch` — the pager keeps the neighbour day's page mounted
/// off-screen, so `firstMatch` can grab a card that isn't visible).
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
        XCTAssertTrue(anyCard().waitForExistence(timeout: 30), "Grid never mounted")
    }

    override func tearDownWithError() throws { app = nil }

    func testSelectingDifferentDayScrollsBackToTop() throws {
        // Fixture lays out "Film 1" first, so the top of the list is Film 1.
        let topLabel = try XCTUnwrap(topOnScreenCardLabel(), "No on-screen card at launch")
        XCTAssertTrue(topLabel.contains("Film 1"), "Expected the grid to open on Film 1, got '\(topLabel)'")

        // Scroll deep into the list so the top film is recycled away.
        let grid = app.scrollViews.firstMatch
        XCTAssertTrue(grid.waitForExistence(timeout: 5), "No scroll view")
        for _ in 0..<3 {
            grid.swipeUp(velocity: .fast)
            Thread.sleep(forTimeInterval: 0.4) // let the lazy grid settle
        }
        XCTAssertNotEqual(
            topOnScreenCardLabel(), topLabel,
            "The grid didn't actually scroll — the reset assertion below would be meaningless")

        // Pick a different day. The list must jump back to Film 1 at the top.
        app.buttons[A11y.TopBar.datePillAnytime].tap()

        let backAtTop = expectation(description: "top on-screen card is Film 1 again")
        let poll = Timer.scheduledTimer(withTimeInterval: 0.25, repeats: true) { t in
            if self.topOnScreenCardLabel()?.contains("Film 1") == true {
                t.invalidate()
                backAtTop.fulfill()
            }
        }
        defer { poll.invalidate() }
        XCTAssertEqual(
            XCTWaiter.wait(for: [backAtTop], timeout: 5), .completed,
            "After choosing a different day the grid stayed scrolled (top on-screen "
            + "film was '\(topOnScreenCardLabel() ?? "nil")') instead of resetting to Film 1")
    }

    // MARK: - Helpers

    private func anyCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }

    /// The label of the topmost card that is actually ON SCREEN — smallest `minY`
    /// among cards whose frame intersects the window and whose origin sits within
    /// the screen's horizontal bounds (so the off-screen neighbour page's cards,
    /// parked at minX ≈ ±screenWidth, are ignored).
    private func topOnScreenCardLabel() -> String? {
        let screen = app.windows.firstMatch.frame
        let cards = app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .allElementsBoundByIndex
        let onScreen = cards.filter { c in
            let f = c.frame
            return f.width > 1 && f.height > 1
                && f.minX >= screen.minX - 1 && f.minX < screen.maxX
                && screen.intersects(f)
        }
        return onScreen.min(by: { $0.frame.minY < $1.frame.minY })?.label
    }
}
