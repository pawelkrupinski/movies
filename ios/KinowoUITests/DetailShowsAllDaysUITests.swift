import XCTest

/// Regression guard: opening a film from a single-day grid page (e.g. "Dziś")
/// must show that film's COMPLETE schedule on the detail screen — every day it
/// plays — not just the day-page it was tapped from.
///
/// The grids are day-paged: each page renders a `filteredFor` copy whose
/// showings are pruned to that day. The detail screen used to render exactly
/// that pruned copy, so a film tapped from "Dziś" showed only today's seanse.
/// ContentView's `navigationDestination` now re-resolves the full, all-days
/// film by title from the unfiltered `store.films`, so the detail shows the
/// whole schedule.
///
/// Driven warm (`KINOWO_UITEST_FIXTURE`) with a today+tomorrow fixture: every
/// fixture film plays "Dziś" AND "Jutro", so tapping from the Dziś page and
/// then seeing the "JUTRO" day block is the proof the prune was undone.
final class DetailShowsAllDaysUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launch()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 15), "City-confirm screen never showed")
        confirm.tap()
    }

    override func tearDownWithError() throws { app = nil }

    func testOpeningFilmFromTodayPageShowsAllItsDays() throws {
        // Pin the listing to the single "Dziś" page so the grid hands the detail
        // screen a today-only copy.
        let today = app.buttons[A11y.TopBar.datePillToday]
        XCTAssertTrue(today.waitForExistence(timeout: 30), "Top bar never appeared")
        today.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never appeared")

        // Tap the poster region (top of the card) — the rating links and showtime
        // chips keep their own hit areas, so a centre tap can miss the
        // NavigationLink (see FilmGridView / PosterFullScreenUITests).
        firstFilmCard().coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()

        // Confirm we actually navigated to the detail screen.
        let detailTitle = app.descendants(matching: .any)
            .matching(identifier: A11y.Tuning.detailTitle)
            .firstMatch
        XCTAssertTrue(detailTitle.waitForExistence(timeout: 10), "Detail screen never opened")

        // The day blocks render their label uppercased (`day.label.uppercased()`).
        // The fixture film plays today AND tomorrow; the detail must show BOTH.
        XCTAssertTrue(app.staticTexts["DZIŚ"].waitForExistence(timeout: 5),
                      "Detail is missing today's seanse block")
        XCTAssertTrue(app.staticTexts["JUTRO"].waitForExistence(timeout: 5),
                      "Detail opened from the Dziś page shows only today — the full, "
                      + "all-days schedule was not resolved (the day-filter leaked into detail)")
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
