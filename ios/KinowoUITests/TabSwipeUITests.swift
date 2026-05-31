import XCTest

final class TabSwipeUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launch()

        // Wait for the grid before any swipe — swiping a still-loading
        // TabView produces nothing.
        let card = firstFilmCard(app)
        XCTAssertTrue(card.waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDownWithError() throws {
        app = nil
    }

    func testSwipeToKinaShowsCinemaSections() throws {
        // The Filmy / Kina overlay labels flash for ~0.7s, which is too
        // transient to assert against reliably under XCUITest's polling
        // cadence. Assert on the durable post-swipe state instead: a
        // cinema-section-header element appearing on the Kina tab.
        app.swipeLeft()

        let header = cinemaSectionHeader(app)
        XCTAssertTrue(header.waitForExistence(timeout: 10),
                      "Expected at least one cinema section header on /kina")
    }

    func testSwipeBackToFilmyShowsGrid() throws {
        app.swipeLeft()
        XCTAssertTrue(cinemaSectionHeader(app).waitForExistence(timeout: 10))

        app.swipeRight()

        XCTAssertTrue(firstFilmCard(app).waitForExistence(timeout: 10),
                      "Expected film grid back after swiping right")
    }

    func testSelectingCinemaFromPillsPinsThatCinema() throws {
        app.swipeLeft()

        let headers = app.descendants(matching: .any)
            .matching(identifier: A11y.CinemaPage.sectionHeader)
        XCTAssertTrue(headers.firstMatch.waitForExistence(timeout: 10),
                      "No cinema sections on /kina")
        let originalCount = headers.count
        try XCTSkipIf(originalCount < 2, "Need ≥2 cinema sections to test pinning")

        // The first section header names a cinema that has films right now;
        // its pill carries the same label. Tapping the pill (its hit area is
        // the rectangle around the capsule) pins that cinema and hides every
        // other section.
        let cinemaName = headers.firstMatch.label
        let pill = app.buttons[cinemaName]
        XCTAssertTrue(pill.waitForExistence(timeout: 5), "No cinema pill for \(cinemaName)")

        // The pill row scrolls horizontally; nudge it until the pill is
        // on-screen (swiping the row, not the page, so the tab doesn't flip).
        let pillRow = app.scrollViews[A11y.CinemaPage.pillRow]
        var tries = 0
        while !pill.isHittable && pillRow.exists && tries < 8 {
            pillRow.swipeLeft()
            tries += 1
        }
        pill.tap()

        let pinnedToOne = XCTNSPredicateExpectation(
            predicate: NSPredicate { _, _ in headers.count == 1 }, object: nil)
        XCTAssertEqual(XCTWaiter.wait(for: [pinnedToOne], timeout: 5), .completed,
                       "Expected exactly one cinema section after pinning \(cinemaName)")
    }

    private func firstFilmCard(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }

    private func cinemaSectionHeader(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.CinemaPage.sectionHeader)
            .firstMatch
    }
}
