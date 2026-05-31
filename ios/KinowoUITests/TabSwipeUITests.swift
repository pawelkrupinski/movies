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

    func testPinningCinemaHidesSectionHeaders() throws {
        app.swipeLeft()

        let headers = app.descendants(matching: .any)
            .matching(identifier: A11y.CinemaPage.sectionHeader)
        XCTAssertTrue(headers.firstMatch.waitForExistence(timeout: 10),
                      "No cinema sections on /kina")
        try XCTSkipIf(headers.count < 2, "Need ≥2 cinema sections to test pinning")

        // Pills in row order: [0] = "Wszystkie", [1] = first cinema. Both are
        // on-screen at the start, and tapping a pill doesn't scroll the row,
        // so no horizontal scrolling is needed.
        let pills = app.buttons.matching(identifier: A11y.CinemaPage.pill)
        let firstCinema = pills.element(boundBy: 1)
        XCTAssertTrue(firstCinema.waitForExistence(timeout: 5), "No cinema pill")

        // Pin a concrete cinema → its pill already names it, so every
        // per-section header is dropped.
        firstCinema.tap()
        let headersHidden = XCTNSPredicateExpectation(
            predicate: NSPredicate { _, _ in headers.count == 0 }, object: nil)
        XCTAssertEqual(XCTWaiter.wait(for: [headersHidden], timeout: 5), .completed,
                       "Section headers should be hidden when a cinema is pinned")

        // Back to "Wszystkie" → the per-section headers return.
        pills.element(boundBy: 0).tap()
        let headersBack = XCTNSPredicateExpectation(
            predicate: NSPredicate { _, _ in headers.count >= 1 }, object: nil)
        XCTAssertEqual(XCTWaiter.wait(for: [headersBack], timeout: 5), .completed,
                       "Section headers should return for Wszystkie")
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
