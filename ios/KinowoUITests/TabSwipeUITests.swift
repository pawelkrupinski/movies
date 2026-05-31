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

    func testSelectingCinemaFromDropdownPinsThatCinema() throws {
        app.swipeLeft()

        let headers = app.descendants(matching: .any)
            .matching(identifier: A11y.CinemaPage.sectionHeader)
        XCTAssertTrue(headers.firstMatch.waitForExistence(timeout: 10),
                      "No cinema sections on /kina")
        let originalCount = headers.count
        try XCTSkipIf(originalCount < 2, "Need ≥2 cinema sections to test pinning")

        // The first section header names a cinema that has films right now;
        // the dropdown carries an option with the same label. Picking it pins
        // that cinema and hides every other section.
        let cinemaName = headers.firstMatch.label

        let dropdown = app.buttons[A11y.CinemaPage.dropdown]
        XCTAssertTrue(dropdown.waitForExistence(timeout: 5), "No cinema dropdown")
        dropdown.tap()

        let option = app.buttons[cinemaName]
        XCTAssertTrue(option.waitForExistence(timeout: 5),
                      "No dropdown option for \(cinemaName)")
        option.tap()

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
