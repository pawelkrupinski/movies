import XCTest

/// Covers the full-screen poster viewer on the film detail screen: tapping or
/// long-pressing the header poster presents a full-screen cover, and its close
/// button dismisses it. Mirrors the Android `DetailScreenPosterTest`.
///
/// The cover's existence is probed via its close button (`A11y.FilmDetail
/// .closeButton`) rather than the backdrop — a `Button` is always an
/// accessibility element, whereas a decorative `Color` backdrop may not surface
/// to XCUITest.
final class PosterFullScreenUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        // Inject the detected city so the first-launch gate is deterministic
        // (no CoreLocation dialog / resolve timeout); confirm it below.
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "poznan"
        app.launch()

        // First-launch city gate → confirm the injected city to reach the
        // repertoire. Skipped automatically when a city is already persisted.
        let confirm = app.buttons[A11y.CityGate.confirmButton]
        if confirm.waitForExistence(timeout: 10) { confirm.tap() }

        // "Wszystkie" so the grid has cards regardless of the hour — late in the
        // evening "Dziś" can be empty (see ios uitests-at-night).
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDownWithError() throws { app = nil }

    func testTappingPosterPresentsAndDismissesFullScreenCover() throws {
        let poster = openFirstFilmAndFindPoster()
        let close = app.buttons[A11y.FilmDetail.closeButton]

        XCTAssertFalse(close.exists, "The cover must not be shown before the poster is tapped")
        poster.tap()
        XCTAssertTrue(close.waitForExistence(timeout: 5),
                      "Tapping the poster should present the full-screen cover")

        close.tap()
        XCTAssertTrue(waitForDisappearance(close, timeout: 5),
                      "Tapping close should dismiss the full-screen cover")
    }

    func testLongPressPosterPresentsFullScreenCover() throws {
        let poster = openFirstFilmAndFindPoster()
        let close = app.buttons[A11y.FilmDetail.closeButton]

        poster.press(forDuration: 0.6)
        XCTAssertTrue(close.waitForExistence(timeout: 5),
                      "Long-pressing the poster should present the full-screen cover")
    }

    // MARK: - helpers

    private func openFirstFilmAndFindPoster() -> XCUIElement {
        // Tap the poster region (top of the card), not the centre: the card's
        // rating links and showtime chips keep their own hit areas, so a
        // centre tap can land on one of those instead of the NavigationLink
        // (see FilmGridView). The poster area has no interactive child.
        firstFilmCard().coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()
        // Confirm we actually navigated to the detail screen before hunting for
        // the poster — the title carries its own identifier.
        let title = app.descendants(matching: .any)
            .matching(identifier: A11y.Tuning.detailTitle)
            .firstMatch
        XCTAssertTrue(title.waitForExistence(timeout: 10), "Detail screen never opened")

        let poster = app.buttons[A11y.FilmDetail.poster]
        XCTAssertTrue(poster.waitForExistence(timeout: 5), "Detail poster never appeared")
        return poster
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }

    private func waitForDisappearance(_ element: XCUIElement, timeout: TimeInterval) -> Bool {
        let gone = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "exists == false"), object: element)
        return XCTWaiter.wait(for: [gone], timeout: timeout) == .completed
    }
}
