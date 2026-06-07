import XCTest

/// The film card's long-press share menu (Udostępnij / Skopiuj link) lives on
/// the POSTER, not the whole card — so it no longer swallows the showtime
/// pills' own long-press (the room tooltip). Covers both halves: long-pressing
/// the poster opens the menu; long-pressing the lower card (where the showtimes
/// live) does NOT. The previous wiring put `.contextMenu` on the whole card, so
/// a hold anywhere — including on a pill — popped the share menu over the room.
///
/// Mirrors the Android `FilmCardShareMenuTest`.
final class CardShareMenuUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "poznan"
        app.launch()

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

    func testLongPressPosterOpensShareMenu() throws {
        // The poster is the top of the card (dy 0.18), the same spot
        // `PosterFullScreenUITests` uses to hit the poster region.
        firstFilmCard()
            .coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18))
            .press(forDuration: 0.7)

        XCTAssertTrue(shareMenuItem().waitForExistence(timeout: 5),
                      "Long-pressing the poster should open the share menu")
    }

    func testLongPressShowingsDoesNotOpenShareMenu() throws {
        // The lower card (dy 0.78) holds the ratings + showtimes, well below the
        // poster. A long-press here must NOT raise the card's share menu — that
        // region is for the pills' own room tooltip / booking links.
        firstFilmCard()
            .coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.78))
            .press(forDuration: 0.7)

        XCTAssertFalse(shareMenuItem().waitForExistence(timeout: 2),
                       "Long-pressing the showtimes must not open the card share menu")
    }

    // MARK: - helpers

    /// The "Udostępnij" entry of the card's `.contextMenu`, surfaced as a button
    /// while the menu is open.
    private func shareMenuItem() -> XCUIElement {
        app.buttons["Udostępnij"]
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
