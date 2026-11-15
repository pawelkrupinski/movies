import XCTest

/// The film card's long-press share menu (Udostępnij / Skopiuj link) lives on
/// the POSTER, not the whole card — so it no longer swallows the showtime
/// pills' own long-press (the room tooltip). Covers both halves: long-pressing
/// the poster opens the menu; long-pressing the lower card (where the showtimes
/// live) does NOT. The previous wiring put `.contextMenu` on the whole card, so
/// a hold anywhere — including on a pill — popped the share menu over the room.
///
/// Mirrors the Android `FilmCardShareMenuTest`.
///
/// The hold is `holdSeconds`, well past the context menu's ~0.5 s threshold:
/// the long-press recognizer fails when the touch-up is handled before its
/// timer fires, so a main-thread stall at touch-down longer than the hold's
/// margin over 0.5 s turns the hold into a TAP, which opens the film detail.
/// A 0.7 s hold left 0.2 s: the first touch on a just-booted Simulator stalled
/// longer (Order independence #74, the class run alone and first after the
/// boot: the grid stayed ~6 s after the press, then the detail opened).
@MainActor
final class CardShareMenuUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app, environment: ["KINOWO_UITEST_SEED_POSTER": "1"])

        // "Wszystkie" so the grid has cards regardless of the hour — late in the
        // evening "Dziś" can be empty (see ios uitests-at-night).
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDown() async throws { app = nil }

    func testLongPressPosterOpensShareMenu() throws {
        // The poster is the top of the card (dy 0.18), the same spot
        // `PosterFullScreenUITests` uses to hit the poster region.
        firstFilmCard()
            .coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18))
            .press(forDuration: Self.holdSeconds)

        XCTAssertTrue(shareMenuItem().waitForExistence(timeout: 5),
                      "Long-pressing the poster should open the share menu")
    }

    func testLongPressShowingsDoesNotOpenShareMenu() throws {
        // The lower card (dy 0.78) holds the ratings + showtimes, well below the
        // poster. A long-press here must NOT raise the card's share menu — that
        // region is for the pills' own room tooltip / booking links.
        firstFilmCard()
            .coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.78))
            .press(forDuration: Self.holdSeconds)

        XCTAssertFalse(shareMenuItem().waitForExistence(timeout: 2),
                       "Long-pressing the showtimes must not open the card share menu")
    }

    // MARK: - helpers

    /// See the class doc: a hold that survives a 1.5 s stall at touch-down.
    private static let holdSeconds: TimeInterval = 2.0

    /// The share entry of the card's `.contextMenu`, surfaced as a button while
    /// the menu is open. Addressed by identifier, not label — the label is
    /// translated, so a literal would only match a Polish-locale simulator.
    private func shareMenuItem() -> XCUIElement {
        app.buttons[A11y.FilmCard.share]
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}
