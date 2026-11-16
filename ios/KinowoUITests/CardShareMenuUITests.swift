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
/// Each press is made at a `ScreenPoint` fixed before the gesture, never at a
/// coordinate anchored to the card: XCUITest re-resolves an element-anchored
/// coordinate on the app's main thread while it plays the touch, and on CI that
/// search over the grid blocked the main thread long enough to turn the hold
/// into a tap (Order independence #74; the detail opened, no menu — see
/// `ScreenPoint`).
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
        XCTAssertTrue(FixtureLaunch.firstFilmCard(app).waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDown() async throws { app = nil }

    func testLongPressPosterOpensShareMenu() throws {
        // The poster is the top of the card (dy 0.18), the same spot
        // `PosterFullScreenUITests` uses to hit the poster region.
        ScreenPoint.of(FixtureLaunch.firstFilmCard(app), normalizedOffset: CGVector(dx: 0.5, dy: 0.18), in: app)
            .press(forDuration: Self.holdSeconds)

        XCTAssertTrue(shareMenuItem().waitForExistence(timeout: 5),
                      "Long-pressing the poster should open the share menu")
    }

    func testLongPressShowingsDoesNotOpenShareMenu() throws {
        // The lower card (dy 0.78) holds the ratings + showtimes, well below the
        // poster. A long-press here must NOT raise the card's share menu — that
        // region is for the pills' own room tooltip / booking links.
        ScreenPoint.of(FixtureLaunch.firstFilmCard(app), normalizedOffset: CGVector(dx: 0.5, dy: 0.78), in: app)
            .press(forDuration: Self.holdSeconds)

        XCTAssertFalse(shareMenuItem().waitForExistence(timeout: 2),
                       "Long-pressing the showtimes must not open the card share menu")
    }

    // MARK: - helpers

    /// A user's hold: past the context menu's ~0.5 s threshold with 0.3 s to
    /// spare, which the application-anchored point leaves intact (its re-resolve
    /// during the gesture costs ~0.05 s of main thread on CI, where the card
    /// query cost up to 1.5 s and needed a 2 s hold to outlast).
    private static let holdSeconds: TimeInterval = 0.8

    /// The share entry of the card's `.contextMenu`, surfaced as a button while
    /// the menu is open. Addressed by identifier, not label — the label is
    /// translated, so a literal would only match a Polish-locale simulator.
    private func shareMenuItem() -> XCUIElement {
        app.buttons[A11y.FilmCard.share]
    }
}
