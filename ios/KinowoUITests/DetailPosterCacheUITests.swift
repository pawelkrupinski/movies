import XCTest

/// The detail screen must render a poster the app already has on disk,
/// without going back to the origin for it.
///
/// The bug this pins down: the listing card loaded posters through
/// `PosterStore` (disk-first) while the detail header called `AsyncImage`
/// directly, so opening a film re-downloaded a poster the grid had just
/// shown. Any origin that was refusing the app at that moment produced the
/// exact report this test is named after — `Minimaraton: Spider-Man`'s card
/// looked fine on the grid and its detail screen said "Brak plakatu"
/// (2026-09-05, Poznań).
///
/// How it's made observable with no network: `KINOWO_UITEST_SEED_POSTER=1`
/// seeds `PosterStore` with one image under
/// `https://poster.invalid/fixture-poster.png` and hands that URL to the
/// fixture films. `.invalid` is the reserved TLD that never resolves, so a
/// screen reading the cache shows the poster and a screen re-downloading
/// can only show the placeholder. Before the fix this suite fails on the
/// detail screen and passes on the grid; after it, both pass.
final class DetailPosterCacheUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.intoGrid(
            app,
            city: "poznan",
            environment: ["KINOWO_UITEST_SEED_POSTER": "1"]
        )
        XCTAssertTrue(FixtureLaunch.firstFilmCard(app).waitForExistence(timeout: 30),
                      "Grid never appeared")
    }

    override func tearDownWithError() throws { app = nil }

    func testGridCardRendersTheCachedPoster() throws {
        // The control leg: the card was already disk-first, so this passes
        // before and after the fix. It's here so a failure on the detail leg
        // can be read as "the detail screen doesn't use the cache" rather
        // than "the seed never landed".
        XCTAssertTrue(loadedPoster.waitForExistence(timeout: 20),
                      "The grid card never rendered the seeded poster — did the seed hook run?")
    }

    func testDetailHeaderRendersTheCachedPoster() throws {
        openFirstFilm()

        // The header poster is a `Button`, which is one accessibility element:
        // its children's identifiers don't surface, but its LABEL is built
        // from their text. An image contributes none, so an empty label means
        // a poster rendered and "Brak plakatu" means the placeholder did.
        let header = app.buttons[A11y.FilmDetail.poster]
        XCTAssertTrue(header.waitForExistence(timeout: 20), "The detail header never appeared")
        XCTAssertFalse(waitForLabel(header, toContain: Self.missingPosterLabel, timeout: 8),
                       "The detail header showed 'Brak plakatu' for a poster the app has on disk")
    }

    func testFullScreenViewerRendersTheCachedPoster() throws {
        // The positive half of the pair: the full-screen viewer is not wrapped
        // in a Button, so the loaded-poster identifier does surface there —
        // proof the detail screen's loader produced an image rather than
        // sitting on a spinner. Same `detailPoster` the header uses.
        openFirstFilm()
        app.buttons[A11y.FilmDetail.poster].tap()

        XCTAssertTrue(app.buttons[A11y.FilmDetail.closeButton].waitForExistence(timeout: 10),
                      "Tapping the header poster never presented the full-screen viewer")
        XCTAssertTrue(loadedPoster.waitForExistence(timeout: 20),
                      "The full-screen viewer never rendered the seeded poster — it went to the network instead of PosterStore")
    }

    // MARK: - helpers

    private var loadedPoster: XCUIElement {
        app.descendants(matching: .any).matching(identifier: A11y.Poster.loaded).firstMatch
    }

    /// The Polish `poster.missing` string — `FixtureLaunch` pins the app to
    /// Polish, so this is what the placeholder reads.
    private static let missingPosterLabel = "Brak plakatu"

    /// Poll `element.label` rather than asserting once: the poster starts in
    /// the loading phase, so a single immediate read would pass before the
    /// chain has resolved either way.
    private func waitForLabel(_ element: XCUIElement, toContain text: String, timeout: TimeInterval) -> Bool {
        let deadline = Date().addingTimeInterval(timeout)
        while Date() < deadline {
            if element.label.contains(text) { return true }
            usleep(200_000)
        }
        return false
    }

    /// Tap the poster region (top of the card), not the centre: the card's
    /// rating links and showtime chips keep their own hit areas, so a centre
    /// tap can land on one of those instead of the NavigationLink.
    private func openFirstFilm() {
        FixtureLaunch.firstFilmCard(app)
            .coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()
        let title = app.descendants(matching: .any)
            .matching(identifier: A11y.Tuning.detailTitle).firstMatch
        XCTAssertTrue(title.waitForExistence(timeout: 15),
                      "Tapping the card never reached the detail screen")
    }
}
