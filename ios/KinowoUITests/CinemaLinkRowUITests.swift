import XCTest

/// Regression guard for the detail screen's external-links row.
///
/// Cinemas routinely share one film URL: a PL chain points all its venues at
/// the same page, and the UK aggregator hands ALL 72 London cinemas one
/// flicks.co.uk page. The row used to render a pill per cinema keyed by that
/// URL, so every pill carried the same SwiftUI identity and the screen filled
/// with copies of the alphabetically-first cinema ("Act One Cinema Acton ↗",
/// 72 times over).
///
/// The row now shows one pill per distinct URL, named by what the cinemas
/// behind it share; a cinema with its own URL keeps its own name. Driven by
/// the `KINOWO_UITEST_SHARED_CINEMA_URL` fixture, whose day carries Multikino
/// Alfa + Multikino Beta behind one URL and Kino Solo behind its own.
final class CinemaLinkRowUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app, environment: ["KINOWO_UITEST_SHARED_CINEMA_URL": "1"])
    }

    override func tearDownWithError() throws { app = nil }

    func testSharedCinemaURLCollapsesToOneChainLink() throws {
        let card = FixtureLaunch.firstFilmCard(app)
        XCTAssertTrue(card.waitForExistence(timeout: 30), "Grid never appeared")
        // Tap the poster region: the rating links and showtime chips own their
        // own hit areas, so a centre tap can miss the NavigationLink.
        card.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()

        let links = app.descendants(matching: .any)
            .matching(identifier: A11y.FilmDetail.cinemaLink)
        XCTAssertTrue(links.firstMatch.waitForExistence(timeout: 15),
                      "Detail screen never showed its cinema-link row")

        let labels = links.allElementsBoundByIndex.map(\.label)
        XCTAssertEqual(labels, ["Kino Solo ↗", "Multikino ↗"],
                       "Two venues share one URL, so the row must show ONE pill for that "
                       + "chain plus the cinema that has its own link — not one pill per "
                       + "cinema, and not the same pill repeated")
    }
}
