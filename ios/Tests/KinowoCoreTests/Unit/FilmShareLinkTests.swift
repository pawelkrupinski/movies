import XCTest
@testable import KinowoCore

/// `FilmShareLink.url` must mirror the server's `controllers.FilmHref`
/// encoding exactly, so a link shared from the app is byte-identical to one
/// copied off the website: the city-scoped path `/<city>/film?title=…`, spaces
/// as `%20` (never the form `+`), and every reserved character or Polish
/// diacritic percent-encoded.
final class FilmShareLinkTests: XCTestCase {

    func testPlainAsciiTitleIsLeftIntact() {
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Oppenheimer", citySlug: "poznan").absoluteString,
            "https://kinowo.fly.dev/poznan/film?title=Oppenheimer"
        )
    }

    func testCarriesTheCitySlugInThePath() {
        // The city the sharer is browsing scopes the link — a city-less
        // `/film?title=…` has no server route and 404s.
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Oppenheimer", citySlug: "bielsko-biala").absoluteString,
            "https://kinowo.fly.dev/bielsko-biala/film?title=Oppenheimer"
        )
    }

    func testSpacesAndAmpersandEncode() {
        // Space → %20 (not `+`), `&` → %26.
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Lilo & Stitch", citySlug: "warszawa").absoluteString,
            "https://kinowo.fly.dev/warszawa/film?title=Lilo%20%26%20Stitch"
        )
    }

    func testColonAndPolishDiacriticsEncode() {
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Diuna: Część druga", citySlug: "wroclaw").absoluteString,
            "https://kinowo.fly.dev/wroclaw/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga"
        )
    }

    func testNeverEmitsFormPlusForSpace() {
        XCTAssertFalse(FilmShareLink.url(forTitle: "Past Lives", citySlug: "poznan").absoluteString.contains("+"))
    }
}
