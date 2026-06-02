import XCTest
@testable import KinowoCore

/// `FilmShareLink.url` must mirror the server's `controllers.FilmHref`
/// encoding exactly, so a link shared from the app is byte-identical to one
/// copied off the website: spaces as `%20` (never the form `+`), and every
/// reserved character or Polish diacritic percent-encoded.
final class FilmShareLinkTests: XCTestCase {

    func testPlainAsciiTitleIsLeftIntact() {
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Oppenheimer").absoluteString,
            "https://kinowo.fly.dev/film?title=Oppenheimer"
        )
    }

    func testSpacesAndAmpersandEncode() {
        // Space → %20 (not `+`), `&` → %26.
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Lilo & Stitch").absoluteString,
            "https://kinowo.fly.dev/film?title=Lilo%20%26%20Stitch"
        )
    }

    func testColonAndPolishDiacriticsEncode() {
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Diuna: Część druga").absoluteString,
            "https://kinowo.fly.dev/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga"
        )
    }

    func testNeverEmitsFormPlusForSpace() {
        XCTAssertFalse(FilmShareLink.url(forTitle: "Past Lives").absoluteString.contains("+"))
    }
}
