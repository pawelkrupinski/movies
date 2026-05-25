import XCTest
@testable import KinowoCore

final class FilmDetailParserTests: XCTestCase {

    private func loadMandalorian() throws -> FilmDetail {
        let html = try Fixtures.load("film_mandalorian")
        return FilmDetailParser.parse(html: html, fallbackTitle: "fallback")
    }

    func testParsesTitle() throws {
        let detail = try loadMandalorian()
        XCTAssertEqual(detail.title, "Mandalorian i Grogu")
    }

    func testParsesPosterAndDecodesAmpersands() throws {
        let detail = try loadMandalorian()
        XCTAssertNotNil(detail.posterURL, "expected a poster on the detail page")
        if let url = detail.posterURL?.absoluteString {
            XCTAssertFalse(url.contains("&amp;"), "poster URL still HTML-escaped: \(url)")
        }
    }

    func testFallbackPosterChainPresent() throws {
        let detail = try loadMandalorian()
        XCTAssertFalse(detail.fallbackPosterURLs.isEmpty, "expected at least one fallback poster")
        for url in detail.fallbackPosterURLs {
            XCTAssertFalse(url.absoluteString.contains("&amp;"))
        }
    }

    func testCinemaLinksPresent() throws {
        let detail = try loadMandalorian()
        XCTAssertGreaterThanOrEqual(detail.cinemaLinks.count, 1, "expected at least one cinema link")
        for link in detail.cinemaLinks {
            XCTAssertFalse(link.cinema.isEmpty)
            XCTAssertFalse(link.cinema.contains("↗"), "decorative arrow should be stripped from \(link.cinema)")
        }
    }

    func testParsesOpisAndReżyseriaAndObsada() throws {
        let detail = try loadMandalorian()
        XCTAssertNotNil(detail.synopsis, "expected an Opis block")
        XCTAssertFalse(detail.synopsis?.isEmpty ?? true)
        XCTAssertNotNil(detail.director, "expected a Reżyseria block")
        XCTAssertNotNil(detail.cast, "expected an Obsada block")
    }

    func testDirectorAndCastContainNoHTMLTags() throws {
        let detail = try loadMandalorian()
        for field in [detail.director, detail.cast] {
            guard let value = field else { continue }
            XCTAssertFalse(value.contains("<a "),    "field contains <a> tags: \(value)")
            XCTAssertFalse(value.contains("</a>"),   "field contains </a> tags: \(value)")
            XCTAssertFalse(value.contains("<!--"),   "field contains HTML comments: \(value)")
        }
    }

    func testTrailersAreYoutubeEmbedURLsWhenPresent() throws {
        let detail = try loadMandalorian()
        // Trailers may legitimately be empty — assert only that we don't
        // crash and that any URL we get is a parseable embed link.
        for url in detail.trailerURLs {
            XCTAssertTrue(url.absoluteString.contains("youtube") || url.absoluteString.contains("embed"),
                          "unexpected trailer URL: \(url)")
        }
    }

    func testShowingsParsed() throws {
        let detail = try loadMandalorian()
        XCTAssertFalse(detail.showings.isEmpty, "expected at least one day of showings")
        let totalShowtimes = detail.showings.reduce(0) {
            $0 + $1.cinemas.reduce(0) { $0 + $1.showtimes.count }
        }
        XCTAssertGreaterThan(totalShowtimes, 0)
    }

    func testFallbackTitleUsedWhenFilmTitleMissing() {
        let detail = FilmDetailParser.parse(html: "<p>not a film page</p>", fallbackTitle: "Some Title")
        XCTAssertEqual(detail.title, "Some Title")
    }
}
