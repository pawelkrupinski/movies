import XCTest
@testable import KinowoCore

// Exercises `FilmDetailParser` against a live render of
// `/film?title=…`. Supersedes the static `FilmDetailFixtureShapeTests`
// (which was pinned to a captured `film_mandalorian.html`).
//
// The test picks the first film from the home render that has at
// least one showing and queries detail by title. That keeps the spec
// resilient to the fixture corpus changing — we don't hardcode a
// specific film name.
final class LocalServerFilmDetailTests: LocalServerTestCase {

    /// Picks the first film with showings from the home render and
    /// returns `(title, parsed detail)`.
    private func firstFilmDetail() throws -> (String, FilmDetail) {
        let homeHTML = try fetchHTML(path: "/")
        let films = HTMLParser.parse(html: homeHTML)
        guard let pick = films.first(where: { !$0.showings.isEmpty }) else {
            XCTFail("home render has no films with showings — can't drive the detail test")
            throw URLError(.cannotParseResponse)
        }
        let encoded = pick.title.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed) ?? pick.title
        let detailHTML = try fetchHTML(path: "/film?title=\(encoded)")
        let detail = FilmDetailParser.parse(html: detailHTML, fallbackTitle: pick.title)
        return (pick.title, detail)
    }

    func testTitleMatchesQueriedFilm() throws {
        let (title, detail) = try firstFilmDetail()
        XCTAssertEqual(detail.title, title,
                       "detail.title should match the title we queried (parsed from `.film-title` or fallback)")
    }

    func testPosterURLIsPresentAndClean() throws {
        let (_, detail) = try firstFilmDetail()
        XCTAssertNotNil(detail.posterURL, "detail should have a poster URL for a film with showings")
        if let s = detail.posterURL?.absoluteString {
            XCTAssertFalse(s.contains("&amp;"), "poster URL still HTML-escaped: \(s)")
        }
    }

    func testAtLeastOneCinemaLink() throws {
        let (_, detail) = try firstFilmDetail()
        XCTAssertGreaterThanOrEqual(detail.cinemaLinks.count, 1,
                                    "expected at least one cinema-link button")
        for link in detail.cinemaLinks {
            XCTAssertFalse(link.cinema.isEmpty, "empty cinema name")
            XCTAssertFalse(link.cinema.contains("↗"),
                           "cinema name should be sanitised of the trailing ↗ glyph")
        }
    }

    func testShowingsNonEmptyAndShowtimesSortedPerCinema() throws {
        let (_, detail) = try firstFilmDetail()
        XCTAssertFalse(detail.showings.isEmpty,
                       "showings tree was empty — FilmDetailParser drift?")
        for day in detail.showings {
            XCTAssertFalse(day.cinemas.isEmpty, "empty cinemas list on \(day.date)")
            for cinema in day.cinemas {
                let minutes = cinema.showtimes.compactMap { slot -> Int? in
                    let parts = slot.time.split(separator: ":").compactMap { Int($0) }
                    return parts.count == 2 ? parts[0] * 60 + parts[1] : nil
                }
                XCTAssertEqual(minutes, minutes.sorted(),
                               "showtimes not sorted on \(day.date) @ \(cinema.cinema): \(cinema.showtimes.map(\.time))")
            }
        }
    }

    func testDatesAscendInIsoOrder() throws {
        let (_, detail) = try firstFilmDetail()
        let dates = detail.showings.map(\.date)
        XCTAssertEqual(dates, dates.sorted(),
                       "showing dates not in ascending order: \(dates)")
    }

    func testAtLeastOneOpisRezyseriaObsadaSurfaces() throws {
        let (_, detail) = try firstFilmDetail()
        let any = detail.synopsis ?? detail.director ?? detail.cast
        XCTAssertNotNil(any,
                        "expected at least one of synopsis / director / cast — template change?")
    }
}
