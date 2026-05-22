import XCTest
@testable import KinowoCore

final class FilmDetailFixtureShapeTests: XCTestCase {

    private func parseMandalorian() throws -> FilmDetail {
        let html = try Fixtures.load("film_mandalorian")
        return FilmDetailParser.parse(html: html, fallbackTitle: "Mandalorian i Grogu")
    }

    func testTitleMatchesExpected() throws {
        let detail = try parseMandalorian()
        XCTAssertEqual(detail.title, "Mandalorian i Grogu",
                       "title should be parsed from <div class=\"film-title\"> or fall back to the supplied fallbackTitle")
    }

    func testPosterURLIsPresent() throws {
        let detail = try parseMandalorian()
        XCTAssertNotNil(detail.posterURL)
        if let s = detail.posterURL?.absoluteString {
            XCTAssertFalse(s.contains("&amp;"), "poster URL still HTML-escaped: \(s)")
        }
    }

    func testAtLeastOneCinemaLink() throws {
        let detail = try parseMandalorian()
        XCTAssertGreaterThanOrEqual(detail.cinemaLinks.count, 1)
        for link in detail.cinemaLinks {
            XCTAssertFalse(link.cinema.isEmpty)
            XCTAssertFalse(link.cinema.contains("↗"))
        }
    }

    func testAtLeastOneOfOpisRezyseriaObsadaIsPresent() throws {
        let detail = try parseMandalorian()
        let any = detail.synopsis ?? detail.director ?? detail.cast
        XCTAssertNotNil(any, "expected at least one of Opis / Reżyseria / Obsada to be present")
    }

    func testShowingsNonEmptyAndShowtimesSortedByTimePerCinema() throws {
        let detail = try parseMandalorian()
        XCTAssertFalse(detail.showings.isEmpty)
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
        let detail = try parseMandalorian()
        let dates = detail.showings.map(\.date)
        XCTAssertEqual(dates, dates.sorted(),
                       "dates not in ascending order: \(dates)")
    }
}
