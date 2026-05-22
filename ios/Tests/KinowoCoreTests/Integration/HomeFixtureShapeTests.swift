import XCTest
@testable import KinowoCore

final class HomeFixtureShapeTests: XCTestCase {

    private func parseHome() throws -> [Film] {
        let html = try Fixtures.load("home")
        return HTMLParser.parse(html: html)
    }

    func testFilmCountIsHealthy() throws {
        let films = try parseHome()
        XCTAssertGreaterThanOrEqual(films.count, 20,
                                    "expected a healthy slate of films; got \(films.count)")
    }

    func testEveryFilmHasNonEmptyTitle() throws {
        let films = try parseHome()
        XCTAssertFalse(films.isEmpty)
        for film in films {
            XCTAssertFalse(film.title.isEmpty, "film with empty title")
        }
    }

    func testAtLeastEightyPercentOfFilmsHaveAPoster() throws {
        let films = try parseHome()
        XCTAssertFalse(films.isEmpty)
        let withPoster = films.filter { $0.posterURL != nil }.count
        let ratio = Double(withPoster) / Double(films.count)
        XCTAssertGreaterThanOrEqual(ratio, 0.8,
                                    "only \(withPoster)/\(films.count) films had a poster")
    }

    func testEveryShowtimeCarriesHHMMTime() throws {
        let films = try parseHome()
        let pattern = #"^\d{2}:\d{2}$"#
        guard let re = try? NSRegularExpression(pattern: pattern) else {
            XCTFail("could not build HH:MM regex")
            return
        }
        var checked = 0
        for film in films {
            for day in film.showings {
                for cinema in day.cinemas {
                    for slot in cinema.showtimes {
                        let ns = slot.time as NSString
                        let m = re.firstMatch(in: slot.time, range: NSRange(location: 0, length: ns.length))
                        XCTAssertNotNil(m, "non-HH:MM time \(slot.time) on \(film.title) @ \(cinema.cinema)")
                        checked += 1
                    }
                }
            }
        }
        XCTAssertGreaterThan(checked, 0, "no showtimes inspected")
    }

    func testFallbackPosterChainsDecodeIntoMultipleURLs() throws {
        let films = try parseHome()
        let withChains = films.filter { $0.fallbackPosterURLs.count >= 2 }
        XCTAssertGreaterThanOrEqual(withChains.count, 1,
                                    "expected at least one film with a multi-URL data-fallbacks chain")
    }

    func testNoExposedURLCarriesEscapedAmpersand() throws {
        let films = try parseHome()
        var urls: [String] = []
        for film in films {
            if let s = film.posterURL?.absoluteString { urls.append(s) }
            urls.append(contentsOf: film.fallbackPosterURLs.map(\.absoluteString))
            for day in film.showings {
                for cinema in day.cinemas {
                    if let s = cinema.cinemaURL?.absoluteString { urls.append(s) }
                    for slot in cinema.showtimes {
                        if let s = slot.bookingURL?.absoluteString { urls.append(s) }
                    }
                }
            }
        }
        XCTAssertGreaterThan(urls.count, 0)
        for url in urls {
            XCTAssertFalse(url.contains("&amp;"), "URL still HTML-escaped: \(url)")
        }
    }

    func testTotalShowtimesIsLarge() throws {
        let films = try parseHome()
        let total = films.reduce(0) { acc, film in
            acc + film.showings.reduce(0) { $0 + $1.cinemas.reduce(0) { $0 + $1.showtimes.count } }
        }
        XCTAssertGreaterThanOrEqual(total, 1000,
                                    "expected at least 1000 showtimes in the home fixture; got \(total)")
    }
}
