import XCTest
@testable import KinowoCore

// Runs the home parser against a live render from `FixtureServerMain`.
// Supersedes `HomeFixtureShapeTests` and `CinemaSectionConsistencyTests`
// — both inspected a captured static snapshot of `/` and so only
// caught parser drift. This file catches both parser drift AND
// server-side template changes that would shift the rendered HTML.
final class LocalServerHomeTests: LocalServerTestCase {

    private func parseHome() throws -> [Film] {
        let html = try fetchHTML(path: "/")
        return HTMLParser.parse(html: html)
    }

    // ── Shape ─────────────────────────────────────────────────────

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

    func testTotalShowtimesIsLarge() throws {
        let films = try parseHome()
        let total = films.reduce(0) { acc, film in
            acc + film.showings.reduce(0) { $0 + $1.cinemas.reduce(0) { $0 + $1.showtimes.count } }
        }
        XCTAssertGreaterThan(total, 100,
                             "expected a meaningful number of showtimes from the fixture; got \(total)")
    }

    // ── Per-element invariants ────────────────────────────────────

    func testEveryShowtimeCarriesHHMMTime() throws {
        let films = try parseHome()
        let pattern = #"^\d{2}:\d{2}$"#
        let re = try NSRegularExpression(pattern: pattern)
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
        // Spot-checks every URL the parser surfaces: posters,
        // fallbacks, cinema links, booking links. If any reaches
        // SwiftUI's `Link` with `&amp;` still embedded, the browser
        // dispatches to a 404. This invariant is the tripwire.
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

    // ── groupedByCinema() consistency ─────────────────────────────

    func testEveryFilmInSectionPlaysOnlyAtThatCinema() throws {
        let films = try parseHome()
        let sections = films.groupedByCinema()
        XCTAssertFalse(sections.isEmpty)

        for section in sections {
            for film in section.films {
                for day in film.showings {
                    for cinema in day.cinemas {
                        XCTAssertEqual(cinema.cinema, section.cinema,
                                       "section \(section.cinema) contains film \(film.title) playing at \(cinema.cinema)")
                    }
                }
            }
        }
    }

    func testSectionsAreAlphabeticalByCinemaName() throws {
        let films = try parseHome()
        let sections = films.groupedByCinema()
        let names = sections.map(\.cinema)
        XCTAssertEqual(names, names.sorted(),
                       "sections not alphabetical: \(names)")
    }

    func testUnionOfCinemasInOneSectionIsTheSectionCinema() throws {
        let films = try parseHome()
        let sections = films.groupedByCinema()

        for section in sections {
            let union = Set(
                section.films
                    .flatMap(\.showings)
                    .flatMap(\.cinemas)
                    .map(\.cinema)
            )
            XCTAssertEqual(union, [section.cinema],
                           "section \(section.cinema) leaked cinemas: \(union)")
        }
    }

    func testTotalFilmPresencesIsAtLeastFilmCount() throws {
        let films = try parseHome()
        let sections = films.groupedByCinema()
        let presences = sections.reduce(0) { $0 + $1.films.count }
        XCTAssertGreaterThanOrEqual(presences, films.count,
                                    "presences \(presences) < film count \(films.count); films appear in multiple sections so this should never be less")
    }

    // ── Ratings ──────────────────────────────────────────────────

    func testAtLeastOneFilmCarriesAnImdbRating() throws {
        let films = try parseHome()
        let withImdb = films.filter { $0.ratings.imdb != nil }.count
        XCTAssertGreaterThan(withImdb, 0,
                             "no films in the home render carried an IMDb rating — RatingsParser drift?")
    }
}
