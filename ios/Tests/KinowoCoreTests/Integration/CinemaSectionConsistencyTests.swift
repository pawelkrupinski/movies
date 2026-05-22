import XCTest
@testable import KinowoCore

final class CinemaSectionConsistencyTests: XCTestCase {

    private func parseHome() throws -> [Film] {
        let html = try Fixtures.load("home")
        return HTMLParser.parse(html: html)
    }

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
                                    "presences \(presences) < film count \(films.count); films can appear in multiple sections so this should never be less")
    }
}
