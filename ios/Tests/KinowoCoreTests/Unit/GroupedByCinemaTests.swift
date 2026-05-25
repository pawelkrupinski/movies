import XCTest
@testable import KinowoCore

final class GroupedByCinemaTests: XCTestCase {

    private func slot(_ time: String) -> Showtime {
        Showtime(time: time, format: "2D NAP", room: nil, bookingURL: nil)
    }

    private func cinema(_ name: String, _ times: [Showtime]) -> CinemaShowings {
        CinemaShowings(cinema: name, cinemaURL: nil, showtimes: times)
    }

    private func day(_ date: String, _ cinemas: [CinemaShowings]) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas)
    }

    private func film(_ title: String, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 100, ratings: .empty, countries: [], directors: [], cast: [], showings: days)
    }

    func testEmptyInputReturnsEmptyArray() {
        XCTAssertTrue([Film]().groupedByCinema().isEmpty)
    }

    func testSectionsSortedAlphabeticallyByCinemaName() {
        let films = [
            film("F1", [day("2026-05-22", [
                cinema("Muza",    [slot("10:00")]),
                cinema("Apollo",  [slot("11:00")]),
                cinema("Helonki", [slot("12:00")]),
            ])]),
        ]
        let sections = films.groupedByCinema()
        XCTAssertEqual(sections.map(\.cinema), ["Apollo", "Helonki", "Muza"])
    }

    func testFilmAtTwoCinemasAppearsInBothSections() {
        let films = [
            film("Dune", [day("2026-05-22", [
                cinema("Apollo",  [slot("18:00")]),
                cinema("Helonki", [slot("20:00")]),
            ])]),
        ]
        let sections = films.groupedByCinema()
        XCTAssertEqual(sections.count, 2)
        XCTAssertEqual(sections.map(\.cinema), ["Apollo", "Helonki"])
        XCTAssertEqual(sections[0].films.map(\.title), ["Dune"])
        XCTAssertEqual(sections[1].films.map(\.title), ["Dune"])
    }

    func testEachOutputFilmCarriesOnlyThatCinemasSlots() {
        let films = [
            film("Dune", [day("2026-05-22", [
                cinema("Apollo",  [slot("18:00"), slot("21:00")]),
                cinema("Helonki", [slot("20:00")]),
            ])]),
        ]
        let sections = films.groupedByCinema()
        let apollo = sections.first { $0.cinema == "Apollo" }!
        let helonki = sections.first { $0.cinema == "Helonki" }!

        XCTAssertEqual(apollo.films[0].showings.count, 1)
        XCTAssertEqual(apollo.films[0].showings[0].cinemas.map(\.cinema), ["Apollo"])
        XCTAssertEqual(apollo.films[0].showings[0].cinemas[0].showtimes.map(\.time), ["18:00", "21:00"])

        XCTAssertEqual(helonki.films[0].showings.count, 1)
        XCTAssertEqual(helonki.films[0].showings[0].cinemas.map(\.cinema), ["Helonki"])
        XCTAssertEqual(helonki.films[0].showings[0].cinemas[0].showtimes.map(\.time), ["20:00"])
    }

    func testFilmAcrossMultipleDaysGroupsByCinema() {
        let films = [
            film("Belle", [
                day("2026-05-22", [cinema("Apollo", [slot("18:00")])]),
                day("2026-05-23", [
                    cinema("Apollo",  [slot("19:00")]),
                    cinema("Helonki", [slot("20:00")]),
                ]),
            ]),
        ]
        let sections = films.groupedByCinema()
        XCTAssertEqual(sections.map(\.cinema), ["Apollo", "Helonki"])

        let apollo = sections[0].films[0]
        XCTAssertEqual(apollo.showings.map(\.date), ["2026-05-22", "2026-05-23"])

        let helonki = sections[1].films[0]
        XCTAssertEqual(helonki.showings.map(\.date), ["2026-05-23"])
    }

    func testFilmsInSectionPreserveInputOrder() {
        let films = [
            film("Zebra", [day("2026-05-22", [cinema("Apollo", [slot("18:00")])])]),
            film("Alpha", [day("2026-05-22", [cinema("Apollo", [slot("19:00")])])]),
            film("Middle", [day("2026-05-22", [cinema("Apollo", [slot("20:00")])])]),
        ]
        let sections = films.groupedByCinema()
        XCTAssertEqual(sections.count, 1)
        XCTAssertEqual(sections[0].films.map(\.title), ["Zebra", "Alpha", "Middle"])
    }
}
