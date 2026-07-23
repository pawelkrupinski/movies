import XCTest
@testable import KinowoCore

final class CinemaLabelVisibilityTests: XCTestCase {

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
             runtimeMinutes: 100, releaseYear: nil, genres: [], ratings: .empty,
             countries: [], directors: [], cast: [], showings: days)
    }

    private let today: String = DateFilter.iso(Date())
    private let tomorrow: String = DateFilter.iso(Date().addingTimeInterval(86_400))

    func testSingleCinemaCityHidesTheLabel() {
        XCTAssertFalse(CinemaLabelVisibility.showsLabels(cityCinemas: ["Apollo"], disabledCinemas: []))
    }

    func testSeveralCinemasShowTheLabel() {
        XCTAssertTrue(CinemaLabelVisibility.showsLabels(cityCinemas: ["Apollo", "Muza"], disabledCinemas: []))
    }

    func testNarrowingTheFiltrySheetToOneCinemaHidesTheLabel() {
        XCTAssertFalse(CinemaLabelVisibility.showsLabels(
            cityCinemas: ["Apollo", "Muza", "Helonki"],
            disabledCinemas: ["Muza", "Helonki"]
        ))
    }

    func testExclusionsFromOtherCitiesDoNotHideTheLabel() {
        // `UserPreferences.disabledCinemas` is global, so it carries cinemas
        // that don't exist in the current city; those must not count.
        XCTAssertTrue(CinemaLabelVisibility.showsLabels(
            cityCinemas: ["Apollo", "Muza"],
            disabledCinemas: ["Cinema City Wrocław", "Helios Gdańsk"]
        ))
    }

    // The regression: by late evening the only screenings left today can all be
    // at one cinema. That used to drop the cinema label off every card — the
    // flag was computed from the films on screen — leaving the user unable to
    // tell where those last shows play, and it flipped back on tomorrow's page.
    // The decision is now keyed on the cinemas the city offers, which the day
    // filter can't shrink.
    func testLastScreeningsOfTheDayAtOneCinemaKeepTheLabel() {
        let films = [
            film("Late Show", [day(today, [cinema("Apollo", [slot("22:30")])])]),
            film("Tomorrow's Premiere", [
                day(tomorrow, [
                    cinema("Muza", [slot("18:00")]),
                    cinema("Apollo", [slot("20:00")]),
                ]),
            ]),
        ]

        // What the buggy version looked at: today's slice names one cinema.
        let todayOnly = films.filteredFor(date: .today, format: .empty, query: "", hidden: [])
        XCTAssertEqual(todayOnly.allCinemas(), ["Apollo"])

        // What it looks at now — the city's cinemas, day-independent.
        XCTAssertEqual(films.allCinemas(), ["Apollo", "Muza"])
        XCTAssertTrue(CinemaLabelVisibility.showsLabels(cityCinemas: films.allCinemas(),
                                                        disabledCinemas: []))
    }
}
