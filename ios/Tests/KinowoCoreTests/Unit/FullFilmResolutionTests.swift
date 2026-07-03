import XCTest
@testable import KinowoCore

/// The day-paged grids hand the detail screen a `filteredFor` copy whose
/// showings were pruned to the tapped day. The detail screen is a
/// full-schedule view, so ContentView re-resolves the complete, all-days
/// film by title from the unfiltered `store.films` via `fullFilm(for:)`.
/// Guards that the resolution restores every day rather than rendering the
/// single-day grid copy (the "detail only shows one day" bug).
final class FullFilmResolutionTests: XCTestCase {

    private func slot(_ time: String) -> Showtime {
        Showtime(time: time, format: "2D", room: nil, bookingURL: nil)
    }

    private func day(_ date: String, _ label: String) -> DayShowings {
        DayShowings(date: date, label: label, cinemas: [
            CinemaShowings(cinema: "Kino", cinemaURL: nil, showtimes: [slot("19:00")])
        ])
    }

    private func film(_ title: String, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 90, releaseYear: nil, genres: [], ratings: .empty,
             countries: [], directors: [], cast: [], showings: days)
    }

    /// `now` pinned so `filteredFor(.today)` resolves "today" to 2026-05-22.
    private func pinnedNow() -> Date {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw")!
        return DateComponents(calendar: cal, timeZone: cal.timeZone,
                              year: 2026, month: 5, day: 22, hour: 9, minute: 0).date!
    }

    func testResolvesAllDaysFromTheDayFilteredGridCopy() {
        let now = pinnedNow()
        let store = [film("Diuna", [
            day("2026-05-22", "Dziś"),
            day("2026-05-23", "Jutro"),
            day("2026-05-24", "Pojutrze"),
        ])]

        // What the "Dziś" grid page hands the detail screen: only today.
        let gridCopy = store.filteredFor(
            date: .today, format: .empty, query: "",
            hidden: [], now: now
        )
        XCTAssertEqual(gridCopy.first?.showings.map(\.date), ["2026-05-22"],
                       "Precondition: the day-page grid copy is pruned to one day")

        // The detail screen must render the complete, all-days schedule.
        let resolved = store.fullFilm(for: gridCopy.first!)
        XCTAssertEqual(resolved.showings.map(\.date),
                       ["2026-05-22", "2026-05-23", "2026-05-24"],
                       "Detail must show every day, not just the tapped day-page")
    }

    func testFallsBackToTheGivenFilmWhenTitleLeftTheListing() {
        // A film no longer present in store.films (it left the repertoire
        // between tap and resolution) returns the handed-in film unchanged.
        let store = [film("Inny", [day("2026-05-22", "Dziś")])]
        let gone = film("Zniknął", [day("2026-05-22", "Dziś")])
        XCTAssertEqual(store.fullFilm(for: gone).title, "Zniknął")
    }
}
