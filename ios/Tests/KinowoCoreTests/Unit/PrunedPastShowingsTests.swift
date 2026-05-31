import XCTest
@testable import KinowoCore

final class PrunedPastShowingsTests: XCTestCase {

    private static let warsawCalendar: Calendar = {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw")!
        return cal
    }()

    private func warsawDate(year: Int, month: Int, day: Int, hour: Int, minute: Int) -> Date {
        DateComponents(
            calendar: Self.warsawCalendar,
            timeZone: Self.warsawCalendar.timeZone,
            year: year, month: month, day: day, hour: hour, minute: minute
        ).date!
    }

    private func slot(_ time: String) -> Showtime {
        Showtime(time: time, format: "2D", room: nil, bookingURL: nil)
    }

    private func cinema(_ name: String, _ times: [Showtime]) -> CinemaShowings {
        CinemaShowings(cinema: name, cinemaURL: nil, showtimes: times)
    }

    private func day(_ date: String, _ cinemas: [CinemaShowings]) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas)
    }

    private func film(_ title: String, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 90, releaseYear: nil, genres: [], ratings: .empty, countries: [], directors: [], cast: [], showings: days)
    }

    private func pinnedNow() -> Date { warsawDate(year: 2026, month: 5, day: 22, hour: 18, minute: 0) }

    func testDropsSlotPast30MinuteGraceKeepsSlotInside() {
        let now = pinnedNow()
        let films = [film("F", [day("2026-05-22", [
            cinema("Helonki", [slot("17:29"), slot("17:35")]),
        ])])]
        let pruned = films.prunedPastShowings(now: now)
        let kept = pruned[0].showings[0].cinemas[0].showtimes.map(\.time)
        XCTAssertEqual(kept, ["17:35"])
    }

    func testDropsSlotExactlyAtNowMinus30() {
        // Web uses strict `isAfter(now - 30min)` — a slot at exactly that
        // moment is dropped.
        let now = pinnedNow()
        let films = [film("F", [day("2026-05-22", [
            cinema("Helonki", [slot("17:30"), slot("17:31")]),
        ])])]
        let pruned = films.prunedPastShowings(now: now)
        let kept = pruned[0].showings[0].cinemas[0].showtimes.map(\.time)
        XCTAssertEqual(kept, ["17:31"])
    }

    func testCinemasInsideDayReorderedByEarliestRemaining() {
        let now = pinnedNow()
        let films = [film("F", [day("2026-05-22", [
            cinema("Apollo",  [slot("15:00"), slot("18:30")]),
            cinema("Helonki", [slot("16:00"), slot("17:35")]),
        ])])]
        let pruned = films.prunedPastShowings(now: now)
        let order = pruned[0].showings[0].cinemas.map(\.cinema)
        XCTAssertEqual(order, ["Helonki", "Apollo"])
    }

    func testDayWithNoRemainingCinemasRemoved() {
        let now = pinnedNow()
        let films = [film("F", [
            day("2026-05-22", [cinema("Muza", [slot("10:00"), slot("12:00")])]),
            day("2026-05-23", [cinema("Apollo", [slot("10:00")])]),
        ])]
        let pruned = films.prunedPastShowings(now: now)
        XCTAssertEqual(pruned[0].showings.map(\.date), ["2026-05-23"])
    }

    func testFilmWithNoRemainingDaysRemovedEntirely() {
        let now = pinnedNow()
        let films = [
            film("AllPast", [day("2026-05-22", [
                cinema("Muza", [slot("10:00"), slot("13:00")])
            ])]),
            film("Live", [day("2026-05-22", [
                cinema("Apollo", [slot("19:00")])
            ])]),
        ]
        let pruned = films.prunedPastShowings(now: now)
        XCTAssertEqual(pruned.map(\.title), ["Live"])
    }

    func testTomorrowSlotsUntouched() {
        let now = pinnedNow()
        let films = [film("F", [
            day("2026-05-23", [cinema("Apollo", [
                slot("10:00"), slot("12:30"), slot("16:00"),
            ])]),
        ])]
        let pruned = films.prunedPastShowings(now: now)
        XCTAssertEqual(
            pruned[0].showings[0].cinemas[0].showtimes.map(\.time),
            ["10:00", "12:30", "16:00"]
        )
    }

    func testIdempotent() {
        let now = pinnedNow()
        let films = [film("F", [day("2026-05-22", [
            cinema("Apollo",  [slot("15:00"), slot("18:30")]),
            cinema("Helonki", [slot("17:35"), slot("19:00")]),
        ])])]
        let once = films.prunedPastShowings(now: now)
        let twice = once.prunedPastShowings(now: now)
        XCTAssertEqual(once, twice)
    }

    func testComposedScenarioMirrorsWebSemantics() {
        // Port of the deleted Tools/PrunePastShowingsTest.swift fixture:
        // three cinemas with a mix of past, edge, grace, and future slots
        // across today + tomorrow.
        let now = pinnedNow()
        let today = "2026-05-22"
        let tomorrow = "2026-05-23"

        let helonki = cinema("Helonki", [
            slot("16:00"), slot("17:30"), slot("17:35"), slot("19:00"),
        ])
        let apollo = cinema("Apollo", [slot("15:00"), slot("18:30")])
        let muza = cinema("Muza", [slot("10:00"), slot("12:30")])

        let liveFilm = film("Live", [
            day(today, [apollo, helonki, muza]),
            day(tomorrow, [cinema("Apollo", [slot("10:00")])]),
        ])
        let allPast = film("All-Past", [
            day(today, [cinema("Muza", [slot("10:00"), slot("13:00")])]),
        ])

        let pruned = [liveFilm, allPast].prunedPastShowings(now: now)
        XCTAssertEqual(pruned.map(\.title), ["Live"])

        let todayDay = pruned[0].showings.first { $0.date == today }!
        XCTAssertEqual(todayDay.cinemas.map(\.cinema), ["Helonki", "Apollo"])
        let helonkiKept = todayDay.cinemas.first { $0.cinema == "Helonki" }!
        XCTAssertEqual(helonkiKept.showtimes.map(\.time), ["17:35", "19:00"])
        let apolloKept = todayDay.cinemas.first { $0.cinema == "Apollo" }!
        XCTAssertEqual(apolloKept.showtimes.map(\.time), ["18:30"])

        let tomorrowDay = pruned[0].showings.first { $0.date == tomorrow }!
        XCTAssertEqual(tomorrowDay.cinemas.count, 1)
        XCTAssertEqual(tomorrowDay.cinemas[0].showtimes.map(\.time), ["10:00"])
    }
}
