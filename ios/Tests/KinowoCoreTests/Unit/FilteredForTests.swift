import XCTest
@testable import KinowoCore

final class FilteredForTests: XCTestCase {

    private func slot(_ time: String, _ format: String = "2D NAP") -> Showtime {
        Showtime(time: time, format: format, room: nil, bookingURL: nil)
    }

    private func cinema(_ name: String, _ times: [Showtime]) -> CinemaShowings {
        CinemaShowings(cinema: name, cinemaURL: nil, showtimes: times)
    }

    private func day(_ date: String, _ cinemas: [CinemaShowings]) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas)
    }

    private func film(_ title: String, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 100, ratings: .empty, showings: days)
    }

    // `filteredFor` calls `date.matches(date:)` without a `now` parameter,
    // so `.today` / `.tomorrow` resolve against `Date()`. Build the
    // fixture's day strings from the live wall-clock so the `.today` case
    // is exercisable regardless of when the suite runs.
    private let today: String = DateFilter.iso(Date())
    private let tomorrow: String = DateFilter.iso(Date().addingTimeInterval(86_400))

    private func fixture() -> [Film] {
        let mandalorian = film("Mandalorian and Grogu", [
            day(today, [
                cinema("Helonki", [slot("17:00"), slot("20:00", "3D NAP")]),
                cinema("Apollo",  [slot("19:30")]),
            ]),
            day(tomorrow, [
                cinema("Muza", [slot("16:00")]),
            ]),
        ])
        let title2 = film("Title2", [
            day(today, [
                cinema("Apollo", [slot("18:00")]),
            ]),
        ])
        let title3 = film("Title3", [
            day(tomorrow, [
                cinema("Helonki", [slot("21:00")]),
            ]),
        ])
        return [mandalorian, title2, title3]
    }

    func testDateTodayFiltersToTodayOnly() {
        let result = fixture().filteredFor(
            date: .today, format: .empty, query: "",
            hidden: [], disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title).sorted(), ["Mandalorian and Grogu", "Title2"])
        for f in result {
            XCTAssertEqual(f.showings.map(\.date), [today])
        }
    }

    func testQueryMatchesCaseInsensitiveSubstring() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "Mand",
            hidden: [], disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])

        let lowered = fixture().filteredFor(
            date: .anytime, format: .empty, query: "mand",
            hidden: [], disabledCinemas: []
        )
        XCTAssertEqual(lowered.map(\.title), ["Mandalorian and Grogu"])
    }

    func testHiddenDropsFilmEntirely() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: ["Title2"], disabledCinemas: []
        )
        XCTAssertFalse(result.contains { $0.title == "Title2" })
        XCTAssertEqual(result.count, 2)
    }

    func testDisabledCinemasDropsCinemaGroupAndCollapsesEmptyDays() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: ["Apollo"]
        )
        // Title2 only ever played at Apollo today → film drops entirely.
        XCTAssertFalse(result.contains { $0.title == "Title2" })
        let mando = result.first { $0.title == "Mandalorian and Grogu" }!
        let todayDay = mando.showings.first { $0.date == today }!
        XCTAssertEqual(todayDay.cinemas.map(\.cinema), ["Helonki"])
    }

    func testFormatNarrowsShowtimesButKeepsFilmIfAnySlotRemains() {
        var f = FormatFilter()
        f.dimension = "3D"
        let result = fixture().filteredFor(
            date: .anytime, format: f, query: "",
            hidden: [], disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])
        let mando = result[0]
        let todayDay = mando.showings.first { $0.date == today }!
        XCTAssertEqual(todayDay.cinemas.count, 1)
        XCTAssertEqual(todayDay.cinemas[0].cinema, "Helonki")
        XCTAssertEqual(todayDay.cinemas[0].showtimes.map(\.time), ["20:00"])
    }

    func testEmptyQueryTrimsWhitespace() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "   ",
            hidden: [], disabledCinemas: []
        )
        XCTAssertEqual(result.count, 3)
    }

    func testCombinedFiltersIntersect() {
        let result = fixture().filteredFor(
            date: .today, format: .empty, query: "Mand",
            hidden: ["Title2"], disabledCinemas: ["Apollo"]
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])
        let todayDay = result[0].showings[0]
        XCTAssertEqual(todayDay.cinemas.map(\.cinema), ["Helonki"])
    }
}
