import XCTest
@testable import KinowoCore

final class ShowtimeClockTests: XCTestCase {

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

    private func cg(_ times: [String]) -> CinemaShowings {
        CinemaShowings(cinema: "X", cinemaURL: nil, showtimes: times.map(slot))
    }

    private func pinnedNow() -> Date { warsawDate(year: 2026, month: 5, day: 22, hour: 18, minute: 0) }

    func testIsFutureAtNowMinus30IsDropped() {
        XCTAssertFalse(ShowtimeClock.isFuture(slot("17:30"), on: "2026-05-22", now: pinnedNow()))
    }

    func testIsFutureOneMinuteInsideGraceIsKept() {
        XCTAssertTrue(ShowtimeClock.isFuture(slot("17:31"), on: "2026-05-22", now: pinnedNow()))
    }

    func testIsFutureLaterTodayIsKept() {
        XCTAssertTrue(ShowtimeClock.isFuture(slot("19:00"), on: "2026-05-22", now: pinnedNow()))
    }

    func testIsFureMalformedTimeIsKeptDefensively() {
        XCTAssertTrue(ShowtimeClock.isFuture(slot("abc"), on: "2026-05-22", now: pinnedNow()))
        XCTAssertTrue(ShowtimeClock.isFuture(slot(""), on: "2026-05-22", now: pinnedNow()))
    }

    func testIsFutureFarPastIsDropped() {
        XCTAssertFalse(ShowtimeClock.isFuture(slot("10:00"), on: "2026-05-22", now: pinnedNow()))
    }

    func testEarliestMinutesPicksMinimum() {
        XCTAssertEqual(ShowtimeClock.earliestMinutes(cg(["18:00", "10:30", "21:45"])), 10 * 60 + 30)
        XCTAssertEqual(ShowtimeClock.earliestMinutes(cg(["09:00"])), 9 * 60)
    }

    func testEarliestMinutesEmptyReturnsIntMax() {
        XCTAssertEqual(ShowtimeClock.earliestMinutes(cg([])), Int.max)
    }

    func testEarliestMinutesIgnoresMalformedTimes() {
        XCTAssertEqual(ShowtimeClock.earliestMinutes(cg(["abc", "20:00", "x:y"])), 20 * 60)
        XCTAssertEqual(ShowtimeClock.earliestMinutes(cg(["abc", "x:y"])), Int.max)
    }
}
