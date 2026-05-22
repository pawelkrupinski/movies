import XCTest
@testable import KinowoCore

final class DateFilterTests: XCTestCase {

    private static let warsawCalendar: Calendar = {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw")!
        return cal
    }()

    private func warsawDate(year: Int, month: Int, day: Int, hour: Int = 12, minute: Int = 0) -> Date {
        let comps = DateComponents(
            calendar: Self.warsawCalendar,
            timeZone: Self.warsawCalendar.timeZone,
            year: year, month: month, day: day, hour: hour, minute: minute
        )
        return comps.date!
    }

    func testAnytimeMatchesAnyDate() {
        let now = warsawDate(year: 2026, month: 5, day: 22)
        XCTAssertTrue(DateFilter.anytime.matches(date: "2026-05-22", now: now))
        XCTAssertTrue(DateFilter.anytime.matches(date: "2099-12-31", now: now))
        XCTAssertTrue(DateFilter.anytime.matches(date: "1999-01-01", now: now))
    }

    func testTodayMatchesOnlyTodayInWarsaw() {
        let now = warsawDate(year: 2026, month: 5, day: 22, hour: 12, minute: 0)
        XCTAssertTrue(DateFilter.today.matches(date: "2026-05-22", now: now))
        XCTAssertFalse(DateFilter.today.matches(date: "2026-05-23", now: now))
        XCTAssertFalse(DateFilter.today.matches(date: "2026-05-21", now: now))
    }

    func testTomorrowMatchesNextDay() {
        let now = warsawDate(year: 2026, month: 5, day: 22, hour: 12, minute: 0)
        XCTAssertTrue(DateFilter.tomorrow.matches(date: "2026-05-23", now: now))
        XCTAssertFalse(DateFilter.tomorrow.matches(date: "2026-05-22", now: now))
        XCTAssertFalse(DateFilter.tomorrow.matches(date: "2026-05-24", now: now))
    }

    func testWeekMatchesTodayThroughDayPlus7Inclusive() {
        let now = warsawDate(year: 2026, month: 5, day: 22, hour: 12, minute: 0)
        XCTAssertTrue(DateFilter.week.matches(date: "2026-05-22", now: now))
        XCTAssertTrue(DateFilter.week.matches(date: "2026-05-26", now: now))
        XCTAssertTrue(DateFilter.week.matches(date: "2026-05-29", now: now))
        XCTAssertFalse(DateFilter.week.matches(date: "2026-05-30", now: now))
        XCTAssertFalse(DateFilter.week.matches(date: "2026-05-21", now: now))
    }

    func testSpecificMatchesExactDateOnly() {
        let now = warsawDate(year: 2026, month: 5, day: 22)
        XCTAssertTrue(DateFilter.specific("2026-05-22").matches(date: "2026-05-22", now: now))
        XCTAssertFalse(DateFilter.specific("2026-05-22").matches(date: "2026-05-23", now: now))
        XCTAssertFalse(DateFilter.specific("2026-05-22").matches(date: "2026-05-21", now: now))
    }

    func testIsoReturnsWarsawLocalDate() {
        let noon = warsawDate(year: 2026, month: 5, day: 22, hour: 12, minute: 0)
        XCTAssertEqual(DateFilter.iso(noon), "2026-05-22")

        // 23:30 Warsaw on 2026-05-22 is still 2026-05-22 locally, not the
        // UTC date — the formatter must honour the Warsaw timezone.
        let lateNight = warsawDate(year: 2026, month: 5, day: 22, hour: 23, minute: 30)
        XCTAssertEqual(DateFilter.iso(lateNight), "2026-05-22")
    }

    func testTodayAcrossDSTSpringForward() {
        // Poland flips to summer time on the last Sunday of March (2026-03-29);
        // any "today" right before/after that boundary still resolves to a
        // calendar date in Warsaw, not a UTC-shifted one.
        let beforeDST = warsawDate(year: 2026, month: 3, day: 28, hour: 23, minute: 0)
        XCTAssertTrue(DateFilter.today.matches(date: "2026-03-28", now: beforeDST))
        let afterDST = warsawDate(year: 2026, month: 3, day: 29, hour: 12, minute: 0)
        XCTAssertTrue(DateFilter.today.matches(date: "2026-03-29", now: afterDST))
    }
}
