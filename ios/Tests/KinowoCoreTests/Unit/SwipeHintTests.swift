import XCTest
@testable import KinowoCore

final class SwipeHintTests: XCTestCase {

    func testShowsWhenNeverSwipedAndNotShownToday() {
        XCTAssertTrue(SwipeHint.shouldShow(hasSwiped: false, lastShownDate: "", today: "2026-05-31"))
        XCTAssertTrue(SwipeHint.shouldShow(hasSwiped: false, lastShownDate: "2026-05-30", today: "2026-05-31"))
    }

    func testHiddenOnceAlreadyShownToday() {
        XCTAssertFalse(SwipeHint.shouldShow(hasSwiped: false, lastShownDate: "2026-05-31", today: "2026-05-31"))
    }

    func testNeverShownAgainAfterFirstSwipe() {
        // Even on a fresh day, a user who has swiped never sees the hint again.
        XCTAssertFalse(SwipeHint.shouldShow(hasSwiped: true, lastShownDate: "", today: "2026-05-31"))
        XCTAssertFalse(SwipeHint.shouldShow(hasSwiped: true, lastShownDate: "2026-05-30", today: "2026-05-31"))
    }

    func testDayKeyFormatsZeroPaddedYearMonthDay() {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw")!
        let date = DateComponents(calendar: cal, year: 2026, month: 5, day: 9).date!
        XCTAssertEqual(SwipeHint.dayKey(date, calendar: cal), "2026-05-09")
    }
}
