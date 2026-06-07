import XCTest
@testable import KinowoCore

final class DaySwipeTests: XCTestCase {

    func testForwardStepWithinBounds() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 1, count: 4), 1)
        XCTAssertEqual(wrappedDayIndex(current: 2, delta: 1, count: 4), 3)
    }

    func testBackwardStepWithinBounds() {
        XCTAssertEqual(wrappedDayIndex(current: 3, delta: -1, count: 4), 2)
        XCTAssertEqual(wrappedDayIndex(current: 1, delta: -1, count: 4), 0)
    }

    func testForwardWrapsPastLast() {
        // Stepping right off the last pill lands back on the first.
        XCTAssertEqual(wrappedDayIndex(current: 3, delta: 1, count: 4), 0)
    }

    func testBackwardWrapsPastFirst() {
        // Stepping left off the first pill lands on the last.
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: -1, count: 4), 3)
    }

    func testLargerDeltaStillWraps() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 5, count: 4), 1)
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: -5, count: 4), 3)
    }

    func testSingleEntryStaysPut() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 1, count: 1), 0)
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: -1, count: 1), 0)
    }

    func testEmptyListReturnsCurrent() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 1, count: 0), 0)
    }
}
