import XCTest
@testable import KinowoCore

final class FormatFilterTests: XCTestCase {

    private func slot(_ time: String, _ format: String) -> Showtime {
        Showtime(time: time, format: format, room: nil, bookingURL: nil)
    }

    func testEmptyFilterMatchesEverything() {
        let f = FormatFilter()
        XCTAssertTrue(f.isEmpty)
        XCTAssertTrue(f.matches(showtime: slot("10:00", "2D NAP")))
        XCTAssertTrue(f.matches(showtime: slot("23:59", "3D DUB IMAX")))
        XCTAssertTrue(f.matches(showtime: slot("abc", "")))
    }

    func testDimensionConstraint() {
        var f = FormatFilter()
        f.dimension = "3D"
        XCTAssertFalse(f.isEmpty)
        XCTAssertTrue(f.matches(showtime: slot("18:00", "3D NAP")))
        XCTAssertFalse(f.matches(showtime: slot("18:00", "2D NAP")))
    }

    func testLanguageConstraint() {
        var f = FormatFilter()
        f.language = "NAP"
        XCTAssertTrue(f.matches(showtime: slot("18:00", "2D NAP")))
        XCTAssertFalse(f.matches(showtime: slot("18:00", "2D DUB")))
    }

    func testImaxRequiresImaxToken() {
        var f = FormatFilter()
        f.imax = true
        XCTAssertTrue(f.matches(showtime: slot("20:00", "IMAX 3D")))
        XCTAssertFalse(f.matches(showtime: slot("20:00", "3D NAP")))
    }

    func testFromHourMinuteBoundary() {
        var f = FormatFilter()
        f.fromHour = 18
        f.fromMinute = 30
        XCTAssertEqual(f.fromMinutes, 18 * 60 + 30)
        XCTAssertTrue(f.matches(showtime: slot("18:30", "2D NAP")))
        XCTAssertFalse(f.matches(showtime: slot("18:29", "2D NAP")))
        XCTAssertTrue(f.matches(showtime: slot("19:00", "2D NAP")))
        // Unparseable time is kept — mirrors the web's `timeMin < 0` guard.
        XCTAssertTrue(f.matches(showtime: slot("abc", "2D NAP")))
    }

    func testFromHourDowolnaIsNoConstraint() {
        var f = FormatFilter()
        f.fromHour = -1
        f.fromMinute = 30
        XCTAssertNil(f.fromMinutes)
        XCTAssertTrue(f.isEmpty)
        XCTAssertTrue(f.matches(showtime: slot("00:00", "2D NAP")))
    }

    func testMultipleConstraintsCombine() {
        var f = FormatFilter()
        f.dimension = "3D"
        f.language = "NAP"
        f.fromHour = 18
        f.fromMinute = 30
        XCTAssertTrue(f.matches(showtime: slot("19:00", "3D NAP")))
        XCTAssertFalse(f.matches(showtime: slot("18:00", "3D NAP")))
        XCTAssertFalse(f.matches(showtime: slot("19:00", "2D NAP")))
        XCTAssertFalse(f.matches(showtime: slot("19:00", "3D DUB")))
        XCTAssertTrue(f.matches(showtime: slot("18:30", "3D NAP IMAX")))
    }
}
