import XCTest
@testable import KinowoCore

/// `Showtime.displayRoom` decides whether a long-press on a time pill
/// has a room to reveal — and what text it shows.
final class DisplayRoomTests: XCTestCase {

    private func slot(room: String?) -> Showtime {
        Showtime(time: "18:00", format: "2D", room: room, bookingURL: nil)
    }

    func testRealRoomIsSurfaced() {
        XCTAssertEqual(slot(room: "Sala 9").displayRoom, "Sala 9")
    }

    func testNilRoomHasNothingToShow() {
        XCTAssertNil(slot(room: nil).displayRoom)
    }

    func testEmptyRoomHasNothingToShow() {
        XCTAssertNil(slot(room: "").displayRoom)
    }

    func testWhitespaceOnlyRoomHasNothingToShow() {
        XCTAssertNil(slot(room: "   \n").displayRoom)
    }

    func testRoomIsTrimmed() {
        XCTAssertEqual(slot(room: "  Sala 5 ").displayRoom, "Sala 5")
    }
}
