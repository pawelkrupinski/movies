import XCTest
@testable import KinowoCore

final class CommonFormatTokensTests: XCTestCase {

    // MARK: – commonTokens

    func testAllShowtimesSameFormatReturnsFullSet() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "2D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonTokens(cinema), ["2D", "NAP"])
    }

    func testDifferentFormatsReturnOnlySharedTokens() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "IMAX 3D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonTokens(cinema), ["NAP"])
    }

    func testNoCommonTokensReturnsEmpty() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "IMAX 3D", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonTokens(cinema), [])
    }

    func testSingleShowtimeFormatsAreAllCommon() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonTokens(cinema), ["2D", "NAP"])
    }

    func testEmptyFormatShowtimesAreIgnored() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "", room: nil, bookingURL: nil),
            Showtime(time: "22:00", format: "2D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonTokens(cinema), ["2D", "NAP"])
    }

    func testAllFormatsEmptyReturnsEmpty() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonTokens(cinema), [])
    }

    // MARK: – filter

    func testFilterRemovesCommonTokens() {
        let result = FormatTokenFilter.filter("IMAX 3D NAP", removing: ["NAP"])
        XCTAssertEqual(result, "IMAX 3D")
    }

    func testFilterRemovesAllTokensLeavingEmpty() {
        let result = FormatTokenFilter.filter("2D NAP", removing: ["2D", "NAP"])
        XCTAssertEqual(result, "")
    }

    func testFilterWithEmptyCommonSetReturnsOriginal() {
        let result = FormatTokenFilter.filter("IMAX 3D", removing: [])
        XCTAssertEqual(result, "IMAX 3D")
    }

    func testFilterOnEmptyFormatReturnsEmpty() {
        let result = FormatTokenFilter.filter("", removing: ["2D"])
        XCTAssertEqual(result, "")
    }
}
