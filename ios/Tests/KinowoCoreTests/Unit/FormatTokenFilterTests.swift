import XCTest
@testable import KinowoCore

final class FormatTokenFilterTests: XCTestCase {

    // MARK: – tokensToStrip

    func testAllShowtimesSameFormatReturnsFullSet() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "2D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), ["2D", "NAP"])
    }

    func testDifferentFormatsReturnOnlySharedTokens() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "IMAX 3D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), ["NAP"])
    }

    func testNoCommonTokensReturnsEmpty() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "IMAX 3D", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), [])
    }

    func testSingleShowtimeFormatsAreAllCommon() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), ["2D", "NAP"])
    }

    func testEmptyFormatShowtimesAreIgnored() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "", room: nil, bookingURL: nil),
            Showtime(time: "22:00", format: "2D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), ["2D", "NAP"])
    }

    func testAllFormatsEmptyReturnsEmpty() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), [])
    }

    // MARK: – what a pill is left showing
    //
    // The whole rendering decision: what a pill drops, and therefore what is
    // left to read. `ShowingsView` is a SwiftUI file and excluded from this
    // target, so this is the closest reachable mechanism to the pill itself —
    // the view does nothing with the answer but hand it to `filter`.

    func testAVersionEverySlotSharesIsDroppedLikeAnyOtherToken() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D DUB", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        let common = FormatTokenFilter.tokensToStrip(cinema)
        XCTAssertEqual(common, ["2D", "DUB"])
        // Six pills all saying DUB tell a visitor as little as six all saying
        // 2D — the cinema screens the film no other way.
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: common), "")
    }

    func testASharedScreenFormatIsDropped() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "IMAX NAP", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "IMAX 3D NAP", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), ["IMAX", "NAP"])
        XCTAssertEqual(FormatTokenFilter.filter("IMAX 3D NAP", removing: ["IMAX", "NAP"]), "3D")
    }

    func testAVersionThatDiffersBetweenSlotsStaysOnEveryPill() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        let common = FormatTokenFilter.tokensToStrip(cinema)
        XCTAssertEqual(FormatTokenFilter.filter("2D NAP", removing: common), "NAP")
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: common), "DUB")
    }

    func testNothingIsStrippedWhenNothingIsCommon() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "IMAX 3D", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema), [])
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
