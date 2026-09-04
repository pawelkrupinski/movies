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

    // MARK: – commonVersion
    //
    // The version a whole cinema shares is what the LABEL says, so it is not
    // simply dropped with the rest of the intersection. This is the napisy-vs-
    // dubbing bug: every slot tagged `2D DUB` left no `DUB` anywhere on screen.

    func testCommonVersionIsTheSharedLanguageToken() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D DUB", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonVersion(cinema), ["DUB"])
    }

    func testCommonVersionKeepsTheSourceOrderAndDropsScreenFormat() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "IMAX NAP ATMOS", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "IMAX NAP ATMOS", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonVersion(cinema), ["NAP"])
    }

    func testCommonVersionIsEmptyWhenTheVersionDiffers() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonVersion(cinema), [])
    }

    func testCommonVersionIsEmptyWhenNothingIsCommon() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "18:00", format: "2D", room: nil, bookingURL: nil),
            Showtime(time: "20:00", format: "IMAX 3D", room: nil, bookingURL: nil),
        ])
        XCTAssertEqual(FormatTokenFilter.commonVersion(cinema), [])
    }

    // MARK: – tokensToStrip
    //
    // The whole rendering decision: what a pill drops, and therefore what is
    // left to read. `ShowingsView` is a SwiftUI file and excluded from this
    // target, so this is the closest reachable mechanism to the pill itself —
    // the view does nothing with the answer but hand it to `filter`.

    func testWithACinemaLabelTheSharedVersionLeavesThePill() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D DUB", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        let strip = FormatTokenFilter.tokensToStrip(cinema, hasLabel: true)
        XCTAssertEqual(strip, ["2D", "DUB"])
        // Nothing on the pill — the label says DUB once instead.
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "")
        XCTAssertEqual(FormatTokenFilter.commonVersion(cinema), ["DUB"])
    }

    func testWithNoCinemaLabelTheSharedVersionStaysOnThePill() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D DUB", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        let strip = FormatTokenFilter.tokensToStrip(cinema, hasLabel: false)
        XCTAssertEqual(strip, ["2D"])
        // The Kina tab has no per-card cinema label, so the pill is the only
        // place the version can live — and "DUB" is narrower than the "2D DUB"
        // the two-per-row guarantee is measured against.
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "DUB")
    }

    func testASharedScreenFormatIsStillDroppedWithNoLabel() {
        let cinema = CinemaShowings(cinema: "Kino A", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D", room: nil, bookingURL: nil),
        ])
        // "2D" on every pill tells a visitor nothing — that is what the
        // stripping is FOR, and it keeps working with or without a label.
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(cinema, hasLabel: false), ["2D"])
    }

    func testAVersionThatDiffersBetweenSlotsStaysOnEveryPill() {
        let cinema = CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
            Showtime(time: "14:30", format: "2D NAP", room: nil, bookingURL: nil),
            Showtime(time: "17:00", format: "2D DUB", room: nil, bookingURL: nil),
        ])
        let strip = FormatTokenFilter.tokensToStrip(cinema, hasLabel: true)
        XCTAssertEqual(FormatTokenFilter.filter("2D NAP", removing: strip), "NAP")
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "DUB")
    }

    // MARK: – isLanguageVersion

    func testEveryMarketsVersionSpellingCounts() {
        for token in ["NAP", "DUB", "LEK", "LEC", "ORG", "SUB",
                      "VO", "VOSE", "VOSI", "DOB", "CAT", "OV", "OmU", "OmeU", "DF"] {
            XCTAssertTrue(FormatTokenFilter.isLanguageVersion(token), "\(token) is a version")
        }
        // An audio language a source names is a version too — at a UK multiplex
        // "Hindi" is the whole difference between two screenings of one film.
        XCTAssertTrue(FormatTokenFilter.isLanguageVersion("HINDI"))
    }

    func testScreenFormatAndAccessibilityAreNotVersions() {
        for token in ["2D", "3D", "IMAX", "4DX", "SCREENX", "ATMOS", "DOLBY",
                      "LASER", "70MM", "VIP", "PREMIUM", "AD", "OC"] {
            XCTAssertFalse(FormatTokenFilter.isLanguageVersion(token), "\(token) is not a version")
        }
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
