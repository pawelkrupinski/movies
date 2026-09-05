import XCTest
@testable import KinowoCore

final class FormatTokenFilterTests: XCTestCase {

    private func day(_ date: String, _ cinemas: [CinemaShowings]) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas)
    }

    private func cinema(_ name: String, _ formats: [String]) -> CinemaShowings {
        CinemaShowings(cinema: name, cinemaURL: nil, showtimes: formats.enumerated().map { i, f in
            Showtime(time: "1\(i):00", format: f, room: nil, bookingURL: nil)
        })
    }

    // MARK: – tokensToStrip

    func testEverySlotSharingAFormatMakesItStrippable() {
        let days = [day("2026-09-05", [cinema("Kino A", ["2D NAP", "2D NAP"])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), ["2D", "NAP"])
    }

    func testDifferentFormatsLeaveOnlySharedTokens() {
        let days = [day("2026-09-05", [cinema("Kino A", ["2D NAP", "IMAX 3D NAP"])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), ["NAP"])
    }

    func testNoCommonTokensReturnsEmpty() {
        let days = [day("2026-09-05", [cinema("Kino A", ["2D", "IMAX 3D"])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), [])
    }

    func testSingleShowtimeFormatsAreAllCommon() {
        let days = [day("2026-09-05", [cinema("Kino A", ["2D NAP"])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), ["2D", "NAP"])
    }

    func testEmptyFormatShowtimesAreIgnored() {
        let days = [day("2026-09-05", [cinema("Kino A", ["2D NAP", "", "2D NAP"])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), ["2D", "NAP"])
    }

    func testAllFormatsEmptyReturnsEmpty() {
        let days = [day("2026-09-05", [cinema("Kino A", ["", ""])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), [])
    }

    // MARK: – what a pill is left showing
    //
    // The whole rendering decision: what a pill drops, and therefore what is
    // left to read. `ShowingsView` is a SwiftUI file and excluded from this
    // target, so this is the closest reachable mechanism to the pill itself —
    // the view does nothing with the answer but hand it to `filter`.

    func testAVersionTheWHOLECardSharesIsDroppedLikeAnyOtherToken() {
        let days = [day("2026-09-05", [cinema("Multikino", ["2D DUB", "2D DUB"])])]
        let strip = FormatTokenFilter.tokensToStrip(days)
        XCTAssertEqual(strip, ["2D", "DUB"])
        // Six pills all saying DUB tell a visitor as little as six all saying
        // 2D — the film screens no other way here.
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "")
    }

    func testTwoCINEMASThatDisagreeKeepTheVersionOnBOTH() {
        // Neither cinema is mixed on its own; the FILM is. That difference is
        // the whole reason a visitor reads the tag, so it stays on every pill.
        let days = [day("2026-09-05", [
            cinema("Multikino", ["2D DUB", "2D DUB"]),
            cinema("Helios",    ["2D NAP", "2D NAP"]),
        ])]
        let strip = FormatTokenFilter.tokensToStrip(days)
        XCTAssertEqual(strip, ["2D"])
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "DUB")
        XCTAssertEqual(FormatTokenFilter.filter("2D NAP", removing: strip), "NAP")
    }

    func testTwoDAYSThatDisagreeKeepTheVersionOnBOTH() {
        // Same cinema, subtitled today and dubbed tomorrow: uniform within each
        // day, mixed across the card.
        let days = [
            day("2026-09-05", [cinema("Multikino", ["2D NAP", "2D NAP"])]),
            day("2026-09-06", [cinema("Multikino", ["2D DUB", "2D DUB"])]),
        ]
        let strip = FormatTokenFilter.tokensToStrip(days)
        XCTAssertEqual(strip, ["2D"])
        XCTAssertEqual(FormatTokenFilter.filter("2D NAP", removing: strip), "NAP")
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "DUB")
    }

    func testASharedScreenFormatIsDropped() {
        let days = [day("2026-09-05", [cinema("Kino A", ["IMAX NAP", "IMAX 3D NAP"])])]
        XCTAssertEqual(FormatTokenFilter.tokensToStrip(days), ["IMAX", "NAP"])
        XCTAssertEqual(FormatTokenFilter.filter("IMAX 3D NAP", removing: ["IMAX", "NAP"]), "3D")
    }

    func testAVersionThatDiffersBetweenSlotsStaysOnEveryPill() {
        let days = [day("2026-09-05", [cinema("Multikino", ["2D NAP", "2D DUB"])])]
        let strip = FormatTokenFilter.tokensToStrip(days)
        XCTAssertEqual(FormatTokenFilter.filter("2D NAP", removing: strip), "NAP")
        XCTAssertEqual(FormatTokenFilter.filter("2D DUB", removing: strip), "DUB")
    }

    // MARK: – filter

    func testFilterRemovesCommonTokens() {
        XCTAssertEqual(FormatTokenFilter.filter("IMAX 3D NAP", removing: ["NAP"]), "IMAX 3D")
    }

    func testFilterRemovesAllTokensLeavingEmpty() {
        XCTAssertEqual(FormatTokenFilter.filter("2D NAP", removing: ["2D", "NAP"]), "")
    }

    func testFilterWithEmptyCommonSetReturnsOriginal() {
        XCTAssertEqual(FormatTokenFilter.filter("IMAX 3D", removing: []), "IMAX 3D")
    }

    func testFilterOnEmptyFormatReturnsEmpty() {
        XCTAssertEqual(FormatTokenFilter.filter("", removing: ["2D"]), "")
    }
}
