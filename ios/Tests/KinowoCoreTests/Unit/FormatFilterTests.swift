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

    func testLanguageConstraintOnAnotherCountrysToken() {
        // The picker now offers the selected country's own pair, so a German
        // user's "subtitles" is `OmU` — and it must match German showtimes, not
        // Poland's `NAP` which no German screening carries.
        var f = FormatFilter()
        f.language = "OmU"
        XCTAssertTrue(f.matches(showtime: slot("18:00", "2D OmU")))
        XCTAssertFalse(f.matches(showtime: slot("18:00", "2D DF")))
        XCTAssertFalse(f.matches(showtime: slot("18:00", "2D NAP")))
    }

    func testImaxRequiresImaxToken() {
        var f = FormatFilter()
        f.imax = true
        XCTAssertTrue(f.matches(showtime: slot("20:00", "IMAX 3D")))
        XCTAssertFalse(f.matches(showtime: slot("20:00", "3D NAP")))
    }

    private func film(_ formatsByDay: [String]...) -> Film {
        Film(title: "Film", posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 90, releaseYear: nil, genres: [], ratings: .empty,
             countries: [], directors: [], cast: [],
             showings: formatsByDay.enumerated().map { i, formats in
                 DayShowings(date: "2026-05-2\(i)", label: "", cinemas: [
                     CinemaShowings(cinema: "Kino", cinemaURL: nil,
                                    showtimes: formats.map { slot("18:00", $0) })
                 ])
             })
    }

    // The Filtry sheet offers "IMAX only" just where a showtime on ANY loaded
    // day carries the token — elsewhere it could only blank the list.
    func testHasImaxShowtimeLooksAtEveryDay() {
        XCTAssertFalse([Film]().hasImaxShowtime)
        XCTAssertFalse([film(["2D NAP"], ["3D DUB"])].hasImaxShowtime)
        XCTAssertTrue([film(["2D NAP"]), film(["2D"], ["IMAX 3D"])].hasImaxShowtime)
    }

    // An IMAX pick carried into a city without IMAX (city switch, deep link)
    // is dropped rather than blanking a list whose sheet has no toggle to undo it.
    func testApplicableDropsImaxWhereNoShowtimeHasIt() {
        var f = FormatFilter()
        f.imax = true
        f.dimension = "3D"
        let without = f.applicable(to: [film(["3D NAP"])])
        XCTAssertFalse(without.imax)
        XCTAssertEqual(without.dimension, "3D")
        XCTAssertEqual(f.applicable(to: [film(["IMAX 3D"])]), f)
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
