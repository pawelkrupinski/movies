import XCTest
@testable import KinowoCore

final class ShowingsParserTests: XCTestCase {

    private func firstFilmChunk() throws -> String {
        let html = try Fixtures.load("home")
        let anchor = "<div class=\"col\" data-title=\""
        let starts = HTMLPrimitives.ranges(of: anchor, in: html)
        guard starts.count >= 2 else {
            XCTFail("expected at least two film cards in home fixture")
            throw NSError(domain: "Fixture", code: 1)
        }
        return String(html[starts[0]..<starts[1]])
    }

    func testParsesShowingsFromFirstFilmCard() throws {
        let chunk = try firstFilmChunk()
        let days = ShowingsParser.parseShowings(in: chunk)
        XCTAssertFalse(days.isEmpty, "expected the first card to advertise at least one date")
        XCTAssertFalse(days[0].cinemas.isEmpty, "expected at least one cinema for day 0")
    }

    func testDatesAscendInIsoOrder() throws {
        let chunk = try firstFilmChunk()
        let days = ShowingsParser.parseShowings(in: chunk)
        let dates = days.map { $0.date }
        XCTAssertEqual(dates, dates.sorted(), "dates not in ascending order: \(dates)")
    }

    func testEveryShowtimeHasHHMMShape() throws {
        let chunk = try firstFilmChunk()
        let days = ShowingsParser.parseShowings(in: chunk)
        let regex = try NSRegularExpression(pattern: #"^\d{2}:\d{2}$"#)
        var count = 0
        for day in days {
            for cinema in day.cinemas {
                for show in cinema.showtimes {
                    let ns = show.time as NSString
                    let match = regex.firstMatch(in: show.time, range: NSRange(location: 0, length: ns.length))
                    XCTAssertNotNil(match, "bad time format: \(show.time)")
                    count += 1
                }
            }
        }
        XCTAssertGreaterThan(count, 0)
    }

    func testCinemaNamesAreNonEmpty() throws {
        let chunk = try firstFilmChunk()
        let days = ShowingsParser.parseShowings(in: chunk)
        for day in days {
            for cinema in day.cinemas {
                XCTAssertFalse(cinema.cinema.isEmpty, "empty cinema name in \(day.date)")
                XCTAssertFalse(cinema.cinema.contains("&amp;"), "cinema name should be html-decoded: \(cinema.cinema)")
            }
        }
    }

    func testFormatOrRoomAppearAtLeastOnce() throws {
        // Across all films, at least some showtimes should carry format or
        // room metadata — even if the first card doesn't.
        let html = try Fixtures.load("home")
        let days = ShowingsParser.parseShowings(in: html)
        var formats = 0
        var rooms = 0
        for day in days {
            for cinema in day.cinemas {
                for show in cinema.showtimes {
                    if !show.format.isEmpty { formats += 1 }
                    if show.room != nil { rooms += 1 }
                }
            }
        }
        XCTAssertGreaterThan(formats + rooms, 0, "expected at least one showtime with format or room metadata")
    }

    /// The `<a class="badge-time" href=…>` pill carries the cinema's
    /// booking URL even when it also has a room — that's the link the iOS
    /// pill opens on tap. A `<span class="badge-time">` (no href) is the
    /// web's non-bookable fallback and must decode to `bookingURL == nil`.
    func testParsesBookingURLAndRoomFromAnchorPill() {
        let chunk = """
        <div class="date-group" data-date="2026-06-02">
          <div class="date-label">Wtorek</div>
          <div class="cinema-group" data-cinema="Multikino">
            <a href="https://multikino.pl/film/abc?showing=123&amp;date=2026-06-02" class="badge-time" target="_blank" data-room="Sala 9" data-format="2D NAP" data-time="18:30">18:30</a>
            <span class="badge-time" data-format="2D" data-time="20:00">20:00</span>
          </div>
        </div>
        """
        let days = ShowingsParser.parseShowings(in: chunk)
        let showtimes = try! XCTUnwrap(days.first?.cinemas.first?.showtimes)
        XCTAssertEqual(showtimes.count, 2)

        let bookable = showtimes[0]
        XCTAssertEqual(bookable.time, "18:30")
        XCTAssertEqual(bookable.room, "Sala 9")
        XCTAssertEqual(
            bookable.bookingURL,
            URL(string: "https://multikino.pl/film/abc?showing=123&date=2026-06-02"),
            "the anchor's href (html-decoded) is the booking link the pill opens on tap"
        )

        let nonBookable = showtimes[1]
        XCTAssertEqual(nonBookable.time, "20:00")
        XCTAssertNil(nonBookable.bookingURL, "a <span> pill has no href, so it is non-bookable")
    }

    func testReturnsEmptyForUnrelatedHTML() {
        let days = ShowingsParser.parseShowings(in: "<p>nothing to see</p>")
        XCTAssertTrue(days.isEmpty)
    }
}
