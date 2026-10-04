import XCTest
@testable import KinowoCore

/// Decodes a US `/api/repertoire` exactly as the server writes it —
/// `api_repertoire_us.json` is rendered and kept current by the web's
/// `ApiRepertoireUsWireSpec` — through `[Film]`. It carries what a Polish
/// listing never does: an age-rating certificate, English day labels, US
/// rating sites, no Filmweb (a Polish site), and showtimes the US web prints on a 12-hour clock but the
/// API must keep as `HH:mm` (the sort key, the from-hour filter and pruning
/// all read it).
final class UsRepertoireWireDecodingTests: XCTestCase {

    private func film() throws -> Film {
        let films = try JSONDecoder().decode([Film].self, from: Data(try Fixtures.load("api_repertoire_us").utf8))
        XCTAssertEqual(films.count, 1)
        return try XCTUnwrap(films.first)
    }

    func testDecodesTheNonPolishFields() throws {
        let film = try film()
        XCTAssertEqual(film.ageRating, "PG-13")
        XCTAssertEqual(film.showings.map(\.label), ["Wednesday 10 June", "Thursday 11 June"])
        XCTAssertEqual(film.ratings.imdb, 7.4)
        XCTAssertEqual(film.ratings.metascore, 68)
        XCTAssertEqual(film.ratings.rottenTomatoes, 91)
        // Filmweb is Polish: no score and no link, so `RatingBadgesView` draws no FW pill.
        XCTAssertNil(film.ratings.filmweb)
        XCTAssertNil(film.ratings.filmwebURL)
        XCTAssertNil(film.posterURL)
    }

    func testShowtimesStayTwentyFourHourWithTheirOptionalFields() throws {
        let slots = try film().showings.flatMap(\.cinemas).flatMap(\.showtimes)
        XCTAssertEqual(slots.map(\.time), ["19:30", "00:05", "12:30"])
        XCTAssertEqual(slots.map(\.room), ["Theater 4", nil, nil])
        XCTAssertEqual(slots.map { $0.bookingURL?.absoluteString },
                       ["https://tickets.example.com/b/1930", "https://tickets.example.com/b/0005", nil])
    }
}
