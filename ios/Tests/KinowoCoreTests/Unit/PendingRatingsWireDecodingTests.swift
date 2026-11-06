import XCTest
@testable import KinowoCore

/// A film whose IMDb id and Filmweb page are known but neither has a rating yet, as
/// `/api/repertoire` writes it (`api_repertoire_pending.json`, kept current by the web's
/// `ApiRatingPendingSpec`). The payload only ADDS the optional `filmwebRatingPending`
/// key, so builds that predate it decode it unchanged.
final class PendingRatingsWireDecodingTests: XCTestCase {

    private func ratings() throws -> Film.Ratings {
        let films = try JSONDecoder().decode([Film].self, from: Data(try Fixtures.load("api_repertoire_pending").utf8))
        return try XCTUnwrap(films.first).ratings
    }

    func testDecodesTheLinksWithoutScores() throws {
        let ratings = try ratings()
        XCTAssertNil(ratings.imdb)
        XCTAssertNil(ratings.filmweb)
        XCTAssertEqual(ratings.imdbURL?.absoluteString, "https://www.imdb.com/title/tt7654321/")
        XCTAssertEqual(ratings.filmwebURL?.absoluteString, "https://www.filmweb.pl/film/Przepis+na+swieta-2026-1")
    }

    func testFlagsBothSourcesAsPendingSoTheCardDrawsTheirPills() throws {
        let ratings = try ratings()
        XCTAssertEqual(ratings.filmwebRatingPending, true)
        XCTAssertTrue(ratings.imdbPending)
        XCTAssertTrue(ratings.filmwebPending)
        XCTAssertFalse(ratings.isEmpty, "the ratings row must render for rating-less pills")
    }

    func testAFilmwebSearchLinkIsNotPending() {
        let search = URL(string: "https://www.filmweb.pl/search?query=X")
        let ratings = Film.Ratings(imdb: nil, imdbURL: nil, metascore: nil, metacriticURL: nil,
                                   rottenTomatoes: nil, rottenTomatoesURL: nil,
                                   filmweb: nil, filmwebURL: search)
        XCTAssertFalse(ratings.filmwebPending)
        XCTAssertFalse(ratings.imdbPending)
        XCTAssertTrue(ratings.isEmpty)
    }

    func testARatedSourceIsNotPending() {
        let page = URL(string: "https://www.filmweb.pl/film/X-1")
        let ratings = Film.Ratings(imdb: 7.0, imdbURL: URL(string: "https://www.imdb.com/title/tt1/"),
                                   metascore: nil, metacriticURL: nil, rottenTomatoes: nil, rottenTomatoesURL: nil,
                                   filmweb: 6.5, filmwebURL: page, filmwebRatingPending: true)
        XCTAssertFalse(ratings.imdbPending)
        XCTAssertFalse(ratings.filmwebPending)
    }
}
