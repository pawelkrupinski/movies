import XCTest
@testable import KinowoCore

/// `Ratings.weightedRating` (mirrors the server's `MovieRecord.weightedRating`)
/// and `[Film].sorted(by:)` — the two-axis sort behind the Filtry "Sortowanie"
/// control, matching the web's `compareCards`.
final class SortOptionTests: XCTestCase {

    private func ratings(imdb: Double? = nil, metascore: Int? = nil,
                         rt: Int? = nil, filmweb: Double? = nil) -> Film.Ratings {
        Film.Ratings(
            imdb: imdb, imdbURL: nil,
            metascore: metascore, metacriticURL: nil,
            rottenTomatoes: rt, rottenTomatoesURL: nil,
            filmweb: filmweb, filmwebURL: nil
        )
    }

    private func film(_ title: String, _ r: Film.Ratings) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: nil, releaseYear: nil, genres: [], ratings: r,
             countries: [], directors: [], cast: [], showings: [])
    }

    // MARK: – weightedRating

    func testWeightedRatingNormalisesEachSourceToTenAndAverages() {
        // imdb 8.0, filmweb 7.0, metascore 60→6.0, rt 90→9.0 → mean of [8,7,6,9] = 7.5
        XCTAssertEqual(ratings(imdb: 8.0, metascore: 60, rt: 90, filmweb: 7.0).weightedRating,
                       7.5, accuracy: 1e-9)
    }

    func testWeightedRatingSkipsMissingSources() {
        // only imdb 8.0 and rt 60→6.0 → mean = 7.0
        XCTAssertEqual(ratings(imdb: 8.0, rt: 60).weightedRating, 7.0, accuracy: 1e-9)
    }

    func testWeightedRatingZeroWhenNoRatings() {
        XCTAssertEqual(Film.Ratings.empty.weightedRating, 0, accuracy: 1e-9)
    }

    // MARK: – sorted(by:)

    func testRatingSortIsDescending() {
        let films = [
            film("Low", ratings(imdb: 5.0)),
            film("High", ratings(imdb: 9.0)),
            film("Mid", ratings(imdb: 7.0)),
        ]
        XCTAssertEqual(films.sorted(by: .rating).map(\.title), ["High", "Mid", "Low"])
    }

    func testRatingSortTieBreaksOnInputOrder() {
        // Equal ratings keep the incoming (earliest-showtime) order.
        let films = [
            film("A", ratings(imdb: 7.0)),
            film("B", ratings(imdb: 7.0)),
            film("C", ratings(imdb: 9.0)),
        ]
        XCTAssertEqual(films.sorted(by: .rating).map(\.title), ["C", "A", "B"])
    }

    func testRatingSortPutsUnratedLast() {
        let films = [
            film("Unrated", .empty),
            film("Rated", ratings(imdb: 6.0)),
        ]
        XCTAssertEqual(films.sorted(by: .rating).map(\.title), ["Rated", "Unrated"])
    }

    func testEarliestSortPreservesInputOrder() {
        let films = [
            film("First", ratings(imdb: 1.0)),
            film("Second", ratings(imdb: 9.0)),
        ]
        XCTAssertEqual(films.sorted(by: .earliest).map(\.title), ["First", "Second"])
    }
}
