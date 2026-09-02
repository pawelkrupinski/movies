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

    private func film(_ title: String, _ r: Film.Ratings = .empty,
                      showings: [DayShowings] = []) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: nil, releaseYear: nil, genres: [], ratings: r,
             countries: [], directors: [], cast: [], showings: showings)
    }

    /// `<date> at <cinema>: <times>` in one line, so a sort case reads as the
    /// schedule it is asserting about.
    private func day(_ date: String, _ cinemas: (String, [String])...) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas.map { name, times in
            CinemaShowings(cinema: name, cinemaURL: nil,
                           showtimes: times.map { Showtime(time: $0, format: "", room: nil, bookingURL: nil) })
        })
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

    func testEarliestSortIsStableForFilmsWithNoShowings() {
        // Nothing to rank on — both carry the same sentinel key, so the input
        // order survives (Swift's `sorted` is not itself stable).
        let films = [
            film("First", ratings(imdb: 1.0)),
            film("Second", ratings(imdb: 9.0)),
        ]
        XCTAssertEqual(films.sorted(by: .earliest).map(\.title), ["First", "Second"])
    }

    func testEarliestSortOrdersBySoonestShowing() {
        let films = [
            film("Evening", showings: [day("2026-05-22", ("X", ["18:00"]))]),
            film("Morning", showings: [day("2026-05-22", ("X", ["09:00"]))]),
            film("Yesterday", showings: [day("2026-05-21", ("X", ["23:00"]))]),
        ]
        XCTAssertEqual(films.sorted(by: .earliest).map(\.title),
                       ["Yesterday", "Morning", "Evening"])
    }

    func testEarliestSortLooksAcrossEveryDayAndCinema() {
        let films = [
            film("Late", showings: [day("2026-05-22", ("X", ["20:00"]))]),
            // The soonest slot is buried in the second cinema-group, and its
            // own group lists a later slot first.
            film("EarlyBuried", showings: [
                day("2026-05-23", ("X", ["21:00"])),
                day("2026-05-22", ("X", ["21:00"]), ("Y", ["22:00", "08:30"])),
            ]),
        ]
        XCTAssertEqual(films.sorted(by: .earliest).map(\.title), ["EarlyBuried", "Late"])
    }

    func testEarliestSortRanksTheDayPageByThatDaysShowings() {
        // The regression this sort exists for. The server ranks the payload by
        // each film's earliest showtime across the WHOLE schedule, so "Opener"
        // (today 10:00) arrives first. On the Tomorrow page it plays at 22:00
        // and "Sleeper" at 09:00 — the day page has to re-rank, or it shows the
        // global order and the cards read as unsorted.
        let payloadOrder = [
            film("Opener", showings: [
                day("2026-05-22", ("X", ["10:00"])),
                day("2026-05-23", ("X", ["22:00"])),
            ]),
            film("Sleeper", showings: [day("2026-05-23", ("X", ["09:00"]))]),
        ]
        XCTAssertEqual(payloadOrder.sorted(by: .earliest).map(\.title), ["Opener", "Sleeper"])

        let tomorrow = payloadOrder.filteredFor(
            date: .specific("2026-05-23"), format: .empty, query: "", hidden: []
        )
        XCTAssertEqual(tomorrow.sorted(by: .earliest).map(\.title), ["Sleeper", "Opener"])
    }

    func testEarliestSortFollowsTheFromHourFilter() {
        // Same shape one axis over: a from-hour bound hides the slot the server
        // ranked "Matinee" by, so the visible order flips.
        let films = [
            film("Matinee", showings: [day("2026-05-22", ("X", ["09:00", "23:00"]))]),
            film("Primetime", showings: [day("2026-05-22", ("X", ["20:00"]))]),
        ]
        XCTAssertEqual(films.sorted(by: .earliest).map(\.title), ["Matinee", "Primetime"])

        let fromEight = films.filteredFor(
            date: .anytime, format: FormatFilter(fromHour: 20), query: "", hidden: []
        )
        XCTAssertEqual(fromEight.sorted(by: .earliest).map(\.title), ["Primetime", "Matinee"])
    }

    func testRatingSortTieBreaksOnEarliestShowingBeforeInputOrder() {
        // `compareCards` falls through rating → earliest → input order; equal
        // ratings must not leave a later film above a sooner one.
        let films = [
            film("Later", ratings(imdb: 7.0), showings: [day("2026-05-22", ("X", ["20:00"]))]),
            film("Sooner", ratings(imdb: 7.0), showings: [day("2026-05-22", ("X", ["10:00"]))]),
        ]
        XCTAssertEqual(films.sorted(by: .rating).map(\.title), ["Sooner", "Later"])
    }
}
