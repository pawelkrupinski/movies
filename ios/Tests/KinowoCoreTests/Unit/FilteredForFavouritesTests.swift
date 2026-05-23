import XCTest
@testable import KinowoCore

final class FilteredForFavouritesTests: XCTestCase {

    private func slot(_ time: String, _ format: String = "2D NAP") -> Showtime {
        Showtime(time: time, format: format, room: nil, bookingURL: nil)
    }

    private func cinema(_ name: String, _ times: [Showtime]) -> CinemaShowings {
        CinemaShowings(cinema: name, cinemaURL: nil, showtimes: times)
    }

    private func day(_ date: String, _ cinemas: [CinemaShowings]) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas)
    }

    private func film(_ title: String, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 100, ratings: .empty, showings: days)
    }

    private let today: String    = DateFilter.iso(Date())
    private let tomorrow: String = DateFilter.iso(Date().addingTimeInterval(86_400))

    private func fixture() -> [Film] {
        let mandalorian = film("Mandalorian and Grogu", [
            day(today, [
                cinema("Helonki", [slot("17:00"), slot("20:00", "3D NAP")]),
                cinema("Apollo",  [slot("19:30")]),
            ]),
            day(tomorrow, [
                cinema("Muza", [slot("16:00")]),
            ]),
        ])
        let title2 = film("Title2", [
            day(today,    [cinema("Apollo",  [slot("18:00")])]),
            day(tomorrow, [cinema("Helonki", [slot("21:00")])]),
        ])
        let title3 = film("Title3", [
            day(tomorrow, [cinema("Helonki", [slot("22:00")])]),
        ])
        return [mandalorian, title2, title3]
    }

    // ── Empty input ───────────────────────────────────────────────

    func testNoFavouritesReturnsEmpty() {
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "",
            favouriteMovies: [], favouriteScreenings: [],
            disabledCinemas: []
        )
        XCTAssertTrue(result.isEmpty,
                      "no favourites of either kind should produce zero films")
    }

    // ── Whole-movie favourites ────────────────────────────────────

    func testWholeMovieFavouriteKeepsAllScreenings() {
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "",
            favouriteMovies: ["Mandalorian and Grogu"],
            favouriteScreenings: [],
            disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])
        let mando = result[0]
        XCTAssertEqual(mando.showings.count, 2,
                       "both days kept for a whole-movie favourite")
        let allTimes = mando.showings.flatMap { $0.cinemas }
            .flatMap { $0.showtimes }
            .map(\.time)
            .sorted()
        XCTAssertEqual(allTimes, ["16:00", "17:00", "19:30", "20:00"])
    }

    // ── Per-screening favourites ──────────────────────────────────

    func testPerScreeningFavouriteKeepsOnlyThatScreening() {
        let id = ScreeningId.make(
            title: "Mandalorian and Grogu", cinema: "Apollo",
            date: today, time: "19:30"
        )
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "",
            favouriteMovies: [],
            favouriteScreenings: [id],
            disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])
        let allSlots = result[0].showings
            .flatMap { $0.cinemas }
            .flatMap { $0.showtimes }
        XCTAssertEqual(allSlots.map(\.time), ["19:30"],
                       "only the favourited 19:30 slot should remain")
        XCTAssertEqual(result[0].showings[0].cinemas.map(\.cinema), ["Apollo"])
    }

    func testWholeMovieFavouriteOverridesScreeningSubset() {
        // Whole-movie favourite + a per-screening favourite both set —
        // the whole-movie flag wins; every screening stays.
        let id = ScreeningId.make(
            title: "Mandalorian and Grogu", cinema: "Apollo",
            date: today, time: "19:30"
        )
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "",
            favouriteMovies: ["Mandalorian and Grogu"],
            favouriteScreenings: [id],
            disabledCinemas: []
        )
        let count = result[0].showings.flatMap { $0.cinemas }.flatMap { $0.showtimes }.count
        XCTAssertEqual(count, 4, "whole-movie favourite should keep all 4 slots")
    }

    // ── Date filter on top ────────────────────────────────────────

    func testDateFilterNarrowsAFavouriteToToday() {
        let result = fixture().filteredForFavourites(
            date: .today, format: .empty, query: "",
            favouriteMovies: ["Mandalorian and Grogu"],
            favouriteScreenings: [],
            disabledCinemas: []
        )
        XCTAssertEqual(result.count, 1)
        XCTAssertEqual(result[0].showings.map(\.date), [today])
    }

    func testDateFilterCanEmptyAFavouriteOut() {
        // Title3 only plays tomorrow. Filter to today → no result,
        // even though it's a favourite. This is the empty-state
        // condition the UI distinguishes "no favourites at all" vs
        // "no favourites on this day".
        let result = fixture().filteredForFavourites(
            date: .today, format: .empty, query: "",
            favouriteMovies: ["Title3"],
            favouriteScreenings: [],
            disabledCinemas: []
        )
        XCTAssertTrue(result.isEmpty)
    }

    // ── Other axes still apply ────────────────────────────────────

    func testQueryFurtherNarrowsFavourites() {
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "title2",
            favouriteMovies: ["Mandalorian and Grogu", "Title2"],
            favouriteScreenings: [],
            disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title), ["Title2"])
    }

    func testFormatFilterDropsNonMatchingScreenings() {
        let only3D = FormatFilter(dimension: "3D", language: "", imax: false, fromHour: -1)
        let result = fixture().filteredForFavourites(
            date: .anytime, format: only3D, query: "",
            favouriteMovies: ["Mandalorian and Grogu"],
            favouriteScreenings: [],
            disabledCinemas: []
        )
        let times = result[0].showings.flatMap { $0.cinemas }.flatMap { $0.showtimes }.map(\.time)
        XCTAssertEqual(times, ["20:00"],
                       "only the 3D NAP slot should pass a 3D-only format filter")
    }

    func testDisabledCinemaDropsItsScreeningsEvenForFavourites() {
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "",
            favouriteMovies: ["Mandalorian and Grogu"],
            favouriteScreenings: [],
            disabledCinemas: ["Helonki"]
        )
        let cinemasSeen = Set(result.flatMap { $0.showings }.flatMap { $0.cinemas }.map(\.cinema))
        XCTAssertFalse(cinemasSeen.contains("Helonki"),
                       "disabled cinema must drop from the favourites view too")
        XCTAssertTrue(cinemasSeen.contains("Apollo"))
    }

    // ── Hidden films do NOT mask favourites ───────────────────────

    func testHiddenFilmsAreNotFilteredOnFavouritesView() {
        // The hidden-films set isn't an input to
        // filteredForFavourites — favouriting a film explicitly
        // overrides any prior dismissal. This test pins that
        // behaviour so a future "hide me" feature doesn't regress
        // the favourites view.
        let result = fixture().filteredForFavourites(
            date: .anytime, format: .empty, query: "",
            favouriteMovies: ["Title2"],
            favouriteScreenings: [],
            disabledCinemas: []
        )
        XCTAssertEqual(result.map(\.title), ["Title2"])
    }
}
