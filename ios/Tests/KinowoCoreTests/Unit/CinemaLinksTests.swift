import XCTest
@testable import KinowoCore

/// `[DayShowings].cinemaLinks()` replaces the scraped
/// `FilmDetail.cinemaLinks`: distinct cinema + URL pairs across all
/// days, deduped by cinema name, sorted alphabetically by name.
final class CinemaLinksTests: XCTestCase {

    private func showtime(_ time: String) -> Showtime {
        Showtime(time: time, format: "2D", room: nil, bookingURL: nil)
    }

    private func cinema(_ name: String, url: String?) -> CinemaShowings {
        CinemaShowings(
            cinema: name,
            cinemaURL: url.flatMap { URL(string: $0) },
            showtimes: [showtime("12:00")]
        )
    }

    func testDedupesByCinemaAcrossDays() {
        let days = [
            DayShowings(date: "2026-06-01", label: "Pon 1 czerwca", cinemas: [
                cinema("Helios", url: "https://helios.pl/film"),
                cinema("Multikino", url: "https://multikino.pl/film"),
            ]),
            DayShowings(date: "2026-06-02", label: "Wt 2 czerwca", cinemas: [
                cinema("Helios", url: "https://helios.pl/film-other-day"),
            ]),
        ]
        let links = days.cinemaLinks()
        XCTAssertEqual(links.map(\.cinema), ["Helios", "Multikino"])
        // First URL seen for a cinema wins.
        XCTAssertEqual(links.first(where: { $0.cinema == "Helios" })?.url.absoluteString,
                       "https://helios.pl/film")
    }

    func testSortsAlphabeticallyByCinemaName() {
        let days = [
            DayShowings(date: "2026-06-01", label: "Pon", cinemas: [
                cinema("Zorza", url: "https://zorza.pl"),
                cinema("Atlantic", url: "https://atlantic.pl"),
                cinema("Multikino", url: "https://multikino.pl"),
            ]),
        ]
        XCTAssertEqual(days.cinemaLinks().map(\.cinema), ["Atlantic", "Multikino", "Zorza"])
    }

    func testSkipsCinemasWithoutURL() {
        let days = [
            DayShowings(date: "2026-06-01", label: "Pon", cinemas: [
                cinema("Helios", url: nil),
                cinema("Multikino", url: "https://multikino.pl"),
            ]),
        ]
        XCTAssertEqual(days.cinemaLinks().map(\.cinema), ["Multikino"])
    }

    func testEmptyShowingsYieldNoLinks() {
        XCTAssertTrue([DayShowings]().cinemaLinks().isEmpty)
    }
}
