import XCTest
@testable import KinowoCore

/// `[DayShowings].cinemaLinks()` replaces the scraped
/// `FilmDetail.cinemaLinks`: one link per distinct URL across all days,
/// labelled by cinema where the URL is that cinema's own and by site where
/// several cinemas share one, sorted alphabetically by label.
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
        XCTAssertEqual(links.map(\.label), ["Helios", "Multikino"])
        // First URL seen for a cinema wins.
        XCTAssertEqual(links.first(where: { $0.label == "Helios" })?.url.absoluteString,
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
        XCTAssertEqual(days.cinemaLinks().map(\.label), ["Atlantic", "Multikino", "Zorza"])
    }

    func testSkipsCinemasWithoutURL() {
        let days = [
            DayShowings(date: "2026-06-01", label: "Pon", cinemas: [
                cinema("Helios", url: nil),
                cinema("Multikino", url: "https://multikino.pl"),
            ]),
        ]
        XCTAssertEqual(days.cinemaLinks().map(\.label), ["Multikino"])
    }

    func testEmptyShowingsYieldNoLinks() {
        XCTAssertTrue([DayShowings]().cinemaLinks().isEmpty)
    }

    /// The PL shape: a chain points every one of its venues at the same film
    /// page, so those venues collapse to one pill named after the chain —
    /// the name they all share.
    func testChainVenuesSharingOneURLCollapseToTheChainName() {
        let multikino = "https://www.multikino.pl/filmy/andre-rieu"
        let days = [
            DayShowings(date: "2026-06-01", label: "Pon", cinemas: [
                cinema("Multikino Złote Tarasy", url: multikino),
                cinema("Multikino Wola Park", url: multikino),
                cinema("Multikino Reduta", url: multikino),
                cinema("Kino Atlantic", url: "https://www.novekino.pl/kina/atlantic/film.php?id=1"),
            ]),
        ]
        XCTAssertEqual(days.cinemaLinks().map(\.label), ["Kino Atlantic", "Multikino"])
    }

    /// The UK shape: the aggregator gives every cinema the same per-film
    /// page, so a per-cinema pill row would be 72 links to one destination —
    /// and, keyed by that one URL, SwiftUI drew the first cinema's name 72
    /// times over. Unrelated cinemas share no name, so the site names the
    /// single pill they collapse into.
    func testUnrelatedCinemasSharingOneURLCollapseToASiteLink() {
        let shared = "https://www.flicks.co.uk/movie/the-odyssey-2026/"
        let days = [
            DayShowings(date: "2026-07-26", label: "Sunday 26 July", cinemas: [
                cinema("Act One Cinema Acton", url: shared),
                cinema("BFI London IMAX", url: shared),
                cinema("Barbican London", url: shared),
            ]),
        ]
        let links = days.cinemaLinks()
        XCTAssertEqual(links.map(\.label), ["Flicks"])
        XCTAssertEqual(links.first?.url.absoluteString, shared)
    }

    /// Mixed data: a cinema that owns its URL keeps its own name; only the
    /// cinemas behind a shared URL fold into the site pill.
    func testOwnURLsKeepTheirCinemaNameAlongsideASharedOne() {
        let shared = "https://www.flicks.co.uk/movie/the-odyssey-2026/"
        let days = [
            DayShowings(date: "2026-07-26", label: "Sunday 26 July", cinemas: [
                cinema("Act One Cinema Acton", url: shared),
                cinema("BFI London IMAX", url: shared),
                cinema("Everyman Baker Street", url: "https://everymancinema.com/the-odyssey"),
            ]),
        ]
        XCTAssertEqual(days.cinemaLinks().map(\.label), ["Everyman Baker Street", "Flicks"])
    }

    /// The row is rendered by a `ForEach` keyed off the link, so no two links
    /// may be equal — that collision is exactly what duplicated the first
    /// pill down the whole screen.
    func testLinksAreDistinct() {
        let shared = "https://www.flicks.co.uk/movie/the-odyssey-2026/"
        let days = [
            DayShowings(date: "2026-07-26", label: "Sunday", cinemas: [
                cinema("Act One Cinema Acton", url: shared),
                cinema("BFI London IMAX", url: shared),
                cinema("Everyman Baker Street", url: "https://everymancinema.com/x"),
            ]),
            DayShowings(date: "2026-07-27", label: "Monday", cinemas: [
                cinema("Act One Cinema Acton", url: shared),
            ]),
        ]
        let links = days.cinemaLinks()
        XCTAssertEqual(Set(links).count, links.count, "duplicate links: \(links)")
        XCTAssertEqual(Set(links.map(\.url)).count, links.count, "duplicate link URLs: \(links)")
    }

    /// One chain listing the same film under two URLs gives two links with the
    /// same label. The grouping is a Dictionary, so without a tie-break their
    /// order would come from hash order and vary between runs.
    func testTwoLinksSharingALabelOrderByURL() {
        let days = [
            DayShowings(date: "2026-06-01", label: "Pon", cinemas: [
                cinema("Cinema City Arkadia", url: "https://www.cinema-city.pl/filmy/x/b"),
                cinema("Cinema City Sadyba", url: "https://www.cinema-city.pl/filmy/x/b"),
                cinema("Cinema City Bemowo", url: "https://www.cinema-city.pl/filmy/x/a"),
                cinema("Cinema City Mokotów", url: "https://www.cinema-city.pl/filmy/x/a"),
            ]),
        ]
        let links = days.cinemaLinks()
        XCTAssertEqual(links.map(\.label), ["Cinema City", "Cinema City"])
        XCTAssertEqual(links.map(\.url.absoluteString),
                       ["https://www.cinema-city.pl/filmy/x/a",
                        "https://www.cinema-city.pl/filmy/x/b"])
    }

    func testSiteNameStripsWWWAndTLD() {
        XCTAssertEqual(CinemaLink.siteName(of: URL(string: "https://www.flicks.co.uk/movie/x/")!), "Flicks")
        XCTAssertEqual(CinemaLink.siteName(of: URL(string: "https://multikino.pl/film/y")!), "Multikino")
    }

    /// The shared name is whole words only — "Cinema City Arkadia" and
    /// "Cinema City Sadyba" share "Cinema City", never a half-word like
    /// "Cinema City S" that no cinema is called.
    func testSharedLabelTakesWholeWordsOnly() {
        let url = URL(string: "https://www.cinema-city.pl/filmy/x")!
        XCTAssertEqual(
            CinemaLink.label(for: url, sharing: ["Cinema City Arkadia", "Cinema City Sadyba"]),
            "Cinema City")
        // "Sadyba" vs "Sadowa": a common leading substring, but not a common word.
        XCTAssertEqual(
            CinemaLink.label(for: url, sharing: ["Kino Sadyba", "Kino Sadowa"]),
            "Kino")
        XCTAssertEqual(
            CinemaLink.label(for: url, sharing: ["Barbican London", "BFI London IMAX"]),
            "Cinema-city")
        XCTAssertEqual(CinemaLink.label(for: url, sharing: ["Kino Atlantic"]), "Kino Atlantic")
    }
}
