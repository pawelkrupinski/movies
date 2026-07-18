import XCTest
@testable import KinowoCore

final class ParsePruneFilterPipelineTests: XCTestCase {

    private static let warsawCalendar: Calendar = {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw")!
        return cal
    }()

    private func warsawDate(year: Int, month: Int, day: Int, hour: Int = 0, minute: Int = 0) -> Date {
        let comps = DateComponents(
            calendar: Self.warsawCalendar,
            timeZone: Self.warsawCalendar.timeZone,
            year: year, month: month, day: day, hour: hour, minute: minute
        )
        return comps.date!
    }

    /// Anchor `now` at midnight on the fixture's earliest day so prune
    /// keeps every showtime — the fixture is captured against that day,
    /// so any wall-clock past that morning would strip slots the tests
    /// below need to reason about.
    private func nowAtEarliestDay(_ films: [Film]) throws -> Date {
        let earliest = films
            .flatMap { $0.showings }
            .map(\.date)
            .min()
        guard let earliest = earliest else {
            XCTFail("no showings in parsed films")
            throw NSError(domain: "Fixture", code: 1)
        }
        let parts = earliest.split(separator: "-").compactMap { Int($0) }
        guard parts.count == 3 else {
            XCTFail("unexpected date format: \(earliest)")
            throw NSError(domain: "Fixture", code: 1)
        }
        return warsawDate(year: parts[0], month: parts[1], day: parts[2], hour: 0, minute: 0)
    }

    func testCinemasAreSortedByEarliestShowtimeWithinEachDay() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let now = try nowAtEarliestDay(films)
        let pruned = films.prunedPastShowings(now: now)
        XCTAssertFalse(pruned.isEmpty)

        for film in pruned {
            for day in film.showings {
                let earliestPerCinema = day.cinemas.map { ShowtimeClock.earliestMinutes($0) }
                XCTAssertEqual(earliestPerCinema, earliestPerCinema.sorted(),
                               "cinemas not sorted by earliest showtime on \(day.date) for \(film.title): \(earliestPerCinema)")
            }
        }
    }

    func testFilteredForTodayKeepsOnlyTodaysShowings() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let now = try nowAtEarliestDay(films)
        let today = DateFilter.iso(now)
        let pruned = films.prunedPastShowings(now: now)
        let filtered = pruned.filteredFor(
            date: .today, format: .empty, query: "",
            hidden: [],
            now: now
        )
        XCTAssertFalse(filtered.isEmpty, "expected films on \(today) but got none")
        for film in filtered {
            for day in film.showings {
                XCTAssertEqual(day.date, today,
                               "film \(film.title) survived 'today' filter with day \(day.date)")
            }
        }
    }

    func testQueryNarrowsResultsCaseInsensitively() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let now = try nowAtEarliestDay(films)
        let pruned = films.prunedPastShowings(now: now)

        // "Mandalorian " is a substring of the regular Mandalorian title
        // and the ukraiński dubbing variant — distinct from the
        // "Mandalorets'" UA card, which isn't an exact substring match.
        let lowercased = pruned.filteredFor(
            date: .anytime, format: .empty, query: "mandalorian ",
            hidden: []
        )
        XCTAssertFalse(lowercased.isEmpty)
        for film in lowercased {
            XCTAssertTrue(film.title.lowercased().contains("mandalorian "),
                          "query 'mandalorian ' returned non-matching \(film.title)")
        }
        XCTAssertLessThan(lowercased.count, pruned.count,
                          "query should reduce the result set")

        let uppercased = pruned.filteredFor(
            date: .anytime, format: .empty, query: "MANDALORIAN ",
            hidden: []
        )
        XCTAssertEqual(uppercased.map(\.title), lowercased.map(\.title),
                       "query should be case-insensitive")
    }

    func testHiddenTitleExcludedEntirely() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let now = try nowAtEarliestDay(films)
        let pruned = films.prunedPastShowings(now: now)

        XCTAssertTrue(pruned.contains { $0.title == "Michael" },
                      "fixture precondition: 'Michael' should be present before hiding")

        let visible = pruned.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: ["Michael"]
        )
        XCTAssertFalse(visible.contains { $0.title == "Michael" },
                       "'Michael' survived the hidden filter")
    }

    func testExcludingEveryOtherCinemaKeepsOnlyTheRemainingOne() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let now = try nowAtEarliestDay(films)
        let pruned = films.prunedPastShowings(now: now)

        // Untick every cinema in the payload except one, the sheet's way of
        // narrowing to a single venue.
        let selected = "Kino Muza"
        let others = Set(pruned.flatMap { $0.showings.flatMap { $0.cinemas.map(\.cinema) } })
            .subtracting([selected])
        let filtered = pruned.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: others
        )
        for film in filtered {
            for day in film.showings {
                for cinema in day.cinemas {
                    XCTAssertEqual(cinema.cinema, selected,
                                   "non-selected cinema \(cinema.cinema) survived on film \(film.title)")
                }
            }
        }
        XCTAssertLessThanOrEqual(filtered.count, pruned.count,
                                 "narrowing to one cinema should never grow the result")
    }

    func testPrunedPastShowingsIsIdempotent() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let now = try nowAtEarliestDay(films)
        let once = films.prunedPastShowings(now: now)
        let twice = once.prunedPastShowings(now: now)
        XCTAssertEqual(once.map(\.title), twice.map(\.title))
        XCTAssertEqual(once, twice, "prune should be idempotent under a fixed now")
    }
}
