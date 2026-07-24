import XCTest
@testable import KinowoCore

/// The UK/DE fix: past-showtime pruning and the Dziś/Jutro day buckets must
/// reason in the SELECTED country's zone, not a hardcoded Europe/Warsaw. Warsaw
/// runs an hour ahead of London, so the old code dropped London screenings ~1h
/// early and mis-bucketed them near midnight.
final class TimezonePruningTests: XCTestCase {

    private let london = TimeZone(identifier: "Europe/London")!
    private let warsaw = TimeZone(identifier: "Europe/Warsaw")!

    /// An absolute instant from wall-clock components read in `zone`.
    private func instant(_ zone: TimeZone, _ y: Int, _ mo: Int, _ d: Int, _ h: Int, _ mi: Int) -> Date {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = zone
        return DateComponents(calendar: cal, timeZone: zone,
                              year: y, month: mo, day: d, hour: h, minute: mi).date!
    }

    private func slot(_ time: String) -> Showtime {
        Showtime(time: time, format: "2D", room: nil, bookingURL: nil)
    }

    // The crux: it's 20:00 in London (= 21:00 in Warsaw). A 20:30 London show is
    // still 30 min away and must stay; judged on Warsaw wall-clock the same slot
    // reads as 18:30 UTC ≤ now−30min and gets dropped an hour early.
    func testLondonShowKeptOnLondonTimeDroppedOnWarsaw() {
        let now = instant(london, 2026, 5, 22, 20, 0)
        XCTAssertTrue(ShowtimeClock.isFuture(slot("20:30"), on: "2026-05-22", now: now, zone: london))
        XCTAssertFalse(ShowtimeClock.isFuture(slot("20:30"), on: "2026-05-22", now: now, zone: warsaw))
    }

    // Same story through the full film-level prune: the film survives on London
    // time (its only slot is still upcoming) but empties out on Warsaw time.
    func testPruneKeepsLondonFilmThatWarsawWouldDrop() {
        let now = instant(london, 2026, 5, 22, 20, 0)
        let films = [
            Film(title: "Evening Show", posterURL: nil, fallbackPosterURLs: [],
                 runtimeMinutes: 100, releaseYear: nil, genres: [], ratings: .empty,
                 countries: [], directors: [], cast: [],
                 showings: [DayShowings(date: "2026-05-22", label: "Fri", cinemas: [
                     CinemaShowings(cinema: "Picturehouse", cinemaURL: nil, showtimes: [slot("20:30")])
                 ])])
        ]
        XCTAssertEqual(films.prunedPastShowings(now: now, zone: london).count, 1)
        XCTAssertTrue(films.prunedPastShowings(now: now, zone: warsaw).isEmpty)
    }

    // Near midnight the day bucket must follow the local calendar day. At 23:30
    // London it's already 00:30 the next day in Warsaw, so "today" differs.
    func testTodayBucketFollowsLocalCalendarDayNearMidnight() {
        let now = instant(london, 2026, 5, 22, 23, 30) // = 2026-05-23 00:30 Warsaw
        XCTAssertTrue(DateFilter.today.matches(date: "2026-05-22", now: now, zone: london))
        XCTAssertFalse(DateFilter.today.matches(date: "2026-05-22", now: now, zone: warsaw))
        XCTAssertTrue(DateFilter.today.matches(date: "2026-05-23", now: now, zone: warsaw))
    }

    func testIsoRendersLocalCalendarDate() {
        let now = instant(london, 2026, 5, 22, 23, 30)
        XCTAssertEqual(DateFilter.iso(now, zone: london), "2026-05-22")
        XCTAssertEqual(DateFilter.iso(now, zone: warsaw), "2026-05-23")
    }

    // The default zone stays Warsaw, so every existing PL-only call site and
    // test keeps its prior behaviour without passing a zone.
    func testDefaultZoneIsWarsaw() {
        let now = instant(warsaw, 2026, 5, 22, 18, 0)
        XCTAssertEqual(DateFilter.iso(now), DateFilter.iso(now, zone: warsaw))
        XCTAssertEqual(
            ShowtimeClock.isFuture(slot("17:30"), on: "2026-05-22", now: now),
            ShowtimeClock.isFuture(slot("17:30"), on: "2026-05-22", now: now, zone: warsaw)
        )
    }
}
