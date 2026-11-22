import XCTest
@testable import KinowoCore
@testable import KinowoNetworking

/// The UI-test fixture (`KINOWO_UITEST_FIXTURE=1`) is dated in the zone the
/// store reasons in, which is the zone the grid judges "today" in. It was
/// dated in Warsaw's, so from 23:00 to 24:00 London a UK launch got a fixture
/// for Warsaw's today and tomorrow, its "Today" read "No showtimes." and
/// LocalizationUITests' UK detail case failed "Grid never appeared"
/// (Order independence #102, 23:57 UTC).
@MainActor
final class UITestFixtureDatingTests: XCTestCase {

    private let directory = ConditionalPayloadCache<Film>.scratchDirectory()
    private let london = TimeZone(identifier: "Europe/London")!

    override func setUp() {
        super.setUp()
        setenv("KINOWO_UITEST_FIXTURE", "1", 1)
    }

    override func tearDown() {
        unsetenv("KINOWO_UITEST_FIXTURE")
        ConditionalPayloadCache<Film>.discardScratchDirectory(directory)
        super.tearDown()
    }

    /// A UK launch on the pinned city: the store's zone is London, the slug
    /// unchanged (so no reload), then the grid's first paint serves the fixture.
    func testTheFixtureIsDatedOnTheStoresClockInItsZone() {
        let store = makeStore()
        store.use(citySlug: "warszawa", timeZone: london)
        // 23:30 London on the 21st is 00:30 on the 22nd in Warsaw.
        let now = ISO8601DateFormatter().date(from: "2026-11-21T23:30:00Z")!
        store.loadCachedData(now: now)
        assertTodayAndTomorrow(of: store, now: now, zone: london)
    }

    /// The other order: the fixture served first (in the store's default zone),
    /// then the city's zone adopted with the slug unchanged.
    ///
    /// Run at the real clock, in a zone whose calendar day differs from Warsaw's
    /// at this very moment, so the case says the same whenever it runs:
    /// UTC-12 is 13 h behind Warsaw in winter (14 in summer), UTC+14 13 h (12)
    /// ahead, so one of them always is.
    func testAdoptingAZoneRedatesAFixtureAlreadyServed() {
        let now = Date()
        let zone = zoneWhoseDayIsNotWarsaws(at: now)
        let store = makeStore()
        store.loadCachedData(now: now)
        store.use(citySlug: "warszawa", timeZone: zone)
        assertTodayAndTomorrow(of: store, now: Date(), zone: zone)
    }

    private func makeStore() -> RepertoireStore {
        RepertoireStore(base: URL(string: "https://fixture-dating-test.invalid")!, citySlug: "warszawa",
                        cache: .repertoire(in: directory), posters: .throwaway())
    }

    private func zoneWhoseDayIsNotWarsaws(at now: Date) -> TimeZone {
        let warsawDay = DateFilter.iso(now, zone: .warsaw)
        let zone = ["Etc/GMT+12", "Etc/GMT-14"]
            .compactMap(TimeZone.init(identifier:))
            .first { DateFilter.iso(now, zone: $0) != warsawDay }
        return zone!
    }

    private func assertTodayAndTomorrow(of store: RepertoireStore, now: Date, zone: TimeZone,
                                        file: StaticString = #filePath, line: UInt = #line) {
        let days = store.films.first?.showings.map(\.date) ?? []
        XCTAssertFalse(store.films.isEmpty, "the fixture was not served", file: file, line: line)
        XCTAssertTrue(days.contains { DateFilter.today.matches(date: $0, now: now, zone: zone) },
                      "no fixture day is today in \(zone.identifier): \(days)", file: file, line: line)
        XCTAssertTrue(days.contains { DateFilter.tomorrow.matches(date: $0, now: now, zone: zone) },
                      "no fixture day is tomorrow in \(zone.identifier): \(days)", file: file, line: line)
    }
}
