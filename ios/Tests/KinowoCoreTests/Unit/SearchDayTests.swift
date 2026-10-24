import XCTest
@testable import KinowoCore

/// A search looks through every day: the first letter moves the listing to "All" and remembers
/// the day it was on; clearing goes back there, unless a day was picked while it ran.
final class SearchDayTests: XCTestCase {

    func testASearchMovesToAllAndClearingGoesBack() {
        var searchDay = SearchDay()
        XCTAssertEqual(searchDay.queryChanged(to: "s", showing: .tomorrow), .anytime)
        XCTAssertNil(searchDay.queryChanged(to: "st", showing: .anytime), "typing on moves nothing")
        XCTAssertEqual(searchDay.outsideOf, .tomorrow)
        XCTAssertEqual(searchDay.queryChanged(to: "  ", showing: .anytime), .tomorrow)
        XCTAssertNil(searchDay.outsideOf)
    }

    func testADayPickedDuringTheSearchStaysWhenItIsCleared() {
        var searchDay = SearchDay()
        _ = searchDay.queryChanged(to: "dune", showing: .today)
        searchDay.dayPicked()
        XCTAssertNil(searchDay.outsideOf, "a picked day shows only itself: nothing is drawn lighter")
        XCTAssertNil(searchDay.queryChanged(to: "", showing: .week))
        // The next search starts afresh from wherever the listing is.
        XCTAssertEqual(searchDay.queryChanged(to: "a", showing: .week), .anytime)
        XCTAssertEqual(searchDay.queryChanged(to: "", showing: .anytime), .week)
    }

    func testAPickWithNoSearchRunningIsNothing() {
        var searchDay = SearchDay()
        searchDay.dayPicked()
        XCTAssertEqual(searchDay.queryChanged(to: "a", showing: .today), .anytime)
        XCTAssertEqual(searchDay.queryChanged(to: "", showing: .anytime), .today)
    }

    func testASearchBegunOnAllMovesNothingAndDrawsNothingLighter() {
        var searchDay = SearchDay()
        XCTAssertNil(searchDay.queryChanged(to: "a", showing: .anytime))
        let zone = TimeZone(identifier: "Europe/Warsaw")!
        XCTAssertFalse(searchDay.outsideDays(now: Date(), zone: zone)?.contains(DateFilter.iso(Date())) ?? false)
    }

    func testTheDaysOutsideTheSearchedFromDay() {
        var searchDay = SearchDay()
        _ = searchDay.queryChanged(to: "a", showing: .today)
        let now = Date()
        let zone = TimeZone(identifier: "Europe/Warsaw")!
        let outside = searchDay.outsideDays(now: now, zone: zone)
        XCTAssertEqual(outside?.contains(DateFilter.iso(now)), false)
        XCTAssertEqual(outside?.contains(DateFilter.iso(now.addingTimeInterval(86_400))), true)
    }
}
