import XCTest
@testable import KinowoCore

/// `City.searchLevelKeys` picks the search box's contextual copy — mirrors
/// `pickerSearchLevels()` in landing.scala.html, so these pin the same cases
/// that file's own comment calls out: a nesting country shows two levels at
/// its root and one once a region is picked, a single-level country shows
/// one, and a flat country shows none. Mirrors Android's `CitySearchLevelTest`.
final class CitySearchLevelKeysTests: XCTestCase {

    func testTheUkShowsBothLevelsAtItsRoot() {
        XCTAssertEqual(
            City.searchLevelKeys(country: "uk", region: nil, subregion: nil),
            ["citygate.level.uk.region", "citygate.level.uk.subregion"])
    }

    func testTheUkDropsToItsSecondLevelOnceARegionIsPicked() {
        XCTAssertEqual(
            City.searchLevelKeys(country: "uk", region: "England", subregion: nil),
            ["citygate.level.uk.subregion"])
    }

    func testTheUkReachesNoFurtherLevelOnceFullyDrilled() {
        XCTAssertEqual(
            City.searchLevelKeys(country: "uk", region: "England", subregion: "West Midlands"),
            [])
    }

    func testASingleLevelCountryShowsOnlyItsOwnTerm() {
        XCTAssertEqual(City.searchLevelKeys(country: "us", region: nil, subregion: nil),
                       ["citygate.level.us.region"])
        XCTAssertEqual(City.searchLevelKeys(country: "de", region: nil, subregion: nil),
                       ["citygate.level.de.region"])
    }

    func testASingleLevelCountryReachesNoFurtherLevelOnceARegionIsPicked() {
        XCTAssertEqual(City.searchLevelKeys(country: "us", region: "California", subregion: nil), [])
    }

    func testAFlatCountryNeverShowsAGroupTerm() {
        XCTAssertEqual(City.searchLevelKeys(country: "pl", region: nil, subregion: nil), [])
        XCTAssertEqual(City.searchLevelKeys(country: "es", region: nil, subregion: nil), [])
    }
}
