import XCTest
@testable import KinowoCore

/// `searchRows(matching:inCountry:)` flattens every region, subregion and
/// city into ONE alphabetical list, so a query matches whichever level it
/// names — unlike `topLevelRows`/`secondLevelRows` (`CityRegionTests` /
/// `CitySubregionTests`), which only ever look at the level they're building.
final class CitySearchRowsTests: XCTestCase {

    private let birmingham = City(slug: "birmingham", name: "Birmingham", lat: 52.46, lon: -1.9,
                                   country: "uk", region: "England", subregion: "West Midlands")
    private let dudley = City(slug: "dudley", name: "Dudley", lat: 52.5, lon: -2.09,
                               country: "uk", region: "England", subregion: "West Midlands")
    private let cheshire = City(slug: "cheshire", name: "Cheshire", lat: 53.29, lon: -2.5,
                                 country: "uk", region: "England")
    private let glasgow = City(slug: "glasgow", name: "Glasgow", lat: 55.86, lon: -4.25,
                                country: "uk", region: "Scotland")

    private var cities: [City] { [birmingham, dudley, cheshire, glasgow] }

    /// The whole point: a subregion two levels below the root the visitor is
    /// looking at still matches, and carries its parent region along so the
    /// picker can jump straight there.
    func testASubregionTwoLevelsDeepIsFound() {
        XCTAssertEqual(
            cities.searchRows(matching: "west midlands", inCountry: "uk"),
            [.subregion("West Midlands", region: "England")])
    }

    func testARegionNameMatchesEvenWithoutDrillingIntoIt() {
        XCTAssertEqual(
            cities.searchRows(matching: "scotland", inCountry: "uk"),
            [.region("Scotland")])
    }

    func testACityNameStillMatchesRegardlessOfNesting() {
        XCTAssertEqual(
            cities.searchRows(matching: "glasgow", inCountry: "uk"),
            [.city(glasgow)])
    }

    /// Matches from every level land in ONE alphabetically sorted list — a
    /// city ("Cheshire", "Dudley"), a region ("England") and a subregion
    /// ("West Midlands") all interleave by NAME here, not grouped by kind or
    /// by depth (Birmingham and Glasgow carry no "e", so they drop out).
    func testMatchesFromDifferentLevelsSortTogetherAlphabetically() {
        XCTAssertEqual(
            cities.searchRows(matching: "e", inCountry: "uk").map(\.label),
            ["Cheshire", "Dudley", "England", "West Midlands"])
    }

    func testBlankQueryYieldsNothingSincePerLevelRowsAlreadyCoverIt() {
        XCTAssertEqual(cities.searchRows(matching: "", inCountry: "uk"), [])
        XCTAssertEqual(cities.searchRows(matching: "   ", inCountry: "uk"), [])
    }

    func testNoMatchYieldsAnEmptyList() {
        XCTAssertEqual(cities.searchRows(matching: "zzz", inCountry: "uk"), [])
    }

    /// Folds diacritics the same way `City.matches` does, so a query typed
    /// without Polish letters still finds a region carrying them.
    func testFoldsDiacriticsInRegionNamesToo() {
        let lodz = City(slug: "lodz", name: "Łódź", lat: 51.77, lon: 19.46, country: "pl", region: "Łódzkie")
        XCTAssertEqual(
            [lodz].searchRows(matching: "lodzkie", inCountry: "pl"),
            [.region("Łódzkie")])
    }
}
