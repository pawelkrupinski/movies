import XCTest
@testable import KinowoCore

/// The UK picker's THIRD step — a county that holds more than one city
/// (West Midlands, Glamorgan, Antrim) gets its own tap; every other county
/// stays flat under its nation, exactly as before. Mirrors `CityRegionTests`'
/// shape one level down.
final class CitySubregionTests: XCTestCase {

    private let birmingham = City(slug: "birmingham", name: "Birmingham", lat: 52.46, lon: -1.9,
                                   country: "uk", region: "England", subregion: "West Midlands")
    private let dudley = City(slug: "dudley", name: "Dudley", lat: 52.5, lon: -2.09,
                               country: "uk", region: "England", subregion: "West Midlands")
    private let cheshire = City(slug: "cheshire", name: "Cheshire", lat: 53.29, lon: -2.5,
                                 country: "uk", region: "England")
    private let london = City(slug: "london", name: "London", lat: 51.51, lon: -0.13,
                               country: "uk", region: "England")

    private var cities: [City] { [birmingham, dudley, cheshire, london] }

    func testSubregionHeadingsAreDistinctAndInCatalogOrder() {
        XCTAssertEqual(
            cities.secondLevelRows(matching: "", inCountry: "uk", region: "England"),
            [.heading("West Midlands"), .city(cheshire), .city(london)])
    }

    /// A county with no split cities contributes no heading — every row comes
    /// back a plain city, which is what lets the picker tell "nothing to open
    /// here" from "there's a group to descend into".
    func testARegionWithNoSplitCountyHasNoSubregionHeadings() {
        let flatOnly: [City] = [cheshire, london]
        XCTAssertEqual(
            flatOnly.secondLevelRows(matching: "", inCountry: "uk", region: "England"),
            [.city(cheshire), .city(london)])
    }

    func testSubregionRowSearchFoldsLikeCityNamesDo() {
        XCTAssertEqual(
            cities.secondLevelRows(matching: "west", inCountry: "uk", region: "England"),
            [.heading("West Midlands")])
        XCTAssertEqual(cities.secondLevelRows(matching: "zzz", inCountry: "uk", region: "England"), [])
    }

    /// The "West Midlands" heading sits INTERLEAVED at its own position in the
    /// catalog's order, between the direct cities on either side of it — not
    /// stranded ahead of every direct row. This is the exact shape the
    /// reported "West Midlands out of order" bug had: the old code listed
    /// every heading before every direct city regardless of where in the
    /// (already-alphabetical) catalog order the heading's cities actually sat.
    func testASubregionHeadingIsInterleavedWithDirectCitiesNotStrandedAheadOfThem() {
        // Catalog order: a direct city, then West Midlands' two cities, then
        // another direct city — exactly how `Catalog.scala` would emit
        // England's cities once "Cheshire" < "West Midlands" < "London"-the-
        // display-name sort (illustrative order here; the real alphabetical
        // placement is asserted server-side in `CountrySpec`/`PageJsBehaviourSpec`).
        let ordered = [cheshire, birmingham, dudley, london]
        XCTAssertEqual(
            ordered.secondLevelRows(matching: "", inCountry: "uk", region: "England"),
            [.city(cheshire), .heading("West Midlands"), .city(london)])
    }

    /// Direct rows on the second step are the cities with NO subregion —
    /// Birmingham/Dudley move behind the "West Midlands" group row instead.
    func testDirectCitiesExcludeThoseWithASubregion() {
        let directCities = cities.secondLevelRows(matching: "", inCountry: "uk", region: "England")
            .compactMap { row -> City? in if case .city(let c) = row { return c } else { return nil } }
        XCTAssertEqual(directCities.map(\.slug), ["cheshire", "london"])
    }

    func testCitiesConfinedToOneSubregion() {
        XCTAssertEqual(
            cities.matching("", inCountry: "uk", region: "England", subregion: "West Midlands").map(\.slug),
            ["birmingham", "dudley"])
    }

    func testSearchStillNarrowsInsideASubregion() {
        XCTAssertEqual(
            cities.matching("dud", inCountry: "uk", region: "England", subregion: "West Midlands").map(\.slug),
            ["dudley"])
    }

    /// Optional on the wire, same shape as `region`: an older/plain payload
    /// still decodes with no subregion.
    func testSubregionIsOptionalOnTheWire() throws {
        let json = #"{"slug":"cheshire","name":"Cheshire","lat":53.29,"lon":-2.5,"country":"uk","region":"England"}"#
        let decoded = try JSONDecoder().decode(City.self, from: Data(json.utf8))
        XCTAssertNil(decoded.subregion)

        let withSubregion = #"{"slug":"birmingham","name":"Birmingham","lat":52.46,"lon":-1.9,"country":"uk","region":"England","subregion":"West Midlands"}"#
        XCTAssertEqual(try JSONDecoder().decode(City.self, from: Data(withSubregion.utf8)).subregion, "West Midlands")
    }

    /// The bundled seed is what the app actually opens on: West Midlands,
    /// Glamorgan and Antrim have to carry a `subregion` there too, not merely
    /// in the model.
    func testTheBundledSeedNamesTheThreeMultiCityCounties() throws {
        struct Seed: Decodable {
            struct Body: Decodable { let cities: [City] }
            let catalog: Body
        }
        let seeded = try JSONDecoder()
            .decode(Seed.self, from: try AppSources.data("Kinowo/catalog-seed.json"))
            .catalog.cities

        func subregion(_ slug: String) -> String? { seeded.first { $0.slug == slug }?.subregion }
        XCTAssertEqual(subregion("birmingham"), "West Midlands")
        XCTAssertEqual(subregion("dudley"), "West Midlands")
        XCTAssertEqual(subregion("sandwell"), "West Midlands")
        XCTAssertEqual(subregion("cardiff"), "Glamorgan")
        XCTAssertEqual(subregion("glamorgan"), "Glamorgan")
        XCTAssertEqual(subregion("antrim"), "Antrim")
        XCTAssertEqual(subregion("belfast"), "Antrim")
        // A collapsed county carries none — Cheshire reads correctly through
        // `region` alone.
        XCTAssertNil(subregion("cheshire"))
        XCTAssertTrue(seeded.inCountry("de").allSatisfy { $0.subregion == nil })
        XCTAssertTrue(seeded.inCountry("us").allSatisfy { $0.subregion == nil })
    }
}
