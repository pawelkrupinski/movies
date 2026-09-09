import XCTest
@testable import KinowoCore

/// The US is picked in two steps — state, then city — because 468 places in one
/// A-to-Z is not a list anybody reads. These are the queries the picker drives
/// both steps from; every other country has no regions and keeps one flat list.
final class CityRegionTests: XCTestCase {

    private let losAngeles = City(slug: "los-angeles", name: "Los Angeles", lat: 34.05, lon: -118.24, country: "us", region: "California")
    private let sanDiego = City(slug: "san-diego", name: "San Diego", lat: 32.72, lon: -117.16, country: "us", region: "California")
    private let austin = City(slug: "austin", name: "Austin", lat: 30.27, lon: -97.74, country: "us", region: "Texas")
    private let poznan = City(slug: "poznan", name: "Poznań", lat: 52.41, lon: 16.93, country: "pl")

    private var cities: [City] { [losAngeles, sanDiego, austin, poznan] }

    func testRegionsAreDistinctAndInCatalogOrder() {
        XCTAssertEqual(cities.regions(inCountry: "us"), ["California", "Texas"])
    }

    /// The emptiness is load-bearing: it is what the picker reads as "one flat
    /// list", so a country without states must report none rather than a blank.
    func testAnUngroupedCountryHasNoRegions() {
        XCTAssertEqual(cities.regions(inCountry: "pl"), [])
    }

    func testRegionSearchFoldsLikeCityNamesDo() {
        XCTAssertEqual(cities.regionsMatching("calif", inCountry: "us"), ["California"])
        XCTAssertEqual(cities.regionsMatching("TEX", inCountry: "us"), ["Texas"])
        XCTAssertEqual(cities.regionsMatching("", inCountry: "us"), ["California", "Texas"])
        XCTAssertEqual(cities.regionsMatching("zzz", inCountry: "us"), [])
    }

    func testACityListConfinedToOneRegion() {
        XCTAssertEqual(
            cities.matching("", inCountry: "us", region: "California").map(\.slug),
            ["los-angeles", "san-diego"])
        XCTAssertEqual(cities.matching("", inCountry: "us", region: "Texas").map(\.slug), ["austin"])
    }

    /// A nil region is the ungrouped case, and must not filter anything out —
    /// the same call serves both a grouped country's second step and an
    /// ungrouped country's only one.
    func testANilRegionLeavesTheCountryListAlone() {
        XCTAssertEqual(cities.matching("", inCountry: "us", region: nil).count, 3)
        XCTAssertEqual(cities.matching("", inCountry: "pl", region: nil).map(\.slug), ["poznan"])
    }

    func testSearchStillNarrowsInsideARegion() {
        XCTAssertEqual(
            cities.matching("san", inCountry: "us", region: "California").map(\.slug),
            ["san-diego"])
        // A city of another state never leaks in, however well it matches.
        XCTAssertEqual(cities.matching("austin", inCountry: "us", region: "California"), [])
    }

    /// The field is optional on the wire: a catalog from a server that predates
    /// it, and every hand-written row, still decode.
    func testRegionIsOptionalOnTheWire() throws {
        let json = #"{"slug":"poznan","name":"Poznań","lat":52.41,"lon":16.93,"country":"pl"}"#
        let decoded = try JSONDecoder().decode(City.self, from: Data(json.utf8))
        XCTAssertNil(decoded.region)

        let withRegion = #"{"slug":"austin","name":"Austin","lat":30.27,"lon":-97.74,"country":"us","region":"Texas"}"#
        XCTAssertEqual(try JSONDecoder().decode(City.self, from: Data(withRegion.utf8)).region, "Texas")
    }

    /// The bundled seed is the list the app actually opens on, so the states have
    /// to be there in it — not merely supported by the model.
    func testTheBundledSeedCarriesEveryUsState() throws {
        // Decoded here rather than through `CatalogStore`, which is an app-target
        // type this Foundation-only test target cannot see.
        struct Seed: Decodable {
            struct Body: Decodable { let cities: [City] }
            let catalog: Body
        }
        let seeded = try JSONDecoder()
            .decode(Seed.self, from: try AppSources.data("Kinowo/catalog-seed.json"))
            .catalog.cities

        // 48 of the 55 states/territories actually split into several metros —
        // the other 7 (Delaware, Vermont, Rhode Island, DC, American Samoa,
        // Guam, the Virgin Islands) are single-metro and collapse onto their
        // one city instead (`CityGroup.soleCity`), the same way a UK county
        // with one place needs no region tap either.
        XCTAssertEqual(seeded.regions(inCountry: "us").count, 48)
        XCTAssertTrue(seeded.regions(inCountry: "us").contains("California"))
        // Every US city is PLACED somewhere reachable: a grouped state (region)
        // or a collapsed single-metro one (a direct row on the region step).
        let collapsed: Set<String> = [
            "delaware", "vermont", "rhode-island", "district-of-columbia",
            "american-samoa", "guam", "virgin-islands",
        ]
        for city in seeded.inCountry("us") {
            if collapsed.contains(city.slug) {
                XCTAssertNil(city.region, "\(city.slug) should have collapsed onto its one metro")
            } else {
                XCTAssertNotNil(city.region, "\(city.slug) has no region and would be unreachable")
            }
        }
        XCTAssertTrue(seeded.inCountry("pl").allSatisfy { $0.region == nil })
    }
}
