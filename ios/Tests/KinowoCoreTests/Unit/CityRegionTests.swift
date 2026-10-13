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

    func testTopLevelRowSearchFoldsLikeCityNamesDo() {
        XCTAssertEqual(cities.topLevelRows(matching: "calif", inCountry: "us"), [.heading("California")])
        XCTAssertEqual(cities.topLevelRows(matching: "TEX", inCountry: "us"), [.heading("Texas")])
        XCTAssertEqual(
            cities.topLevelRows(matching: "", inCountry: "us"),
            [.heading("California"), .heading("Texas")])
        XCTAssertEqual(cities.topLevelRows(matching: "zzz", inCountry: "us"), [])
    }

    /// A city whose top group collapsed onto it alone (Delaware, Vermont —
    /// too small to split into metros) sits INTERLEAVED with the region
    /// headings, at its own position in the catalog's order — not stranded
    /// after every heading. Fails on the old "every heading, then every
    /// direct city" shape, which put "West Midlands" ahead of its
    /// alphabetical neighbours on the UK picker.
    func testATopLevelDirectCityIsInterleavedWithHeadingsNotStrandedAfterThem() {
        let delaware = City(slug: "delaware", name: "Delaware", lat: 39.0, lon: -75.5, country: "us")
        // Catalog order: California's first city, then Delaware (no region —
        // its top group collapsed), then Texas's first city.
        let ordered = [losAngeles, sanDiego, delaware, austin]
        XCTAssertEqual(
            ordered.topLevelRows(matching: "", inCountry: "us"),
            [.heading("California"), .city(delaware), .heading("Texas")])
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
        let seeded = try bundledSeed()

        // The first step lists all 55 states/territories: 52 split into
        // several places and show as a heading, while DC, American Samoa and
        // Guam hold one place each and collapse onto it (`CityGroup.soleCity`),
        // shown as a direct row, the same way a UK county with one place
        // needs no region tap either.
        let top = seeded.topLevelRows(matching: "", inCountry: "us")
        XCTAssertEqual(seeded.regions(inCountry: "us").count, 52)
        XCTAssertTrue(seeded.regions(inCountry: "us").contains("California"))
        XCTAssertEqual(top.count, 55)
        // Every US city is reachable: under a region heading, or as a direct
        // row on that first step. Checked against the rows the picker builds,
        // not a list of slugs, which went stale when the seed renamed
        // district-of-columbia to washington, guam to dededo-village and
        // american-samoa to pago-pago.
        let directRows = Set(top.compactMap { row -> String? in
            if case .city(let city) = row { return city.slug }
            return nil
        })
        XCTAssertEqual(directRows, ["washington", "pago-pago", "dededo-village"])
        for city in seeded.inCountry("us") where city.region == nil {
            XCTAssertTrue(directRows.contains(city.slug), "\(city.slug) has no region and is not a direct row, so it is unreachable")
        }
        // Poland is grouped by voivodeship: every page — major city, town or
        // cluster — carries its own, and all 16 are there.
        XCTAssertTrue(seeded.inCountry("pl").allSatisfy { $0.region != nil })
        XCTAssertEqual(seeded.regions(inCountry: "pl").count, 16)
        XCTAssertTrue(seeded.regions(inCountry: "pl").contains("Wielkopolskie"))
    }

    /// A place whose state collapsed onto it alone keeps that state's name as
    /// an alias, and search answers to it the way the web's `data-alias` does:
    /// typing "guam" must find the row that reads "Dededo Village area".
    func testSearchFindsACollapsedCityByItsGroupsName() throws {
        let seeded = try bundledSeed()
        func found(_ query: String) -> [String] {
            seeded.searchRows(matching: query, inCountry: "us").compactMap { row -> String? in
                if case .city(let city) = row { return city.slug }
                return nil
            }
        }
        XCTAssertEqual(found("guam"), ["dededo-village"])
        XCTAssertEqual(found("American Samoa"), ["pago-pago"])
        XCTAssertEqual(found("district of col"), ["washington"])
        // The first picker step's own filter answers to the alias as well.
        XCTAssertTrue(seeded.topLevelRows(matching: "guam", inCountry: "us").contains {
            if case .city(let city) = $0 { return city.slug == "dededo-village" }
            return false
        })
        // And the flat list a country-wide match reads.
        XCTAssertEqual(seeded.matching("shetland", inCountry: "uk").map(\.slug), ["lerwick"])
    }

    /// Optional on the wire, like `region`: an older server's catalog decodes.
    func testAliasesAreOptionalOnTheWire() throws {
        let bare = #"{"slug":"poznan","name":"Poznań","lat":52.41,"lon":16.93,"country":"pl"}"#
        XCTAssertNil(try JSONDecoder().decode(City.self, from: Data(bare.utf8)).aliases)
        let aliased = #"{"slug":"dededo-village","name":"Dededo Village area","lat":13.5,"lon":144.8,"country":"us","aliases":["Guam"]}"#
        XCTAssertEqual(try JSONDecoder().decode(City.self, from: Data(aliased.utf8)).aliases, ["Guam"])
    }

    /// The seed the app opens on. Decoded here rather than through
    /// `CatalogStore`, an app-target type this Foundation-only test target
    /// cannot see.
    private func bundledSeed() throws -> [City] {
        struct Seed: Decodable {
            struct Body: Decodable { let cities: [City] }
            let catalog: Body
        }
        return try JSONDecoder()
            .decode(Seed.self, from: try AppSources.data("Kinowo/catalog-seed.json"))
            .catalog.cities
    }
}
