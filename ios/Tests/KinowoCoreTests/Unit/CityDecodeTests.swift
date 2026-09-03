import XCTest
@testable import KinowoCore

/// `City` decodes directly from the `/api/catalog` city shape
/// (`{slug,name,lat,lon,country}`), so the catalog's `cities` array parses with
/// no DTO — the same `[City]` the pickers query.
final class CityDecodeTests: XCTestCase {
    func testDecodesCatalogCityShape() throws {
        let json = #"[{"slug":"london","name":"London","lat":51.5074,"lon":-0.1278,"country":"uk"}]"#
        let cities = try JSONDecoder().decode([City].self, from: Data(json.utf8))
        XCTAssertEqual(cities.count, 1)
        XCTAssertEqual(cities[0].slug, "london")
        XCTAssertEqual(cities[0].name, "London")
        XCTAssertEqual(cities[0].country, "uk")
        XCTAssertEqual(cities[0].lat, 51.5074, accuracy: 1e-6)
        XCTAssertEqual(cities[0].lon, -0.1278, accuracy: 1e-6)
    }

    /// A city carries its OWN zone only where it differs from its country's, so
    /// most cities have none and must fall back rather than fail.
    func testDecodesPerCityTimezoneWhenPresent() throws {
        let json = #"""
        [{"slug":"knoxville","name":"Knoxville","lat":35.9,"lon":-83.9,"country":"us","region":"Tennessee","timezone":"America/New_York"},
         {"slug":"los-angeles","name":"Los Angeles","lat":34.05,"lon":-118.3,"country":"us","region":"California"}]
        """#
        let cities = try JSONDecoder().decode([City].self, from: Data(json.utf8))
        XCTAssertEqual(cities[0].timezone, "America/New_York")
        XCTAssertNil(cities[1].timezone)
    }

    /// The country's zone is the fallback, and the whole point of the field is
    /// that a US metro does NOT take it: Tennessee is Central-predominant, so a
    /// Knoxville showtime pruned on the country's clock would go an hour wrong —
    /// and on the US country zone (Pacific, its biggest city) three hours.
    func testZoneFallsBackToTheCountrysOnlyWhenTheCityHasNone() throws {
        let pacific = TimeZone(identifier: "America/Los_Angeles")!
        let json = #"""
        [{"slug":"knoxville","name":"Knoxville","lat":35.9,"lon":-83.9,"country":"us","timezone":"America/New_York"},
         {"slug":"los-angeles","name":"Los Angeles","lat":34.05,"lon":-118.3,"country":"us"}]
        """#
        let cities = try JSONDecoder().decode([City].self, from: Data(json.utf8))
        XCTAssertEqual(cities.zone(ofSlug: "knoxville", fallback: pacific).identifier, "America/New_York")
        XCTAssertEqual(cities.zone(ofSlug: "los-angeles", fallback: pacific).identifier, "America/Los_Angeles")
        // A slug this catalog does not know — a stale saved selection, or a deep
        // link into another deployment — is the country's, never a crash.
        XCTAssertEqual(cities.zone(ofSlug: "nowhere", fallback: pacific).identifier, "America/Los_Angeles")
        XCTAssertEqual(cities.zone(ofSlug: nil, fallback: pacific).identifier, "America/Los_Angeles")
        // An identifier the platform does not know falls back rather than crashing.
        let bogus = [City(slug: "x", name: "X", lat: 0, lon: 0, country: "us", timezone: "Mars/Olympus")]
        XCTAssertEqual(bogus.zone(ofSlug: "x", fallback: pacific).identifier, "America/Los_Angeles")
    }
}
