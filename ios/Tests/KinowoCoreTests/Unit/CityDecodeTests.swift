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
}
