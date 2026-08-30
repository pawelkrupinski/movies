import XCTest
@testable import KinowoAuth

/// The `/api/catalog` country wire shape
/// (`{code,name,baseUrl,language,brand,timezone}`) decodes to [CountryDTO] and
/// maps to [Country]; a row with an unparseable `baseUrl` is dropped rather than
/// crashing the app.
final class CountryDTOTests: XCTestCase {
    func testDecodesAndMapsToCountry() throws {
        let json = #"[{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes.cc/uk","language":"en","brand":"Showtimes","timezone":"Europe/London"}]"#
        let dtos = try JSONDecoder().decode([CountryDTO].self, from: Data(json.utf8))
        let country = dtos[0].toCountry()
        XCTAssertEqual(country?.code, "uk")
        XCTAssertEqual(country?.displayName, "United Kingdom")
        XCTAssertEqual(country?.baseURL.absoluteString, "https://showtimes.cc/uk")
        XCTAssertEqual(country?.languageCode, "en")
        // The field the pruning fix reads — a London show disappears on London
        // time, not Warsaw.
        XCTAssertEqual(country?.timeZone, TimeZone(identifier: "Europe/London"))
    }

    /// The US row: the catalog's zone (its first region's, which the server
    /// derives) is what the app runs on — not the nominal Eastern zone the
    /// compile-time fallback registry carries.
    func testUsRowTakesItsZoneFromTheCatalogNotTheFallbackRegistry() throws {
        let json = #"[{"code":"us","name":"United States","baseUrl":"https://showtimes.cc/us","language":"en","brand":"Showtimes","timezone":"America/Chicago"}]"#
        let dtos = try JSONDecoder().decode([CountryDTO].self, from: Data(json.utf8))
        let country = dtos[0].toCountry()
        XCTAssertEqual(country?.code, "us")
        XCTAssertEqual(country?.baseURL.absoluteString, "https://showtimes.cc/us")
        XCTAssertEqual(country?.languageCode, "en")
        XCTAssertEqual(country?.timeZone, TimeZone(identifier: "America/Chicago"))
    }

    func testMissingTimezoneFallsBackToWarsaw() throws {
        // An older bundled seed / a server that predates the field: no timezone
        // key. Decode must still succeed and default to the historical zone.
        let json = #"[{"code":"pl","name":"Polska","baseUrl":"https://kinowo.net","language":"pl","brand":"Kinowo"}]"#
        let dtos = try JSONDecoder().decode([CountryDTO].self, from: Data(json.utf8))
        XCTAssertEqual(dtos[0].toCountry()?.timeZone, TimeZone(identifier: "Europe/Warsaw"))
    }

    func testDropsRowWithUnparseableBaseUrl() {
        let dto = CountryDTO(code: "xx", name: "X", baseUrl: "ht tp://bad url", language: "en", timezone: nil)
        XCTAssertNil(dto.toCountry())
    }
}
