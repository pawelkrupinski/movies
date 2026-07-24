import XCTest
@testable import KinowoAuth

/// The `/api/catalog` country wire shape
/// (`{code,name,baseUrl,language,brand,timezone}`) decodes to [CountryDTO] and
/// maps to [Country]; a row with an unparseable `baseUrl` is dropped rather than
/// crashing the app.
final class CountryDTOTests: XCTestCase {
    func testDecodesAndMapsToCountry() throws {
        let json = #"[{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes-uk.fly.dev","language":"en","brand":"Showtimes","timezone":"Europe/London"}]"#
        let dtos = try JSONDecoder().decode([CountryDTO].self, from: Data(json.utf8))
        let country = dtos[0].toCountry()
        XCTAssertEqual(country?.code, "uk")
        XCTAssertEqual(country?.displayName, "United Kingdom")
        XCTAssertEqual(country?.baseURL.absoluteString, "https://showtimes-uk.fly.dev")
        XCTAssertEqual(country?.languageCode, "en")
        // The field the pruning fix reads — a London show disappears on London
        // time, not Warsaw.
        XCTAssertEqual(country?.timeZone, TimeZone(identifier: "Europe/London"))
    }

    func testMissingTimezoneFallsBackToWarsaw() throws {
        // An older bundled seed / a server that predates the field: no timezone
        // key. Decode must still succeed and default to the historical zone.
        let json = #"[{"code":"pl","name":"Polska","baseUrl":"https://kinowo.fly.dev","language":"pl","brand":"Kinowo"}]"#
        let dtos = try JSONDecoder().decode([CountryDTO].self, from: Data(json.utf8))
        XCTAssertEqual(dtos[0].toCountry()?.timeZone, TimeZone(identifier: "Europe/Warsaw"))
    }

    func testDropsRowWithUnparseableBaseUrl() {
        let dto = CountryDTO(code: "xx", name: "X", baseUrl: "ht tp://bad url", language: "en", timezone: nil)
        XCTAssertNil(dto.toCountry())
    }
}
