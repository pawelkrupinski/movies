import XCTest
@testable import KinowoAuth

/// The `/api/catalog` country wire shape (`{code,name,baseUrl,language,brand}`)
/// decodes to [CountryDTO] and maps to [Country]; a row with an unparseable
/// `baseUrl` is dropped rather than crashing the app.
final class CountryDTOTests: XCTestCase {
    func testDecodesAndMapsToCountry() throws {
        let json = #"[{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes-uk.fly.dev","language":"en","brand":"Showtimes"}]"#
        let dtos = try JSONDecoder().decode([CountryDTO].self, from: Data(json.utf8))
        let country = dtos[0].toCountry()
        XCTAssertEqual(country?.code, "uk")
        XCTAssertEqual(country?.displayName, "United Kingdom")
        XCTAssertEqual(country?.baseURL.absoluteString, "https://showtimes-uk.fly.dev")
        XCTAssertEqual(country?.languageCode, "en")
    }

    func testDropsRowWithUnparseableBaseUrl() {
        let dto = CountryDTO(code: "xx", name: "X", baseUrl: "ht tp://bad url", language: "en")
        XCTAssertNil(dto.toCountry())
    }
}
