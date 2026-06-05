import XCTest
@testable import KinowoCore

final class CityTests: XCTestCase {

    // ── nearestWithin100km ────────────────────────────────────────

    func testPoznanCoordsResolveToPoznan() {
        let city = City.nearestWithin100km(lat: 52.4064, lon: 16.9252)
        XCTAssertEqual(city?.slug, "poznan")
    }

    func testCoordsJustOutsidePoznanStillResolveWithin100km() {
        // ~40 km west of Poznań — well inside the radius.
        let city = City.nearestWithin100km(lat: 52.4064, lon: 16.3)
        XCTAssertEqual(city?.slug, "poznan")
    }

    func testEachSupportedCityResolvesFromItsOwnCoords() {
        XCTAssertEqual(City.nearestWithin100km(lat: 51.1079, lon: 17.0385)?.slug, "wroclaw")
        XCTAssertEqual(City.nearestWithin100km(lat: 52.2297, lon: 21.0122)?.slug, "warszawa")
    }

    func testCoordsFarFromEveryCityAreOutOfRange() {
        // Kraków (50.06, 19.94) is >200 km from Poznań, Wrocław and Warszawa —
        // beyond the 100 km cutoff, so the gate falls back to manual choice.
        XCTAssertNil(City.nearestWithin100km(lat: 50.0647, lon: 19.9450))
    }

    func testFarAwayCoordsReturnNil() {
        let city = City.nearestWithin100km(lat: 0, lon: 0)
        XCTAssertNil(city)
    }

    func testDefaultIsFirstCity() {
        XCTAssertEqual(City.default.slug, City.all.first?.slug)
        XCTAssertEqual(City.default.slug, "poznan")
    }

    // ── apiURL (city-prefixed endpoints) ──────────────────────────

    func testRepertoireURLIsCityPrefixed() {
        let base = URL(string: "https://kinowo.fly.dev")!
        let url = City.apiURL(base: base, slug: "poznan", endpoint: "repertoire")
        XCTAssertEqual(url.absoluteString, "https://kinowo.fly.dev/poznan/api/repertoire")
        XCTAssertTrue(url.absoluteString.hasSuffix("/poznan/api/repertoire"))
    }

    func testDetailsURLIsCityPrefixed() {
        let base = URL(string: "https://kinowo.fly.dev")!
        let url = City.apiURL(base: base, slug: "poznan", endpoint: "details")
        XCTAssertEqual(url.absoluteString, "https://kinowo.fly.dev/poznan/api/details")
        XCTAssertTrue(url.absoluteString.hasSuffix("/poznan/api/details"))
    }

    func testApiURLHonoursADifferentSlug() {
        let base = URL(string: "https://kinowo.fly.dev")!
        let url = City.apiURL(base: base, slug: "warszawa", endpoint: "repertoire")
        XCTAssertEqual(url.absoluteString, "https://kinowo.fly.dev/warszawa/api/repertoire")
    }
}
