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

    // ── switchSuggestion (you're-nearer-another-city prompt) ──────

    func testSwitchSuggestionWhenNearerCityDiffers() {
        // Chosen Poznań, but the device is sitting in Wrocław.
        let s = City.switchSuggestion(
            chosenSlug: "poznan",
            lat: 51.1079, lon: 17.0385,
            lastPromptKey: nil
        )
        XCTAssertEqual(s?.target.slug, "wroclaw")
        XCTAssertEqual(s?.key, "poznan→wroclaw")
    }

    func testSwitchSuggestionSuppressedWhenAlreadyPromptedForThatPair() {
        let s = City.switchSuggestion(
            chosenSlug: "poznan",
            lat: 51.1079, lon: 17.0385,
            lastPromptKey: "poznan→wroclaw"
        )
        XCTAssertNil(s)
    }

    func testNoSwitchSuggestionWhenAlreadyInThatCity() {
        let s = City.switchSuggestion(
            chosenSlug: "wroclaw",
            lat: 51.1079, lon: 17.0385,
            lastPromptKey: nil
        )
        XCTAssertNil(s)
    }

    func testNoSwitchSuggestionWhenOutOfRangeOfEveryCity() {
        // Kraków — beyond the 100 km radius of every supported city.
        let s = City.switchSuggestion(
            chosenSlug: "poznan",
            lat: 50.0647, lon: 19.9450,
            lastPromptKey: nil
        )
        XCTAssertNil(s)
    }

    // ── initialChoiceSuppressKey (deliberate first-launch pick) ───

    func testInitialChoiceSuppressKeyForADifferentCityMatchesTheSwitchKey() {
        // User picks Warszawa at the gate while location placed them near
        // Poznań — the pair must match what `switchSuggestion` would produce,
        // so seeding it suppresses the immediate "you're nearer Poznań" prompt.
        let key = City.initialChoiceSuppressKey(chosenSlug: "warszawa", nearestSlug: "poznan")
        XCTAssertEqual(key, "warszawa→poznan")

        // End-to-end: feeding that key back as `lastPromptKey` suppresses the
        // suggestion the gate would otherwise raise from Poznań coordinates.
        let suppressed = City.switchSuggestion(
            chosenSlug: "warszawa",
            lat: 52.4064, lon: 16.9252,
            lastPromptKey: key
        )
        XCTAssertNil(suppressed)
    }

    func testInitialChoiceSuppressKeyIsNilWhenChosenCityIsTheNearest() {
        // Confirming the detected city — nothing to suppress.
        XCTAssertNil(City.initialChoiceSuppressKey(chosenSlug: "poznan", nearestSlug: "poznan"))
    }

    func testInitialChoiceSuppressKeyIsNilWithoutALocationFix() {
        // Location unavailable at the gate — no nearest, so a later legitimate
        // "you're nearer …" prompt must stay armed.
        XCTAssertNil(City.initialChoiceSuppressKey(chosenSlug: "warszawa", nearestSlug: nil))
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
