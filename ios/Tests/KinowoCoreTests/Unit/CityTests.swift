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
        XCTAssertEqual(City.nearestWithin100km(lat: 50.0647, lon: 19.9450)?.slug, "krakow")
        XCTAssertEqual(City.nearestWithin100km(lat: 51.7592, lon: 19.4560)?.slug, "lodz")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.2649, lon: 19.0238)?.slug, "katowice")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.4285, lon: 14.5528)?.slug, "szczecin")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.1325, lon: 23.1688)?.slug, "bialystok")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.1235, lon: 18.0084)?.slug, "bydgoszcz")
        XCTAssertEqual(City.nearestWithin100km(lat: 51.2465, lon: 22.5684)?.slug, "lublin")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.8118, lon: 19.1203)?.slug, "czestochowa")
        XCTAssertEqual(City.nearestWithin100km(lat: 51.4027, lon: 21.1471)?.slug, "radom")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.2863, lon: 19.1041)?.slug, "sosnowiec")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.0138, lon: 18.5984)?.slug, "torun")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.8661, lon: 20.6286)?.slug, "kielce")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.0413, lon: 21.9990)?.slug, "rzeszow")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.2945, lon: 18.6714)?.slug, "gliwice")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.3249, lon: 18.7857)?.slug, "zabrze")
        // Anywhere in the Tri-City resolves to the combined Trójmiasto scope —
        // Gdańsk in the south, Gdynia in the north are both inside 100 km of
        // the Sopot-centred coordinate.
        XCTAssertEqual(City.nearestWithin100km(lat: 54.3520, lon: 18.6466)?.slug, "trojmiasto") // Gdańsk
        XCTAssertEqual(City.nearestWithin100km(lat: 54.5189, lon: 18.5305)?.slug, "trojmiasto") // Gdynia
    }

    func testCoordsFarFromEveryCityAreOutOfRange() {
        // Open Baltic, ~150 km north of Trójmiasto (its nearest served city) —
        // beyond the 100 km cutoff, so the gate falls back to a manual choice
        // rather than dropping the user on a far-off city.
        XCTAssertNil(City.nearestWithin100km(lat: 55.5, lon: 17.0))
    }

    func testFarAwayCoordsReturnNil() {
        let city = City.nearestWithin100km(lat: 0, lon: 0)
        XCTAssertNil(city)
    }

    func testDefaultIsFirstCity() {
        XCTAssertEqual(City.default.slug, City.all.first?.slug)
        XCTAssertEqual(City.default.slug, "poznan")
    }

    func testAllFortyOneCitiesArePresentInOrder() {
        XCTAssertEqual(City.all.map(\.slug), [
            "poznan", "wroclaw", "warszawa", "krakow", "lodz", "katowice", "szczecin",
            "bialystok", "trojmiasto", "bydgoszcz", "lublin", "czestochowa", "radom",
            "sosnowiec", "torun", "kielce", "rzeszow", "gliwice", "zabrze",
            "olsztyn", "bielsko-biala", "opole", "rybnik", "gorzow-wielkopolski", "elblag",
            "koszalin", "kalisz", "zielona-gora", "tychy", "walbrzych", "tarnow", "wloclawek",
            "legnica", "plock", "bytom", "dabrowa-gornicza", "nowy-sacz", "slupsk",
            "jelenia-gora", "przemysl", "konin",
        ])
    }

    func testAllSortedIsAlphabeticalUnderPolishCollation() {
        // Same cities as `all`, just reordered for the UI pickers.
        XCTAssertEqual(Set(City.allSorted.map(\.slug)), Set(City.all.map(\.slug)))
        XCTAssertEqual(City.allSorted.map(\.slug), [
            "bialystok", "bielsko-biala", "bydgoszcz", "bytom", "czestochowa",
            "dabrowa-gornicza", "elblag", "gliwice", "gorzow-wielkopolski", "jelenia-gora",
            "kalisz", "katowice", "kielce", "konin", "koszalin", "krakow",
            "legnica", "lublin", "lodz", "nowy-sacz", "olsztyn", "opole",
            "plock", "poznan", "przemysl", "radom", "rybnik", "rzeszow",
            "slupsk", "sosnowiec", "szczecin", "tarnow", "torun", "trojmiasto",
            "tychy", "walbrzych", "warszawa", "wloclawek", "wroclaw", "zabrze",
            "zielona-gora",
        ])
    }

    func testAllSortedCollatesLAfterLNotAtTheEnd() {
        // Polish-collation discriminator: a naive code-point sort puts "Łódź"
        // (Ł = U+0141) after every ASCII-initial name, i.e. near the very end.
        let slugs = City.allSorted.map(\.slug)
        XCTAssertEqual(slugs.firstIndex(of: "lodz"), slugs.firstIndex(of: "lublin").map { $0 + 1 })
        XCTAssertLessThan(slugs.firstIndex(of: "lodz")!, slugs.firstIndex(of: "zabrze")!)
    }

    // ── cinema pill names (full map mirrors web Cinema.pillMap) ────

    func testPillNameShortensCinemasBeyondPoznan() {
        // A cinema outside the original Poznań ten now resolves to its short
        // pill label instead of falling back to the full name.
        XCTAssertEqual(CinemaSection.pillName(for: "Cinema City Wroclavia"), "Wroclavia")
        XCTAssertEqual(CinemaSection.pillName(for: "Multikino Złote Tarasy"), "Złote Tarasy")
        XCTAssertEqual(CinemaSection.pillName(for: "Kino Zorza"), "Zorza")
    }

    func testPillNameFallsBackToTheFullNameWhenUnmapped() {
        XCTAssertEqual(CinemaSection.pillName(for: "Some New Kino"), "Some New Kino")
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
        // Open Baltic — beyond the 100 km radius of every supported city.
        let s = City.switchSuggestion(
            chosenSlug: "poznan",
            lat: 55.5, lon: 17.0,
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
