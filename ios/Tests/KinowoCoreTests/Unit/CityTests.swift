import XCTest
@testable import KinowoCore

/// The per-country city queries, exercised over the bundled fallback list
/// `City.all` (the app runs them over the live catalog's `cities`, same code).
final class CityTests: XCTestCase {

    // ── nearestWithin100km (scoped to the selected country) ───────

    func testPoznanCoordsResolveToPoznan() {
        XCTAssertEqual(City.all.nearestWithin100km(lat: 52.4064, lon: 16.9252, inCountry: "pl")?.slug, "poznan")
    }

    func testCoordsJustOutsidePoznanStillResolveWithin100km() {
        // ~40 km west of Poznań — well inside the radius.
        XCTAssertEqual(City.all.nearestWithin100km(lat: 52.4064, lon: 16.3, inCountry: "pl")?.slug, "poznan")
    }

    func testEachSupportedCityResolvesFromItsOwnCoords() {
        XCTAssertEqual(City.all.nearestWithin100km(lat: 51.1079, lon: 17.0385, inCountry: "pl")?.slug, "wroclaw")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 52.2297, lon: 21.0122, inCountry: "pl")?.slug, "warszawa")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 50.0647, lon: 19.9450, inCountry: "pl")?.slug, "krakow")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 51.7592, lon: 19.4560, inCountry: "pl")?.slug, "lodz")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 50.2649, lon: 19.0238, inCountry: "pl")?.slug, "katowice")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 53.4285, lon: 14.5528, inCountry: "pl")?.slug, "szczecin")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 53.1325, lon: 23.1688, inCountry: "pl")?.slug, "bialystok")
        // Anywhere in the Tri-City resolves to the combined Trójmiasto scope.
        XCTAssertEqual(City.all.nearestWithin100km(lat: 54.3520, lon: 18.6466, inCountry: "pl")?.slug, "trojmiasto") // Gdańsk
        XCTAssertEqual(City.all.nearestWithin100km(lat: 54.5189, lon: 18.5305, inCountry: "pl")?.slug, "trojmiasto") // Gdynia
    }

    func testCoordsFarFromEveryCityAreOutOfRange() {
        // Open Baltic, beyond the 100 km cutoff of every Polish city.
        XCTAssertNil(City.all.nearestWithin100km(lat: 55.5, lon: 17.0, inCountry: "pl"))
    }

    func testFarAwayCoordsReturnNil() {
        XCTAssertNil(City.all.nearestWithin100km(lat: 0, lon: 0, inCountry: "pl"))
    }

    // ── UK cities + per-country isolation ─────────────────────────

    func testLondonCoordsResolveToLondonInGB() {
        XCTAssertEqual(City.all.nearestWithin100km(lat: 51.5074, lon: -0.1278, inCountry: "uk")?.slug, "london")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 53.4808, lon: -2.2426, inCountry: "uk")?.slug, "manchester")
        XCTAssertEqual(City.all.nearestWithin100km(lat: 55.8682, lon: -4.2316, inCountry: "uk")?.slug, "glasgow")
    }

    func testNearestIsScopedToTheSelectedCountry() {
        // A London fix must NOT resolve to any Polish city, and a Poznań fix must
        // NOT resolve to any UK region.
        XCTAssertNil(City.all.nearestWithin100km(lat: 51.5074, lon: -0.1278, inCountry: "pl"))
        XCTAssertNil(City.all.nearestWithin100km(lat: 52.4064, lon: 16.9252, inCountry: "uk"))
    }

    func testUkRosterIsTheFullSeventyNineRegions() {
        XCTAssertEqual(City.all.inCountry("uk").count, 79)
        XCTAssertEqual(City.all.inCountry("uk").first?.slug, "london")   // hand order
        XCTAssertTrue(City.all.inCountry("uk").allSatisfy { $0.country == "uk" })
    }

    func testUkSortedIsAlphabeticalUnderEnglishCollation() {
        let sorted = City.all.sortedForPicker(inCountry: "uk")
        XCTAssertEqual(sorted.first?.slug, "aberdeenshire")
        XCTAssertEqual(sorted.last?.slug, "yorkshire")
        XCTAssertEqual(Set(sorted.map(\.slug)), Set(City.all.inCountry("uk").map(\.slug)))
    }

    func testUkMatchingSearchesUkCitiesOnly() {
        XCTAssertEqual(City.all.matching("manch", inCountry: "uk").map(\.slug), ["manchester"])
        XCTAssertTrue(City.all.matching("poznan", inCountry: "uk").isEmpty)
        let yorks = City.all.matching("york", inCountry: "uk").map(\.slug)
        XCTAssertTrue(yorks.contains("east-yorkshire"))
        XCTAssertTrue(yorks.contains("yorkshire"))
    }

    // ── default city (per country) ────────────────────────────────

    func testPerCountryDefaultIsThatCountrysFirstCity() {
        XCTAssertEqual(City.all.defaultCity(inCountry: "pl")?.slug, "poznan")
        XCTAssertEqual(City.all.defaultCity(inCountry: "uk")?.slug, "london")
        XCTAssertNil(City.all.defaultCity(inCountry: "zz"))
    }

    // ── catalogue (global union, per-country order) ───────────────

    func testAllIsTheGlobalUnionOfPolishAndUkCities() {
        XCTAssertEqual(City.all.count, 120)                 // 41 PL + 79 UK
        XCTAssertEqual(City.all.inCountry("pl").count, 41)
        XCTAssertEqual(City.all.inCountry("uk").count, 79)
    }

    func testPolishCitiesArePresentInOrder() {
        XCTAssertEqual(City.all.inCountry("pl").map(\.slug), [
            "poznan", "wroclaw", "warszawa", "krakow", "lodz", "katowice", "szczecin",
            "bialystok", "trojmiasto", "bydgoszcz", "lublin", "czestochowa", "radom",
            "sosnowiec", "torun", "kielce", "rzeszow", "gliwice", "zabrze",
            "olsztyn", "bielsko-biala", "opole", "rybnik", "gorzow-wielkopolski", "elblag",
            "koszalin", "kalisz", "zielona-gora", "tychy", "walbrzych", "tarnow", "wloclawek",
            "legnica", "plock", "bytom", "dabrowa-gornicza", "nowy-sacz", "slupsk",
            "jelenia-gora", "przemysl", "konin",
        ])
    }

    func testPolishSortedIsAlphabeticalUnderPolishCollation() {
        XCTAssertEqual(Set(City.all.sortedForPicker(inCountry: "pl").map(\.slug)), Set(City.all.inCountry("pl").map(\.slug)))
        XCTAssertEqual(City.all.sortedForPicker(inCountry: "pl").map(\.slug), [
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

    func testPolishSortedCollatesLAfterLNotAtTheEnd() {
        // "Łódź" (Ł = U+0141) sorts right after "Lublin", not at the very end.
        let slugs = City.all.sortedForPicker(inCountry: "pl").map(\.slug)
        XCTAssertEqual(slugs.firstIndex(of: "lodz"), slugs.firstIndex(of: "lublin").map { $0 + 1 })
        XCTAssertLessThan(slugs.firstIndex(of: "lodz")!, slugs.firstIndex(of: "zabrze")!)
    }

    // ── country(ofSlug:) (deep-link → right deployment) ───────────

    func testCountryOfSlugResolvesEachCitysCountry() {
        XCTAssertEqual(City.all.country(ofSlug: "poznan"), "pl")
        XCTAssertEqual(City.all.country(ofSlug: "london"), "uk")
    }

    func testCountryOfSlugIsNilForUnknownCity() {
        XCTAssertNil(City.all.country(ofSlug: "berlin"))   // DE arrives via the live catalog, not the fallback
        XCTAssertNil(City.all.country(ofSlug: "nope"))
    }

    // ── switchSuggestion (you're-nearer-another-city prompt) ──────

    func testSwitchSuggestionWhenNearerCityDiffers() {
        let s = City.all.switchSuggestion(chosenSlug: "poznan", lat: 51.1079, lon: 17.0385, lastPromptKey: nil, inCountry: "pl")
        XCTAssertEqual(s?.target.slug, "wroclaw")
        XCTAssertEqual(s?.key, "poznan→wroclaw")
    }

    func testSwitchSuggestionSuppressedWhenAlreadyPromptedForThatPair() {
        XCTAssertNil(City.all.switchSuggestion(chosenSlug: "poznan", lat: 51.1079, lon: 17.0385, lastPromptKey: "poznan→wroclaw", inCountry: "pl"))
    }

    func testNoSwitchSuggestionWhenAlreadyInThatCity() {
        XCTAssertNil(City.all.switchSuggestion(chosenSlug: "wroclaw", lat: 51.1079, lon: 17.0385, lastPromptKey: nil, inCountry: "pl"))
    }

    func testNoSwitchSuggestionWhenOutOfRangeOfEveryCity() {
        XCTAssertNil(City.all.switchSuggestion(chosenSlug: "poznan", lat: 55.5, lon: 17.0, lastPromptKey: nil, inCountry: "pl"))
    }

    func testSwitchSuggestionIsCountryScoped() {
        // Chosen London, device in Manchester — GB suggests the switch; a Polish
        // scope sees no nearby city.
        let gb = City.all.switchSuggestion(chosenSlug: "london", lat: 53.4808, lon: -2.2426, lastPromptKey: nil, inCountry: "uk")
        XCTAssertEqual(gb?.target.slug, "manchester")
        XCTAssertNil(City.all.switchSuggestion(chosenSlug: "london", lat: 53.4808, lon: -2.2426, lastPromptKey: nil, inCountry: "pl"))
    }

    // ── initialChoiceSuppressKey (deliberate first-launch pick) ───

    func testInitialChoiceSuppressKeyForADifferentCityMatchesTheSwitchKey() {
        let key = City.initialChoiceSuppressKey(chosenSlug: "warszawa", nearestSlug: "poznan")
        XCTAssertEqual(key, "warszawa→poznan")
        XCTAssertNil(City.all.switchSuggestion(chosenSlug: "warszawa", lat: 52.4064, lon: 16.9252, lastPromptKey: key, inCountry: "pl"))
    }

    func testInitialChoiceSuppressKeyIsNilWhenChosenCityIsTheNearest() {
        XCTAssertNil(City.initialChoiceSuppressKey(chosenSlug: "poznan", nearestSlug: "poznan"))
    }

    func testInitialChoiceSuppressKeyIsNilWithoutALocationFix() {
        XCTAssertNil(City.initialChoiceSuppressKey(chosenSlug: "warszawa", nearestSlug: nil))
    }

    // ── apiURL (city-prefixed endpoints) ──────────────────────────

    func testRepertoireURLIsCityPrefixed() {
        let url = City.apiURL(base: URL(string: "https://kinowo.fly.dev")!, slug: "poznan", endpoint: "repertoire")
        XCTAssertEqual(url.absoluteString, "https://kinowo.fly.dev/poznan/api/repertoire")
    }

    func testApiURLHonoursADifferentSlug() {
        let url = City.apiURL(base: URL(string: "https://kinowo.fly.dev")!, slug: "warszawa", endpoint: "repertoire")
        XCTAssertEqual(url.absoluteString, "https://kinowo.fly.dev/warszawa/api/repertoire")
    }

    // ── matching (city-picker search, per country) ────────────────

    func testBlankQueryMatchesEveryCity() {
        XCTAssertEqual(City.all.matching("", inCountry: "pl").count, City.all.inCountry("pl").count)
        XCTAssertEqual(City.all.matching("   ", inCountry: "pl").count, City.all.inCountry("pl").count)
    }

    func testQueryNarrowsToAMatchingPrefix() {
        XCTAssertEqual(City.all.matching("wroc", inCountry: "pl").map(\.slug), ["wroclaw"])
    }

    func testMatchIsDiacriticInsensitiveTypedWithoutPolishLetters() {
        XCTAssertEqual(City.all.matching("lodz", inCountry: "pl").map(\.slug), ["lodz"])
        XCTAssertEqual(City.all.matching("krakow", inCountry: "pl").map(\.slug), ["krakow"])
        XCTAssertEqual(City.all.matching("gdansk", inCountry: "pl").map(\.slug), [])   // Trójmiasto, no "Gdańsk" name
        XCTAssertTrue(City.all.matching("zielona gora", inCountry: "pl").map(\.slug).contains("zielona-gora"))
    }

    func testMatchingKeepsPolishAlphabeticalOrder() {
        // Gliwice (G) before Łódź (Ł) before Opole (O): the filter preserves the collation.
        let slugs = City.all.matching("l", inCountry: "pl").map(\.slug)
        XCTAssertTrue(slugs.firstIndex(of: "gliwice")! < slugs.firstIndex(of: "lodz")!)
        XCTAssertTrue(slugs.firstIndex(of: "lodz")! < slugs.firstIndex(of: "opole")!)
    }
}
