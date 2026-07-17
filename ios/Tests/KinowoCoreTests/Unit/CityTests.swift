import XCTest
@testable import KinowoCore

final class CityTests: XCTestCase {

    // ── nearestWithin100km (scoped to the selected country) ───────

    func testPoznanCoordsResolveToPoznan() {
        let city = City.nearestWithin100km(lat: 52.4064, lon: 16.9252, in: "PL")
        XCTAssertEqual(city?.slug, "poznan")
    }

    func testCoordsJustOutsidePoznanStillResolveWithin100km() {
        // ~40 km west of Poznań — well inside the radius.
        let city = City.nearestWithin100km(lat: 52.4064, lon: 16.3, in: "PL")
        XCTAssertEqual(city?.slug, "poznan")
    }

    func testEachSupportedCityResolvesFromItsOwnCoords() {
        XCTAssertEqual(City.nearestWithin100km(lat: 51.1079, lon: 17.0385, in: "PL")?.slug, "wroclaw")
        XCTAssertEqual(City.nearestWithin100km(lat: 52.2297, lon: 21.0122, in: "PL")?.slug, "warszawa")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.0647, lon: 19.9450, in: "PL")?.slug, "krakow")
        XCTAssertEqual(City.nearestWithin100km(lat: 51.7592, lon: 19.4560, in: "PL")?.slug, "lodz")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.2649, lon: 19.0238, in: "PL")?.slug, "katowice")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.4285, lon: 14.5528, in: "PL")?.slug, "szczecin")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.1325, lon: 23.1688, in: "PL")?.slug, "bialystok")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.1235, lon: 18.0084, in: "PL")?.slug, "bydgoszcz")
        XCTAssertEqual(City.nearestWithin100km(lat: 51.2465, lon: 22.5684, in: "PL")?.slug, "lublin")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.8118, lon: 19.1203, in: "PL")?.slug, "czestochowa")
        XCTAssertEqual(City.nearestWithin100km(lat: 51.4027, lon: 21.1471, in: "PL")?.slug, "radom")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.2863, lon: 19.1041, in: "PL")?.slug, "sosnowiec")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.0138, lon: 18.5984, in: "PL")?.slug, "torun")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.8661, lon: 20.6286, in: "PL")?.slug, "kielce")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.0413, lon: 21.9990, in: "PL")?.slug, "rzeszow")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.2945, lon: 18.6714, in: "PL")?.slug, "gliwice")
        XCTAssertEqual(City.nearestWithin100km(lat: 50.3249, lon: 18.7857, in: "PL")?.slug, "zabrze")
        // Anywhere in the Tri-City resolves to the combined Trójmiasto scope —
        // Gdańsk in the south, Gdynia in the north are both inside 100 km of
        // the Sopot-centred coordinate.
        XCTAssertEqual(City.nearestWithin100km(lat: 54.3520, lon: 18.6466, in: "PL")?.slug, "trojmiasto") // Gdańsk
        XCTAssertEqual(City.nearestWithin100km(lat: 54.5189, lon: 18.5305, in: "PL")?.slug, "trojmiasto") // Gdynia
    }

    func testCoordsFarFromEveryCityAreOutOfRange() {
        // Open Baltic, ~150 km north of Trójmiasto (its nearest served city) —
        // beyond the 100 km cutoff, so the gate falls back to a manual choice
        // rather than dropping the user on a far-off city.
        XCTAssertNil(City.nearestWithin100km(lat: 55.5, lon: 17.0, in: "PL"))
    }

    func testFarAwayCoordsReturnNil() {
        let city = City.nearestWithin100km(lat: 0, lon: 0, in: "PL")
        XCTAssertNil(city)
    }

    // ── UK cities + per-country isolation ─────────────────────────

    func testLondonCoordsResolveToLondonInGB() {
        XCTAssertEqual(City.nearestWithin100km(lat: 51.5074, lon: -0.1278, in: "GB")?.slug, "london")
        XCTAssertEqual(City.nearestWithin100km(lat: 53.4808, lon: -2.2426, in: "GB")?.slug, "manchester")
        XCTAssertEqual(City.nearestWithin100km(lat: 55.8682, lon: -4.2316, in: "GB")?.slug, "glasgow")
    }

    func testNearestIsScopedToTheSelectedCountry() {
        // A London fix must NOT resolve to any Polish city, and a Poznań fix
        // must NOT resolve to any UK region — each country's gate only ever
        // offers its own cities.
        XCTAssertNil(City.nearestWithin100km(lat: 51.5074, lon: -0.1278, in: "PL"))
        XCTAssertNil(City.nearestWithin100km(lat: 52.4064, lon: 16.9252, in: "GB"))
    }

    func testUkRosterIsTheFullSeventyNineRegions() {
        XCTAssertEqual(City.cities(in: "GB").count, 79)
        XCTAssertEqual(City.cities(in: "GB").first?.slug, "london")   // hand order
        // Every UK city carries the GB country code.
        XCTAssertTrue(City.cities(in: "GB").allSatisfy { $0.country == "GB" })
    }

    func testUkSortedIsAlphabeticalUnderEnglishCollation() {
        let sorted = City.sorted(in: "GB")
        XCTAssertEqual(sorted.first?.slug, "aberdeenshire")
        XCTAssertEqual(sorted.last?.slug, "yorkshire")
        XCTAssertEqual(Set(sorted.map(\.slug)), Set(City.cities(in: "GB").map(\.slug)))
    }

    func testUkMatchingSearchesUkCitiesOnly() {
        XCTAssertEqual(City.matching("manch", in: "GB").map(\.slug), ["manchester"])
        // A Polish query returns nothing under the UK scope.
        XCTAssertTrue(City.matching("poznan", in: "GB").isEmpty)
        // "york" surfaces the several Yorkshire regions, in English A→Z order.
        let yorks = City.matching("york", in: "GB").map(\.slug)
        XCTAssertTrue(yorks.contains("east-yorkshire"))
        XCTAssertTrue(yorks.contains("yorkshire"))
    }

    // ── default city (global + per-country) ───────────────────────

    func testDefaultIsFirstCity() {
        XCTAssertEqual(City.default.slug, City.all.first?.slug)
        XCTAssertEqual(City.default.slug, "poznan")
    }

    func testPerCountryDefaultIsThatCountrysFirstCity() {
        XCTAssertEqual(City.defaultCity(in: "PL").slug, "poznan")
        XCTAssertEqual(City.defaultCity(in: "GB").slug, "london")
    }

    // ── catalogue (global union, per-country order) ───────────────

    func testAllIsTheGlobalUnionOfPolishAndUkCities() {
        XCTAssertEqual(City.all.count, 120)                 // 41 PL + 79 GB
        XCTAssertEqual(City.cities(in: "PL").count, 41)
        XCTAssertEqual(City.cities(in: "GB").count, 79)
    }

    func testPolishCitiesArePresentInOrder() {
        XCTAssertEqual(City.cities(in: "PL").map(\.slug), [
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
        // Same cities as `cities(in: "PL")`, just reordered for the UI pickers.
        XCTAssertEqual(Set(City.sorted(in: "PL").map(\.slug)), Set(City.cities(in: "PL").map(\.slug)))
        XCTAssertEqual(City.sorted(in: "PL").map(\.slug), [
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
        // Polish-collation discriminator: a naive code-point sort puts "Łódź"
        // (Ł = U+0141) after every ASCII-initial name, i.e. near the very end.
        let slugs = City.sorted(in: "PL").map(\.slug)
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
            lastPromptKey: nil,
            in: "PL"
        )
        XCTAssertEqual(s?.target.slug, "wroclaw")
        XCTAssertEqual(s?.key, "poznan→wroclaw")
    }

    func testSwitchSuggestionSuppressedWhenAlreadyPromptedForThatPair() {
        let s = City.switchSuggestion(
            chosenSlug: "poznan",
            lat: 51.1079, lon: 17.0385,
            lastPromptKey: "poznan→wroclaw",
            in: "PL"
        )
        XCTAssertNil(s)
    }

    func testNoSwitchSuggestionWhenAlreadyInThatCity() {
        let s = City.switchSuggestion(
            chosenSlug: "wroclaw",
            lat: 51.1079, lon: 17.0385,
            lastPromptKey: nil,
            in: "PL"
        )
        XCTAssertNil(s)
    }

    func testNoSwitchSuggestionWhenOutOfRangeOfEveryCity() {
        // Open Baltic — beyond the 100 km radius of every supported city.
        let s = City.switchSuggestion(
            chosenSlug: "poznan",
            lat: 55.5, lon: 17.0,
            lastPromptKey: nil,
            in: "PL"
        )
        XCTAssertNil(s)
    }

    func testSwitchSuggestionIsCountryScoped() {
        // Chosen London, device in Manchester — under GB this suggests the
        // switch; a Polish scope sees no nearby city at all.
        let gb = City.switchSuggestion(
            chosenSlug: "london",
            lat: 53.4808, lon: -2.2426,
            lastPromptKey: nil,
            in: "GB"
        )
        XCTAssertEqual(gb?.target.slug, "manchester")
        XCTAssertNil(City.switchSuggestion(
            chosenSlug: "london",
            lat: 53.4808, lon: -2.2426,
            lastPromptKey: nil,
            in: "PL"
        ))
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
            lastPromptKey: key,
            in: "PL"
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

    // ── matching (city-picker search, per country) ────────────────

    func testBlankQueryMatchesEveryCity() {
        XCTAssertEqual(City.matching("", in: "PL").count, City.cities(in: "PL").count)
        XCTAssertEqual(City.matching("   ", in: "PL").count, City.cities(in: "PL").count)
    }

    func testQueryNarrowsToAMatchingPrefix() {
        let slugs = City.matching("wroc", in: "PL").map(\.slug)
        XCTAssertEqual(slugs, ["wroclaw"])
    }

    func testMatchIsCaseInsensitive() {
        XCTAssertEqual(City.matching("KRAKÓW", in: "PL").map(\.slug), ["krakow"])
    }

    func testMatchIsDiacriticInsensitiveTypedWithoutPolishLetters() {
        // The whole point: people type "lodz"/"krakow"/"poznan" on a plain
        // keyboard and still find the diacritic'd city.
        XCTAssertEqual(City.matching("lodz", in: "PL").map(\.slug), ["lodz"])
        XCTAssertEqual(City.matching("krakow", in: "PL").map(\.slug), ["krakow"])
        XCTAssertEqual(City.matching("gdansk", in: "PL").map(\.slug), [])   // Trójmiasto, no "Gdańsk" name
        XCTAssertTrue(City.matching("zielona gora", in: "PL").map(\.slug).contains("zielona-gora"))
    }

    func testMatchIsASubstringNotJustAPrefix() {
        // "gora" appears mid-name in "Zielona Góra", "Jelenia Góra",
        // "Dąbrowa Górnicza" — all should surface.
        let slugs = City.matching("gora", in: "PL").map(\.slug)
        XCTAssertTrue(slugs.contains("zielona-gora"))
        XCTAssertTrue(slugs.contains("jelenia-gora"))
    }

    func testNoMatchReturnsEmpty() {
        XCTAssertTrue(City.matching("zzzzz", in: "PL").isEmpty)
    }

    func testMatchingKeepsPolishAlphabeticalOrder() {
        // "Gliwice" (G) before "Łódź" (Ł) before "Opole" (O): the filter must
        // preserve the Polish collation, not reorder by match.
        let slugs = City.matching("l", in: "PL").map(\.slug)
        let g = slugs.firstIndex(of: "gliwice")
        let l = slugs.firstIndex(of: "lodz")
        let o = slugs.firstIndex(of: "opole")
        XCTAssertNotNil(g); XCTAssertNotNil(l); XCTAssertNotNil(o)
        XCTAssertTrue(g! < l!, "Gliwice should sort before Łódź")
        XCTAssertTrue(l! < o!, "Łódź should sort before Opole")
    }
}
