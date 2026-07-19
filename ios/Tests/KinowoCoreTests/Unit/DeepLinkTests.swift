import XCTest
@testable import KinowoCore

/// `DeepLink.parse` is the inverse of the web's `buildShareURL()` + the iOS
/// `FilmShareLink.url`: every kinowo.fly.dev URL the site can produce must round
/// back into the right city / film / filter state, and anything that ISN'T a
/// city link (OAuth callbacks, unknown hosts, foreign paths) must be rejected so
/// `onOpenURL` no-ops rather than navigating somewhere wrong.
final class DeepLinkTests: XCTestCase {

    private func parse(_ s: String) -> DeepLink? {
        DeepLink.parse(URL(string: s)!)
    }

    // MARK: city + film identity

    func testCityListingLink() {
        let dl = parse("https://kinowo.fly.dev/poznan/")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertNil(dl?.filmTitle)
        XCTAssertTrue(dl?.filters.isEmpty ?? false)
    }

    func testFilmDetailLink() {
        let dl = parse("https://kinowo.fly.dev/warszawa/film?title=Oppenheimer")
        XCTAssertEqual(dl?.citySlug, "warszawa")
        XCTAssertEqual(dl?.filmTitle, "Oppenheimer")
    }

    func testFilmDetailDecodesEncodedTitle() {
        let dl = parse("https://kinowo.fly.dev/wroclaw/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga")
        XCTAssertEqual(dl?.filmTitle, "Diuna: Część druga")
    }

    func testCustomSchemeCity() {
        let dl = parse("kinowo://poznan/")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertNil(dl?.filmTitle)
    }

    func testCustomSchemeFilm() {
        let dl = parse("kinowo://krakow/film?title=Wicked")
        XCTAssertEqual(dl?.citySlug, "krakow")
        XCTAssertEqual(dl?.filmTitle, "Wicked")
    }

    // MARK: rejection

    func testRejectsOAuthCallback() {
        XCTAssertNil(parse("https://kinowo.fly.dev/auth/google/callback?code=abc"))
        XCTAssertNil(parse("kinowo://auth-done?code=abc"))
    }

    func testRejectsUnknownCity() {
        XCTAssertNil(parse("https://kinowo.fly.dev/uptime"))
        XCTAssertNil(parse("https://kinowo.fly.dev/nieznane-miasto/"))
    }

    func testRejectsForeignHostAndScheme() {
        XCTAssertNil(parse("https://evil.example.com/poznan/"))
        XCTAssertNil(parse("mailto:hi@kinowo.fly.dev"))
    }

    // MARK: per-country deployment hosts

    func testUKDeploymentHostOpensInApp() {
        let dl = parse("https://showtimes-uk.fly.dev/london/film?title=Wicked")
        XCTAssertEqual(dl?.citySlug, "london")
        XCTAssertEqual(dl?.filmTitle, "Wicked")
    }

    func testDEDeploymentHostOpensInApp() {
        // No German city ships in the compile-time `City.all` fallback (they
        // arrive via the live catalog), so pass the slug set the app hands in at
        // runtime (`catalog.allSlugs`) — the same call `handleDeepLink` makes.
        let dl = DeepLink.parse(URL(string: "https://showtimes-de.fly.dev/berlin/")!, knownCitySlugs: ["berlin"])
        XCTAssertEqual(dl?.citySlug, "berlin")
    }

    func testEmptyTitleParamIsNoFilm() {
        XCTAssertNil(parse("https://kinowo.fly.dev/poznan/film?title=")?.filmTitle)
    }

    // MARK: scalar filters

    func testScalarFilters() {
        let f = parse("https://kinowo.fly.dev/poznan/?date=tomorrow&q=duna&dim=2D&lang=NAP&imax=1&from=18:30&sort=rating")!.filters
        XCTAssertEqual(f.date, .tomorrow)
        XCTAssertEqual(f.query, "duna")
        XCTAssertEqual(f.dimension, "2D")
        XCTAssertEqual(f.language, "NAP")
        XCTAssertEqual(f.imax, true)
        XCTAssertEqual(f.fromHour, 18)
        XCTAssertEqual(f.fromMinute, 30)
        XCTAssertEqual(f.sort, .rating)
    }

    func testIsoDateFilter() {
        XCTAssertEqual(parse("https://kinowo.fly.dev/poznan/?date=2026-07-01")!.filters.date, .specific("2026-07-01"))
    }

    func testFormatFilterMergesOntoBase() {
        let f = parse("https://kinowo.fly.dev/poznan/?dim=3D")!.filters
        // Only the dim axis is set; an existing language on the base survives.
        let merged = f.formatFilter(base: FormatFilter(language: "DUB"))
        XCTAssertEqual(merged.dimension, "3D")
        XCTAssertEqual(merged.language, "DUB")
    }

    func testRejectsGarbageScalarValues() {
        let f = parse("https://kinowo.fly.dev/poznan/?dim=4D&lang=XX&from=99:99&date=lolwut")!.filters
        XCTAssertNil(f.dimension)
        XCTAssertNil(f.language)
        XCTAssertNil(f.fromHour)
        XCTAssertNil(f.date)
    }

    // MARK: multi-value inclusion → exclusion

    func testRepeatedAndCommaListInclusionFlatten() {
        let repeated = parse("https://kinowo.fly.dev/poznan/?genre=Komedia&genre=Dramat")!.filters
        let comma = parse("https://kinowo.fly.dev/poznan/?genre=Komedia,Dramat")!.filters
        XCTAssertEqual(Set(repeated.includedGenres), ["Komedia", "Dramat"])
        XCTAssertEqual(Set(comma.includedGenres), ["Komedia", "Dramat"])
    }

    func testInclusionConvertsToExclusionAgainstUniverse() {
        let f = parse("https://kinowo.fly.dev/poznan/?country=USA&country=Polska")!.filters
        let universe: Set<String> = ["USA", "Polska", "Francja", "Niemcy"]
        // Keep USA + Polska → exclude everything else.
        XCTAssertEqual(f.excluded(f.includedCountries, universe: universe), ["Francja", "Niemcy"])
    }

    func testEmptyInclusionMeansNoExclusion() {
        let f = parse("https://kinowo.fly.dev/poznan/")!.filters
        XCTAssertEqual(f.excluded(f.includedCountries, universe: ["USA", "Polska"]), [])
    }

    func testCinemaParamInvertsToDisabledSet() {
        let f = parse("https://kinowo.fly.dev/poznan/?cinema=Kino%20Muza&cinema=Rialto")!.filters
        let all: Set<String> = ["Kino Muza", "Rialto", "Multikino", "Apollo"]
        XCTAssertEqual(f.disabledCinemas(allCinemas: all), ["Multikino", "Apollo"])
    }

    func testAbsentCinemaParamLeavesChoiceAlone() {
        let f = parse("https://kinowo.fly.dev/poznan/?dim=2D")!.filters
        XCTAssertNil(f.enabledCinemas)
        XCTAssertNil(f.disabledCinemas(allCinemas: ["A", "B"]))
    }

    // MARK: title normalization (matches web TitleNormalizer.normalize)

    func testNormalizeFoldsArabicNumeralsToRoman() {
        XCTAssertEqual(DeepLinkTitle.normalize("Diabeł ubiera się u Prady 2"), "Diabeł ubiera się u Prady II")
        XCTAssertEqual(DeepLinkTitle.normalize("Mortal Kombat 2"), "Mortal Kombat II")
        // Only standalone-numeral WORDS fold; digits inside a word stay.
        XCTAssertEqual(DeepLinkTitle.normalize("Blade Runner 2049"), "Blade Runner 2049")
    }

    func testNumberedTitleMatchesAcrossArabicAndRoman() {
        XCTAssertTrue(DeepLinkTitle.matches("…Prady 2", "…Prady II"))
        XCTAssertFalse(DeepLinkTitle.matches("Dune 2", "Dune"))
    }
}
