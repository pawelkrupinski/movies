import XCTest
@testable import KinowoCore

/// `DeepLink.parse` is the inverse of the web's `buildShareURL()` + the iOS
/// `FilmShareLink.url`: every kinowo.net URL the site can produce must round
/// back into the right city / film / filter state, and anything that ISN'T a
/// city link (OAuth callbacks, unknown hosts, foreign paths) must be rejected so
/// `onOpenURL` no-ops rather than navigating somewhere wrong.
final class DeepLinkTests: XCTestCase {

    private func parse(_ s: String) -> DeepLink? {
        DeepLink.parse(URL(string: s)!)
    }

    // MARK: city + film identity

    func testCityListingLink() {
        let dl = parse("https://kinowo.net/poznan/")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertNil(dl?.filmTitle)
        XCTAssertTrue(dl?.filters.isEmpty ?? false)
    }

    func testFilmDetailSlugLink() {
        // The canonical form the site and the app both mint now.
        let dl = parse("https://kinowo.net/warszawa/movie/oppenheimer")
        XCTAssertEqual(dl?.citySlug, "warszawa")
        XCTAssertEqual(dl?.filmSlug, "oppenheimer")
        XCTAssertNil(dl?.filmTitle)
    }

    func testPreRenameFilmSegmentStillOpensTheDetailScreen() {
        // `/film` was the address before the rename to `/movie`, and it is what
        // every link shared before it — and every installed app build's share
        // button — still mints. The server 301s it, but a Universal Link is
        // handed to the app without ever reaching the server, so the parser is
        // the only thing standing between an old link and a no-op open.
        let slug = parse("https://kinowo.net/warszawa/film/oppenheimer")
        XCTAssertEqual(slug?.citySlug, "warszawa")
        XCTAssertEqual(slug?.filmSlug, "oppenheimer")

        let title = parse("kinowo://poznan/film?title=Oppenheimer")
        XCTAssertEqual(title?.citySlug, "poznan")
        XCTAssertEqual(title?.filmTitle, "Oppenheimer")
    }

    func testFilmDetailSlugLinkOnTheCustomScheme() {
        let dl = parse("kinowo://poznan/movie/diuna-czesc-druga")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertEqual(dl?.filmSlug, "diuna-czesc-druga")
    }

    func testFilmDetailLink() {
        // The legacy form: still parsed, because links shared before the switch
        // — and by app builds still in the wild — carry it.
        let dl = parse("https://kinowo.net/warszawa/movie?title=Oppenheimer")
        XCTAssertEqual(dl?.citySlug, "warszawa")
        XCTAssertEqual(dl?.filmTitle, "Oppenheimer")
        XCTAssertNil(dl?.filmSlug)
    }

    func testBareFilmPathIsNeitherSlugNorTitle() {
        let dl = parse("https://kinowo.net/warszawa/movie")
        XCTAssertEqual(dl?.citySlug, "warszawa")
        XCTAssertNil(dl?.filmSlug)
        XCTAssertNil(dl?.filmTitle)
    }

    func testFilmDetailDecodesEncodedTitle() {
        let dl = parse("https://kinowo.net/wroclaw/movie?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga")
        XCTAssertEqual(dl?.filmTitle, "Diuna: Część druga")
    }

    func testCustomSchemeCity() {
        let dl = parse("kinowo://poznan/")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertNil(dl?.filmTitle)
    }

    func testCustomSchemeFilm() {
        let dl = parse("kinowo://krakow/movie?title=Wicked")
        XCTAssertEqual(dl?.citySlug, "krakow")
        XCTAssertEqual(dl?.filmTitle, "Wicked")
    }

    // MARK: rejection

    func testRejectsOAuthCallback() {
        XCTAssertNil(parse("https://kinowo.net/auth/google/callback?code=abc"))
        XCTAssertNil(parse("kinowo://auth-done?code=abc"))
    }

    func testRejectsUnknownCity() {
        XCTAssertNil(parse("https://kinowo.net/uptime"))
        XCTAssertNil(parse("https://kinowo.net/nieznane-miasto/"))
    }

    func testRejectsForeignHostAndScheme() {
        XCTAssertNil(parse("https://evil.example.com/poznan/"))
        XCTAssertNil(parse("mailto:hi@kinowo.net"))
    }

    // MARK: the shared brand host, where the country is a PATH segment

    func testUKCountrySegmentOpensInApp() {
        let dl = parse("https://showtimes.cc/uk/london/movie?title=Wicked")
        XCTAssertEqual(dl?.citySlug, "london")
        XCTAssertEqual(dl?.filmTitle, "Wicked")
    }

    func testDECountrySegmentOpensInApp() {
        // No German city ships in the compile-time `City.all` fallback (they
        // arrive via the live catalog), so pass the slug set the app hands in at
        // runtime (`catalog.allSlugs`) — the same call `handleDeepLink` makes.
        let dl = DeepLink.parse(URL(string: "https://showtimes.cc/de/berlin/")!, knownCitySlugs: ["berlin"])
        XCTAssertEqual(dl?.citySlug, "berlin")
    }

    func testUSCountrySegmentOpensInApp() {
        // Like Germany's, the US regions arrive via the live catalog rather than
        // the compile-time `City.all`, so pass the runtime slug set.
        let dl = DeepLink.parse(URL(string: "https://showtimes.cc/us/california/movie/wicked")!,
                                knownCitySlugs: ["california"])
        XCTAssertEqual(dl?.citySlug, "california")
        XCTAssertEqual(dl?.filmSlug, "wicked")
    }

    func testESCountrySegmentOpensInApp() {
        // Spain's provinces arrive via the live catalog too, so pass the runtime
        // slug set the way the German and US cases do.
        let dl = DeepLink.parse(URL(string: "https://showtimes.cc/es/madrid/movie/la-odisea")!,
                                knownCitySlugs: ["madrid"])
        XCTAssertEqual(dl?.citySlug, "madrid")
        XCTAssertEqual(dl?.filmSlug, "la-odisea")
    }

    /// The subdomains the Showtimes countries used to answer on are gone — they
    /// stop serving outright rather than redirecting, so a link to one must not
    /// be claimed by the app either (the browser is what shows the failure).
    func testRetiredCountrySubdomainIsRejected() {
        XCTAssertNil(parse("https://uk.showtimes.cc/london/"))
        XCTAssertNil(DeepLink.parse(URL(string: "https://de.showtimes.cc/berlin/")!,
                                    knownCitySlugs: ["berlin"]))
    }

    /// The country segment is dropped only when it ISN'T also a city the build
    /// knows, so a country whose roster ever gained such a slug still resolves —
    /// and Poland's own one-segment links can't lose their city to a
    /// coincidence.
    func testCountrySegmentStrippingLeavesAOneSegmentLinkAlone() {
        let dl = parse("https://kinowo.net/poznan/movie/oppenheimer")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertEqual(dl?.filmSlug, "oppenheimer")

        let asCity = DeepLink.parse(URL(string: "https://showtimes.cc/us/")!, knownCitySlugs: ["us"])
        XCTAssertEqual(asCity?.citySlug, "us")
    }

    func testFrontDoorIsNotACityLink() {
        // `showtimes.cc/` is the country picker: no city, nothing to open.
        XCTAssertNil(parse("https://showtimes.cc/"))
    }

    func testEmptyTitleParamIsNoFilm() {
        XCTAssertNil(parse("https://kinowo.net/poznan/movie?title=")?.filmTitle)
    }

    // MARK: scalar filters

    func testScalarFilters() {
        let f = parse("https://kinowo.net/poznan/?date=tomorrow&q=duna&dim=2D&lang=NAP&imax=1&from=18:30&sort=rating")!.filters
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
        XCTAssertEqual(parse("https://kinowo.net/poznan/?date=2026-07-01")!.filters.date, .specific("2026-07-01"))
    }

    func testFormatFilterMergesOntoBase() {
        let f = parse("https://kinowo.net/poznan/?dim=3D")!.filters
        // Only the dim axis is set; an existing language on the base survives.
        let merged = f.formatFilter(base: FormatFilter(language: "DUB"))
        XCTAssertEqual(merged.dimension, "3D")
        XCTAssertEqual(merged.language, "DUB")
    }

    func testRejectsGarbageScalarValues() {
        let f = parse("https://kinowo.net/poznan/?dim=4D&lang=XX&from=99:99&date=lolwut")!.filters
        XCTAssertNil(f.dimension)
        XCTAssertNil(f.language)
        XCTAssertNil(f.fromHour)
        XCTAssertNil(f.date)
    }

    // MARK: multi-value inclusion → exclusion

    func testRepeatedAndCommaListInclusionFlatten() {
        let repeated = parse("https://kinowo.net/poznan/?genre=Komedia&genre=Dramat")!.filters
        let comma = parse("https://kinowo.net/poznan/?genre=Komedia,Dramat")!.filters
        XCTAssertEqual(Set(repeated.includedGenres), ["Komedia", "Dramat"])
        XCTAssertEqual(Set(comma.includedGenres), ["Komedia", "Dramat"])
    }

    func testInclusionConvertsToExclusionAgainstUniverse() {
        let f = parse("https://kinowo.net/poznan/?country=USA&country=Polska")!.filters
        let universe: Set<String> = ["USA", "Polska", "Francja", "Niemcy"]
        // Keep USA + Polska → exclude everything else.
        XCTAssertEqual(f.excluded(f.includedCountries, universe: universe), ["Francja", "Niemcy"])
    }

    func testEmptyInclusionMeansNoExclusion() {
        let f = parse("https://kinowo.net/poznan/")!.filters
        XCTAssertEqual(f.excluded(f.includedCountries, universe: ["USA", "Polska"]), [])
    }

    func testCinemaParamInvertsToDisabledSet() {
        let f = parse("https://kinowo.net/poznan/?cinema=Kino%20Muza&cinema=Rialto")!.filters
        let all: Set<String> = ["Kino Muza", "Rialto", "Multikino", "Apollo"]
        XCTAssertEqual(f.disabledCinemas(allCinemas: all), ["Multikino", "Apollo"])
    }

    func testAbsentCinemaParamLeavesChoiceAlone() {
        let f = parse("https://kinowo.net/poznan/?dim=2D")!.filters
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
