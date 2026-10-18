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

    /// One city per slug in `country`, standing in for the live catalog's
    /// rows of a country the compile-time `City.all` doesn't ship.
    private func catalog(_ country: String, _ slugs: String...) -> [City] {
        slugs.map { City(slug: $0, name: $0, lat: 0, lon: 0, country: country) }
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

    func testARetiredCitysLinkOpensTheCityThatHoldsItNow() {
        // The web 301s `/miedzyrzec-podlaski/…` onto Biała Podlaska, but a
        // Universal Link never reaches that redirect: the catalog names the
        // former slug on its successor, and the link opens there.
        let cities = try! JSONDecoder().decode([City].self, from: Data(#"[{"slug":"biala-podlaska","name":"Biała Podlaska i okolice","lat":52.0,"lon":23.1,"country":"pl","formerSlugs":["miedzyrzec-podlaski"]}]"#.utf8))
        let dl = DeepLink.parse(URL(string: "https://kinowo.net/miedzyrzec-podlaski/movie/oppenheimer?date=tomorrow")!,
                                cities: cities)
        XCTAssertEqual(dl?.citySlug, "biala-podlaska")
        XCTAssertEqual(dl?.filmSlug, "oppenheimer")
        XCTAssertEqual(dl?.filters.date, .tomorrow)
        // A slug nobody ever answered at is still no city.
        XCTAssertNil(DeepLink.parse(URL(string: "https://kinowo.net/nowhere/")!, cities: cities))
    }

    /// Every country owns its slugs: the link's path segment picks the country,
    /// and a slug two countries share opens the one the URL names.
    func testASharedSlugOpensTheCountryTheLinkNames() throws {
        let cities = try JSONDecoder().decode([City].self, from: Data(#"""
        [{"slug":"york","name":"York","lat":53.96,"lon":-1.08,"country":"uk"},
         {"slug":"kent","name":"Kent","lat":47.38,"lon":-122.23,"country":"us"},
         {"slug":"chatham","name":"Chatham area","lat":51.37,"lon":0.53,"country":"uk","formerSlugs":["kent"]},
         {"slug":"york","name":"York","lat":39.96,"lon":-76.73,"country":"us"}]
        """#.utf8))
        let us = DeepLink.parse(URL(string: "https://showtimes.cc/us/york/movie/dune")!, cities: cities)
        XCTAssertEqual(us?.countryCode, "us")
        XCTAssertEqual(us?.citySlug, "york")
        XCTAssertEqual(DeepLink.parse(URL(string: "https://showtimes.cc/uk/york/")!, cities: cities)?.countryCode, "uk")
        // A slug that is one country's live city and another's former one opens the country the URL names.
        XCTAssertEqual(DeepLink.parse(URL(string: "https://showtimes.cc/us/kent/")!, cities: cities)?.citySlug, "kent")
        let ukKent = DeepLink.parse(URL(string: "https://showtimes.cc/uk/kent/")!, cities: cities)
        XCTAssertEqual(ukKent?.citySlug, "chatham")
        XCTAssertEqual(ukKent?.countryCode, "uk")
        // A slug the named country does not have is no city, whoever else has it.
        XCTAssertNil(DeepLink.parse(URL(string: "https://showtimes.cc/de/york/")!, cities: cities))
        // The version pair is the resolved country's.
        let german: (String) -> Set<String> = { $0 == "us" ? ["SUB", "DUB"] : ["OmU", "DF"] }
        XCTAssertEqual(DeepLink.parse(URL(string: "https://showtimes.cc/us/york/?lang=SUB")!, cities: cities,
                                      languageTokensOfCountry: german)?.filters.language, "SUB")
    }

    /// The bundled seed — the server's catalog — decoded as the app decodes it.
    /// (`CatalogStore`, which owns the decode, is app-target only.)
    private func seed() throws -> (cities: [City], owners: [String: String]) {
        struct Seed: Decodable {
            struct Body: Decodable { let cities: [City]; let bareSlugOwners: [String: String] }
            let catalog: Body
        }
        let body = try JSONDecoder().decode(Seed.self, from: try AppSources.data("Kinowo/catalog-seed.json")).catalog
        return (body.cities, body.bareSlugOwners)
    }

    /// A slug the UK retired that the US has live: the catalog's `formerSlugs`
    /// leaves it out (it protects older builds), and this build reads `otherSlugs`,
    /// so `/uk/kent/` opens the UK page the web 301s it to.
    func testARetiredSlugAnotherCountryHasLiveOpensTheNamedCountrysPage() throws {
        let (cities, owners) = try seed()
        let link = DeepLink.parse(URL(string: "https://showtimes.cc/uk/kent/movie/dune")!, cities: cities, bareSlugOwners: owners)
        XCTAssertEqual(link?.countryCode, "uk")
        XCTAssertEqual(link?.citySlug, "chatham")
        XCTAssertEqual(link?.filmSlug, "dune")
        XCTAssertEqual(cities.currentSlugByFormer(inCountry: "uk")["kent"], "chatham")
        XCTAssertNil(cities.currentSlugByFormer(inCountry: "us")["kent"])
    }

    /// A town a page covers is a URL the Universal Links claim and the web 301s
    /// onto the page: the app opens that page.
    func testATownAliasOpensThePageCoveringIt() throws {
        let (cities, owners) = try seed()
        let link = DeepLink.parse(URL(string: "https://showtimes.cc/uk/ilkley/")!, cities: cities, bareSlugOwners: owners)
        XCTAssertEqual(link?.countryCode, "uk")
        XCTAssertNotNil(link.flatMap { cities.city(slug: $0.citySlug, inCountry: "uk") })
        XCTAssertNotEqual(link?.citySlug, "ilkley")
        // A qualified slug too.
        XCTAssertEqual(DeepLink.parse(URL(string: "https://showtimes.cc/uk/york-north-yorkshire/")!, cities: cities,
                                      bareSlugOwners: owners)?.citySlug, "york")
    }

    /// A page slug two countries share, in the real catalog: the catalog names the
    /// UK's York `york-north-yorkshire` for installed builds, and this build reads
    /// its `pageSlug` — so `/uk/york/` opens the UK's York and `/us/york/` Pennsylvania's.
    func testASharedPageSlugOpensEachCountrysOwnPageInTheBundledCatalog() throws {
        let (cities, owners) = try seed()
        let uk = DeepLink.parse(URL(string: "https://showtimes.cc/uk/york/")!, cities: cities, bareSlugOwners: owners)
        XCTAssertEqual(uk?.countryCode, "uk")
        XCTAssertEqual(uk?.citySlug, "york")
        XCTAssertEqual(cities.city(slug: "york", inCountry: "uk")?.lat ?? 0, 53.96, accuracy: 0.1)
        let us = DeepLink.parse(URL(string: "https://showtimes.cc/us/york/movie/dune")!, cities: cities, bareSlugOwners: owners)
        XCTAssertEqual(us?.countryCode, "us")
        XCTAssertEqual(us?.citySlug, "york")
        XCTAssertEqual(cities.city(slug: "york", inCountry: "us")?.lat ?? 0, 39.96, accuracy: 0.1)
    }

    /// A bare `showtimes.cc/{slug}/` or `kinowo://{slug}` names no country: the
    /// country the server's old-owner table names (shipped in the catalog), else
    /// the first in catalog order that answers to it — as the web's front door.
    func testABareSlugOpensTheCountryTheServersTableNames() throws {
        let (cities, owners) = try seed()
        func country(_ url: String) -> String? {
            DeepLink.parse(URL(string: url)!, cities: cities, bareSlugOwners: owners)?.countryCode
        }
        // The old owner, though the UK has a York page now (catalog order would say UK).
        XCTAssertEqual(owners["york"], "us")
        XCTAssertEqual(country("https://showtimes.cc/york/"), "us")
        XCTAssertEqual(country("kinowo://york/movie/dune"), "us")
        XCTAssertEqual(country("https://showtimes.cc/somerset/"), "uk")
        XCTAssertEqual(country("kinowo://poznan/"), "pl")
        // Every entry opens where the table says.
        for (slug, code) in owners {
            XCTAssertEqual(country("https://showtimes.cc/\(slug)/"), code, slug)
            XCTAssertEqual(country("kinowo://\(slug)/"), code, slug)
        }
        // Not in the table: the one country that has it, else the first in order.
        XCTAssertNil(owners["harrogate"])
        XCTAssertEqual(country("https://showtimes.cc/harrogate/"), "uk")
        XCTAssertNil(owners["leeds"])
        XCTAssertEqual(country("https://showtimes.cc/leeds/"), "uk")
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
        // arrive via the live catalog), so pass catalog rows the way
        // `handleDeepLink` passes the live catalog's.
        let dl = DeepLink.parse(URL(string: "https://showtimes.cc/de/berlin/")!, cities: catalog("de", "berlin"))
        XCTAssertEqual(dl?.citySlug, "berlin")
        XCTAssertEqual(dl?.countryCode, "de")
    }

    func testUSCountrySegmentOpensInApp() {
        // Like Germany's, the US regions arrive via the live catalog rather than
        // the compile-time `City.all`, so pass catalog rows.
        let dl = DeepLink.parse(URL(string: "https://showtimes.cc/us/california/movie/wicked")!,
                                cities: catalog("us", "california"))
        XCTAssertEqual(dl?.citySlug, "california")
        XCTAssertEqual(dl?.filmSlug, "wicked")
    }

    func testESCountrySegmentOpensInApp() {
        // Spain's cities arrive via the live catalog too, so pass catalog rows
        // the way the German and US cases do.
        let dl = DeepLink.parse(URL(string: "https://showtimes.cc/es/madrid/movie/la-odisea")!,
                                cities: catalog("es", "madrid"))
        XCTAssertEqual(dl?.citySlug, "madrid")
        XCTAssertEqual(dl?.filmSlug, "la-odisea")
    }

    /// The subdomains the Showtimes countries used to answer on are gone — they
    /// stop serving outright rather than redirecting, so a link to one must not
    /// be claimed by the app either (the browser is what shows the failure).
    func testRetiredCountrySubdomainIsRejected() {
        XCTAssertNil(parse("https://uk.showtimes.cc/london/"))
        XCTAssertNil(DeepLink.parse(URL(string: "https://de.showtimes.cc/berlin/")!,
                                    cities: catalog("de", "berlin")))
    }

    /// The country segment is dropped only when it ISN'T also a city the build
    /// knows, so a country whose roster ever gained such a slug still resolves —
    /// and Poland's own one-segment links can't lose their city to a
    /// coincidence.
    func testCountrySegmentStrippingLeavesAOneSegmentLinkAlone() {
        let dl = parse("https://kinowo.net/poznan/movie/oppenheimer")
        XCTAssertEqual(dl?.citySlug, "poznan")
        XCTAssertEqual(dl?.filmSlug, "oppenheimer")

        let asCity = DeepLink.parse(URL(string: "https://showtimes.cc/us/")!, cities: catalog("pl", "us"))
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

    func testLangIsAcceptedFromTheLinkedCountrysOwnVersionTokens() {
        // A German share link carries the German pair; the whitelist used to be
        // Poland's, so `lang=OmU` was dropped as garbage and the link opened
        // unfiltered.
        let german: (String) -> Set<String> = { country in country == "de" ? ["OmU", "DF"] : ["NAP", "DUB"] }
        let cities = catalog("de", "berlin") + catalog("pl", "poznan")
        let url = URL(string: "https://showtimes.cc/de/berlin/?lang=OmU")!
        XCTAssertEqual(DeepLink.parse(url, cities: cities, languageTokensOfCountry: german)?.filters.language, "OmU")
        // …and a Polish token on a German city is not the German filter's.
        let mixed = URL(string: "https://showtimes.cc/de/berlin/?lang=NAP")!
        XCTAssertNil(DeepLink.parse(mixed, cities: cities, languageTokensOfCountry: german)?.filters.language)
        // The default whitelist is still Poland's pair.
        XCTAssertNil(parse("https://kinowo.net/poznan/?lang=OmU")?.filters.language)
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
