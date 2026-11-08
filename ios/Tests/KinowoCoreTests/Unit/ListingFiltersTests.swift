import XCTest
@testable import KinowoCore

/// The Filtry axes `ContentView` holds (`ListingFilters`), the name universes it
/// builds its lists from (`FilterFacets`), and how a deep link lands on both.
final class ListingFiltersTests: XCTestCase {

    private func film(_ title: String, slug: String? = nil, countries: [String] = [], genres: [String] = [],
                      directors: [String] = [], cast: [String] = [], showings: [DayShowings] = []) -> Film {
        var film = Film(title: title, posterURL: nil, fallbackPosterURLs: [],
                        runtimeMinutes: 100, releaseYear: nil, genres: genres, ratings: .empty, countries: countries,
                        directors: directors, cast: cast, showings: showings)
        film.slug = slug
        return film
    }

    private func link(film title: String? = nil, slug: String? = nil, _ query: String = "") -> DeepLink {
        DeepLink(citySlug: "poznan", filmTitle: title, filmSlug: slug,
                 filters: DeepLinkFilters(queryItems: URLComponents(string: "x:/?" + query)?.queryItems ?? []),
                 countryCode: "pl")
    }

    // MARK: FilterFacets

    func testNameCountsOrderByFrequencyThenCaseInsensitiveName() {
        let films = [
            film("A", countries: ["Polska", "francja"]),
            film("B", countries: ["Francja2", "Polska"]),
            film("C", countries: ["Austria"]),
        ]
        XCTAssertEqual(FilterFacets(films: films).countries, [
            NameCount(name: "Polska", count: 2),
            NameCount(name: "Austria", count: 1),
            NameCount(name: "francja", count: 1),
            NameCount(name: "Francja2", count: 1),
        ])
    }

    func testEachFacetReadsItsOwnAxis() {
        let facets = FilterFacets(films: [film("A", countries: ["PL"], genres: ["Dramat"], directors: ["Wajda"], cast: ["Cybulski"])])
        XCTAssertEqual(facets.countries.map(\.name), ["PL"])
        XCTAssertEqual(facets.genres.map(\.name), ["Dramat"])
        XCTAssertEqual(facets.directors.map(\.name), ["Wajda"])
        XCTAssertEqual(facets.cast.map(\.name), ["Cybulski"])
    }

    // MARK: ListingFilters

    private func playing(_ title: String, at cinemas: String...) -> Film {
        film(title, showings: [DayShowings(date: "2026-11-08", label: "", cinemas: cinemas.map {
            CinemaShowings(cinema: $0, cinemaURL: nil, showtimes: [Showtime(time: "18:00", format: "", room: nil, bookingURL: nil)])
        })])
    }

    /// The Filtry button's rule over one city's listing, with nothing hidden
    /// and no cinema unticked unless a case says so.
    private func active(_ filters: ListingFilters, disabledCinemas: Set<String> = [], hidden: Set<String> = []) -> Bool {
        let films = [playing("A", at: "Muza")]
        return filters.isActive(over: films, cityCinemas: films.allCinemas(),
                                disabledCinemas: disabledCinemas, hidden: hidden)
    }

    func testNothingSetIsNotActive() {
        XCTAssertFalse(active(.empty))
    }

    /// Sort orders rather than narrows: it never lights the Filtry button.
    func testSortAloneIsNotActive() {
        var filters = ListingFilters.empty
        filters.sort = .rating
        XCTAssertFalse(active(filters))
    }

    func testEveryFormatAxisMakesTheFiltersActive() {
        for format in [FormatFilter(dimension: "3D"), FormatFilter(language: "NAP"), FormatFilter(fromHour: 18)] {
            var filters = ListingFilters.empty
            filters.format = format
            XCTAssertTrue(active(filters), "\(format)")
        }
    }

    func testEveryExcludedNameSetMakesTheFiltersActive() {
        let axes: [WritableKeyPath<ListingFilters, Set<String>>] =
            [\.excludedCountries, \.excludedGenres, \.excludedDirectors, \.excludedCast]
        for axis in axes {
            var filters = ListingFilters.empty
            filters[keyPath: axis] = ["X"]
            XCTAssertTrue(active(filters), "\(axis)")
        }
    }

    /// A cinema unticked in this city narrows the listing, so it lights the
    /// button — as on Android (`FilterFacets.filtersActive`) and the web.
    func testACinemaUntickedInThisCityMakesTheFiltersActive() {
        XCTAssertTrue(active(.empty, disabledCinemas: ["Muza"]))
    }

    /// The excluded-cinema set is global across cities: one unticked elsewhere
    /// narrows nothing here and must not light the button.
    func testACinemaUntickedInAnotherCityIsNotActive() {
        XCTAssertFalse(active(.empty, disabledCinemas: ["Elsewhere"]))
    }

    /// A hidden film narrows the listing too, so it lights the button — as on
    /// Android.
    func testAHiddenFilmMakesTheFiltersActive() {
        XCTAssertTrue(active(.empty, hidden: ["Hidden"]))
    }

    /// An IMAX pick no loaded film can satisfy is dropped, so it lights nothing.
    func testAnUnsatisfiableImaxPickIsNotActive() {
        var filters = ListingFilters.empty
        filters.format.imax = true
        XCTAssertFalse(active(filters))
    }

    func testALinksScalarsSetSortAndFormatAndLeaveAbsentAxesAlone() {
        var filters = ListingFilters.empty
        filters.format.language = "NAP"
        filters.excludedCast = ["X"]
        filters.applyScalars(of: link("dim=3D&sort=rating").filters)
        XCTAssertEqual(filters.sort, .rating)
        XCTAssertEqual(filters.format.dimension, "3D")
        XCTAssertEqual(filters.format.language, "NAP", "an axis the link does not name stays")
        XCTAssertEqual(filters.excludedCast, ["X"])
    }

    func testALinksInclusionsInvertAgainstTheLoadedFacets() {
        let facets = FilterFacets(films: [film("A", countries: ["PL", "FR"], genres: ["Dramat", "Horror"])])
        var filters = ListingFilters.empty
        filters.excludedDirectors = ["Kept"]
        filters.applyInclusions(of: link("country=PL&genre=Horror").filters, facets: facets)
        XCTAssertEqual(filters.excludedCountries, ["FR"])
        XCTAssertEqual(filters.excludedGenres, ["Dramat"])
        XCTAssertEqual(filters.excludedDirectors, ["Kept"], "an axis the link does not name stays")
    }

    // MARK: Deep-link film

    func testASlugLinkResolvesExactlyBeforeAnyTitleMatch() {
        let films = [film("Diabeł ubiera się u Prady II", slug: "prada-2"), film("Other", slug: "other")]
        XCTAssertEqual(films.deepLinkTarget(link(slug: "other"))?.title, "Other")
    }

    func testALegacyTitleLinkMatchesUnderTheNumeralFold() {
        let films = [film("Diabeł ubiera się u Prady II")]
        XCTAssertEqual(films.deepLinkTarget(link(film: "Diabeł ubiera się u Prady 2"))?.title, "Diabeł ubiera się u Prady II")
        XCTAssertNil(films.deepLinkTarget(link(film: "Gone")))
    }
}
