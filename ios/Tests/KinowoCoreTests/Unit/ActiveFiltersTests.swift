import XCTest
@testable import KinowoCore

final class ActiveFiltersTests: XCTestCase {

    private func active(
        format: FormatFilter = .empty,
        countries: Set<String> = [],
        genres: Set<String> = [],
        directors: Set<String> = [],
        cast: Set<String> = []
    ) -> Bool {
        ActiveFilters.any(
            format: format,
            excludedCountries: countries,
            excludedGenres: genres,
            excludedDirectors: directors,
            excludedCast: cast
        )
    }

    func testNothingSetIsNotActive() {
        XCTAssertFalse(active())
    }

    // The crux of the change: cinema selection and hidden films are not even
    // inputs here, so they can never light the Filtry bar — only the filters
    // Wyczyść clears do. (They used to count toward `filtersActive`.)
    func testCinemaSelectionAndHiddenFilmsCannotMakeFiltersActive() {
        // No filter axis is set; whatever the user has hidden or which cinemas
        // they've toggled is irrelevant to this decision.
        XCTAssertFalse(active())
    }

    func testFormatAxisMakesFiltersActive() {
        XCTAssertTrue(active(format: FormatFilter(dimension: "3D")))
        XCTAssertTrue(active(format: FormatFilter(imax: true)))
        XCTAssertTrue(active(format: FormatFilter(fromHour: 18)))
    }

    func testAnyExcludedSetMakesFiltersActive() {
        XCTAssertTrue(active(countries: ["Polska"]))
        XCTAssertTrue(active(genres: ["Horror"]))
        XCTAssertTrue(active(directors: ["Nolan"]))
        XCTAssertTrue(active(cast: ["Bale"]))
    }
}
