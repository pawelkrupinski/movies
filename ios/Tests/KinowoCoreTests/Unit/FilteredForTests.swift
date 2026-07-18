import XCTest
@testable import KinowoCore

final class FilteredForTests: XCTestCase {

    private func slot(_ time: String, _ format: String = "2D NAP") -> Showtime {
        Showtime(time: time, format: format, room: nil, bookingURL: nil)
    }

    private func cinema(_ name: String, _ times: [Showtime]) -> CinemaShowings {
        CinemaShowings(cinema: name, cinemaURL: nil, showtimes: times)
    }

    private func day(_ date: String, _ cinemas: [CinemaShowings]) -> DayShowings {
        DayShowings(date: date, label: date, cinemas: cinemas)
    }

    private func film(
        _ title: String, _ days: [DayShowings],
        countries: [String] = [], genres: [String] = [], directors: [String] = [], cast: [String] = []
    ) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: 100, releaseYear: nil, genres: genres, ratings: .empty, countries: countries,
             directors: directors, cast: cast, showings: days)
    }

    private let today: String = DateFilter.iso(Date())
    private let tomorrow: String = DateFilter.iso(Date().addingTimeInterval(86_400))

    private func fixture() -> [Film] {
        let mandalorian = film("Mandalorian and Grogu", [
            day(today, [
                cinema("Helonki", [slot("17:00"), slot("20:00", "3D NAP")]),
                cinema("Apollo",  [slot("19:30")]),
            ]),
            day(tomorrow, [
                cinema("Muza", [slot("16:00")]),
            ]),
        ])
        let title2 = film("Title2", [
            day(today, [
                cinema("Apollo", [slot("18:00")]),
            ]),
        ])
        let title3 = film("Title3", [
            day(tomorrow, [
                cinema("Helonki", [slot("21:00")]),
            ]),
        ])
        return [mandalorian, title2, title3]
    }

    func testDateTodayFiltersToTodayOnly() {
        let result = fixture().filteredFor(
            date: .today, format: .empty, query: "",
            hidden: []
        )
        XCTAssertEqual(result.map(\.title).sorted(), ["Mandalorian and Grogu", "Title2"])
        for f in result {
            XCTAssertEqual(f.showings.map(\.date), [today])
        }
    }

    func testQueryMatchesCaseInsensitiveSubstring() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "Mand",
            hidden: []
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])

        let lowered = fixture().filteredFor(
            date: .anytime, format: .empty, query: "mand",
            hidden: []
        )
        XCTAssertEqual(lowered.map(\.title), ["Mandalorian and Grogu"])
    }

    func testHiddenDropsFilmEntirely() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: ["Title2"]
        )
        XCTAssertFalse(result.contains { $0.title == "Title2" })
        XCTAssertEqual(result.count, 2)
    }

    // MARK: - disabledCinemas axis (the cinema exclusion set)

    func testEmptyExclusionSetRetainsEveryCinemasShowings() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: []
        )
        // No cinema constraint: all three films survive with every cinema-group intact.
        XCTAssertEqual(result.map(\.title).sorted(), ["Mandalorian and Grogu", "Title2", "Title3"])
        let mando = result.first { $0.title == "Mandalorian and Grogu" }!
        let todayDay = mando.showings.first { $0.date == today }!
        XCTAssertEqual(todayDay.cinemas.map(\.cinema).sorted(), ["Apollo", "Helonki"])
    }

    func testExcludingEveryOtherCinemaDropsFilmsLeftWithNone() {
        // Untick everything but Apollo. Title2 played only at Apollo → survives;
        // Title3 played only at Helonki → drops entirely.
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: ["Helonki", "Muza"]
        )
        XCTAssertEqual(result.map(\.title).sorted(), ["Mandalorian and Grogu", "Title2"])
        for film in result {
            for day in film.showings {
                XCTAssertEqual(day.cinemas.map(\.cinema), ["Apollo"])
            }
        }
    }

    func testAnExcludedNameFromAnotherCityChangesNothing() {
        // The excluded set is GLOBAL across cities, so it routinely carries
        // names this city has never heard of. They must simply never match —
        // that's what makes a city switch safe without resetting the set.
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: ["Cinema In Another City"]
        )
        XCTAssertEqual(result.map(\.title).sorted(), ["Mandalorian and Grogu", "Title2", "Title3"])
        let mando = result.first { $0.title == "Mandalorian and Grogu" }!
        let todayDay = mando.showings.first { $0.date == today }!
        XCTAssertEqual(todayDay.cinemas.map(\.cinema).sorted(), ["Apollo", "Helonki"])
    }

    func testDisabledCinemasDropsThoseCinemasKeepsOthers() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: ["Helonki"]
        )
        // Helonki is excluded everywhere; Title3 (only Helonki) disappears,
        // the others keep their remaining cinema-groups.
        XCTAssertEqual(result.map(\.title).sorted(), ["Mandalorian and Grogu", "Title2"])
        for f in result {
            for d in f.showings {
                XCTAssertFalse(d.cinemas.map(\.cinema).contains("Helonki"))
            }
        }
    }

    func testDisablingEveryCinemaOfAFilmDropsIt() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: ["Apollo", "Helonki", "Muza"]
        )
        XCTAssertTrue(result.isEmpty)
    }

    func testEmptyDisabledCinemasKeepsEveryCinema() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], disabledCinemas: []
        )
        XCTAssertEqual(result.count, 3)
    }

    func testDisabledCinemasAndFormatIntersect() {
        var f = FormatFilter(); f.dimension = "3D"
        // Only Helonki has a 3D slot — disabling Helonki plus a 3D filter
        // leaves nothing.
        let result = fixture().filteredFor(
            date: .anytime, format: f, query: "",
            hidden: [], disabledCinemas: ["Helonki"]
        )
        XCTAssertTrue(result.isEmpty)
    }

    func testFormatNarrowsShowtimesButKeepsFilmIfAnySlotRemains() {
        var f = FormatFilter()
        f.dimension = "3D"
        let result = fixture().filteredFor(
            date: .anytime, format: f, query: "",
            hidden: []
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])
        let mando = result[0]
        let todayDay = mando.showings.first { $0.date == today }!
        XCTAssertEqual(todayDay.cinemas.count, 1)
        XCTAssertEqual(todayDay.cinemas[0].cinema, "Helonki")
        XCTAssertEqual(todayDay.cinemas[0].showtimes.map(\.time), ["20:00"])
    }

    func testEmptyQueryTrimsWhitespace() {
        let result = fixture().filteredFor(
            date: .anytime, format: .empty, query: "   ",
            hidden: []
        )
        XCTAssertEqual(result.count, 3)
    }

    func testCombinedFiltersIntersect() {
        let result = fixture().filteredFor(
            date: .today, format: .empty, query: "Mand",
            hidden: ["Title2"], disabledCinemas: ["Apollo", "Muza"]
        )
        XCTAssertEqual(result.map(\.title), ["Mandalorian and Grogu"])
        let todayDay = result[0].showings[0]
        XCTAssertEqual(todayDay.cinemas.map(\.cinema), ["Helonki"])
    }

    func testTodayFilterUsesProvidedNowNotSystemClock() {
        let fixedToday = "2020-06-15"
        let fixedTomorrow = "2020-06-16"
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw")!
        let now = cal.date(from: DateComponents(year: 2020, month: 6, day: 15, hour: 12))!

        let films = [
            film("A", [day(fixedToday, [cinema("X", [slot("18:00")])])]),
            film("B", [day(fixedTomorrow, [cinema("X", [slot("19:00")])])]),
        ]

        let filtered = films.filteredFor(
            date: .today, format: .empty, query: "",
            hidden: [],
            now: now
        )
        XCTAssertEqual(filtered.map(\.title), ["A"])
        XCTAssertEqual(filtered[0].showings.map(\.date), [fixedToday])
    }

    // MARK: - Country filter (excluded semantics)

    func testExcludedCountryHidesFilmsOnlyFromThatCountry() {
        let films = [
            film("Polish Film", [day(today, [cinema("A", [slot("18:00")])])], countries: ["Polska"]),
            film("US Film",     [day(today, [cinema("A", [slot("19:00")])])], countries: ["USA"]),
            film("Co-prod",     [day(today, [cinema("A", [slot("20:00")])])], countries: ["Polska", "Francja"]),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCountries: ["Polska"]
        )
        XCTAssertEqual(filtered.map(\.title).sorted(), ["Co-prod", "US Film"])
    }

    func testExcludedCountryHidesCoProductionOnlyWhenAllExcluded() {
        let films = [
            film("Co-prod", [day(today, [cinema("A", [slot("18:00")])])], countries: ["Polska", "Francja"]),
        ]
        let still = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCountries: ["Polska"]
        )
        XCTAssertEqual(still.count, 1, "co-prod stays when only one country is excluded")

        let gone = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCountries: ["Polska", "Francja"]
        )
        XCTAssertEqual(gone.count, 0, "co-prod drops when all its countries are excluded")
    }

    func testEmptyExcludedCountriesShowsAll() {
        let films = [
            film("A", [day(today, [cinema("A", [slot("18:00")])])], countries: ["Polska"]),
            film("B", [day(today, [cinema("A", [slot("19:00")])])], countries: ["USA"]),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCountries: []
        )
        XCTAssertEqual(filtered.count, 2)
    }

    // MARK: - Genre filter (excluded semantics)

    func testExcludedGenreHidesFilmsOnlyFromThatGenre() {
        let films = [
            film("Comedy", [day(today, [cinema("A", [slot("18:00")])])], genres: ["Komedia"]),
            film("Drama",  [day(today, [cinema("A", [slot("19:00")])])], genres: ["Dramat"]),
            film("Mix",    [day(today, [cinema("A", [slot("20:00")])])], genres: ["Komedia", "Dramat"]),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedGenres: ["Komedia"]
        )
        XCTAssertEqual(filtered.map(\.title).sorted(), ["Drama", "Mix"])
    }

    func testExcludedGenreDropsMultiGenreFilmOnlyWhenAllExcluded() {
        let films = [
            film("Mix", [day(today, [cinema("A", [slot("18:00")])])], genres: ["Komedia", "Dramat"]),
        ]
        XCTAssertEqual(
            films.filteredFor(date: .anytime, format: .empty, query: "",
                              hidden: [], excludedGenres: ["Komedia"]).count,
            1, "stays when only one of its genres is excluded")
        XCTAssertEqual(
            films.filteredFor(date: .anytime, format: .empty, query: "",
                              hidden: [], excludedGenres: ["Komedia", "Dramat"]).count,
            0, "drops when all its genres are excluded")
    }

    func testFilmWithNoGenresSurvivesGenreExclusion() {
        // A film with an empty genre list must not be swept out just because
        // a genre exclusion is active (empty set is a subset of everything).
        let films = [film("No genres", [day(today, [cinema("A", [slot("18:00")])])], genres: [])]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedGenres: ["Komedia"]
        )
        XCTAssertEqual(filtered.count, 1)
    }

    // MARK: - Director filter

    func testExcludedDirectorHidesFilm() {
        let films = [
            film("A", [day(today, [cinema("X", [slot("18:00")])])], directors: ["Spielberg"]),
            film("B", [day(today, [cinema("X", [slot("19:00")])])], directors: ["Nolan"]),
            film("C", [day(today, [cinema("X", [slot("20:00")])])], directors: ["Spielberg", "Nolan"]),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedDirectors: ["Spielberg"]
        )
        XCTAssertEqual(filtered.map(\.title).sorted(), ["B", "C"])
    }

    func testExcludedDirectorKeepsFilmWithNoDirector() {
        let films = [
            film("Known",   [day(today, [cinema("X", [slot("18:00")])])], directors: ["Spielberg"]),
            film("Unknown", [day(today, [cinema("X", [slot("19:00")])])], directors: []),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedDirectors: ["Spielberg"]
        )
        XCTAssertEqual(filtered.map(\.title), ["Unknown"])
    }

    // MARK: - Cast filter

    func testExcludedCastHidesFilm() {
        let films = [
            film("A", [day(today, [cinema("X", [slot("18:00")])])], cast: ["DiCaprio", "Pitt"]),
            film("B", [day(today, [cinema("X", [slot("19:00")])])], cast: ["Hanks"]),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCast: ["DiCaprio", "Pitt"]
        )
        XCTAssertEqual(filtered.map(\.title), ["B"])
    }

    func testExcludedCastKeepsFilmWhenOnlyPartialOverlap() {
        let films = [
            film("A", [day(today, [cinema("X", [slot("18:00")])])], cast: ["DiCaprio", "Pitt"]),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCast: ["DiCaprio"]
        )
        XCTAssertEqual(filtered.count, 1, "film stays when only part of its cast is excluded")
    }

    func testExcludedCastKeepsFilmWithNoCast() {
        let films = [
            film("Known",   [day(today, [cinema("X", [slot("18:00")])])], cast: ["Hanks"]),
            film("Unknown", [day(today, [cinema("X", [slot("19:00")])])], cast: []),
        ]
        let filtered = films.filteredFor(
            date: .anytime, format: .empty, query: "",
            hidden: [], excludedCast: ["Hanks"]
        )
        XCTAssertEqual(filtered.map(\.title), ["Unknown"])
    }
}
