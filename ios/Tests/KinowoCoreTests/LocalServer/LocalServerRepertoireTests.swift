import XCTest
@testable import KinowoCore

// The JSON contract the app actually consumes, checked against a live
// `FixtureServerMain` render: `/{city}/api/repertoire` → `[Film]`,
// `/{city}/api/details` → `[FilmDetails]`, `/{city}/api/cinemas` →
// `CinemaCatalog`. Each is decoded exactly as `RepertoireStore` /
// `DetailsStore` decode it (bare `JSONDecoder()`, same URL builder, same
// request headers), so a server-side shape change that would break the
// production stores breaks here first.
//
// `RepertoireStore` / `DetailsStore` / `CatalogStore` themselves are
// Combine-bound and excluded from the `KinowoCore` SPM target (see
// Package.swift), so this suite exercises the decoder + model line they
// delegate to rather than the store objects. `/api/catalog` is not covered
// for the same reason: its `CatalogBody` decoder lives in the app-target-only
// `CatalogStore.swift`.
//
// Android's twin is `LocalServerApiTest`; the two run against the one
// server boot in the `mobile-local-server` CI job.
final class LocalServerRepertoireTests: LocalServerTestCase {

    private let city = "poznan"

    private func repertoire() throws -> (films: [Film], fetched: Fetched) {
        let decoded = try decodeCityAPI([Film].self, city: city, endpoint: "repertoire")
        return (decoded.value, decoded.fetched)
    }

    // ── /api/repertoire ──────────────────────────────────────────

    func testRepertoireDecodesAHealthySlateOfFilms() throws {
        let (films, fetched) = try repertoire()
        XCTAssertGreaterThanOrEqual(films.count, 20,
                                    "expected a healthy slate of films; got \(films.count)")
        XCTAssertNotNil(fetched.lastModified,
                        "the JSON API should stamp Last-Modified — RepertoireCache keys its conditional request on it")
    }

    func testEveryFilmCarriesWhatTheCardRenders() throws {
        let (films, _) = try repertoire()
        XCTAssertFalse(films.isEmpty)
        for film in films {
            XCTAssertFalse(film.title.isEmpty, "film with empty title")
            XCTAssertFalse(film.showings.isEmpty, "\(film.title) listed with no showings")
            XCTAssertNotNil(film.slug, "\(film.title) has no slug — FilmShareLink falls back to the legacy ?title= form")
        }
    }

    func testAtLeastEightyPercentOfFilmsHaveAPoster() throws {
        let (films, _) = try repertoire()
        XCTAssertFalse(films.isEmpty)
        let withPoster = films.filter { $0.posterURL != nil }.count
        let ratio = Double(withPoster) / Double(films.count)
        XCTAssertGreaterThanOrEqual(ratio, 0.8,
                                    "only \(withPoster)/\(films.count) films had a poster")
    }

    func testShowingsCarryTheFullDateCinemaShowtimeTree() throws {
        let (films, _) = try repertoire()
        let re = try NSRegularExpression(pattern: #"^\d{2}:\d{2}$"#)
        var checked = 0
        for film in films {
            for day in film.showings {
                XCTAssertFalse(day.date.isEmpty, "\(film.title): day with no date")
                XCTAssertFalse(day.cinemas.isEmpty, "\(film.title) \(day.date): day with no cinemas")
                for cinema in day.cinemas {
                    XCTAssertFalse(cinema.cinema.isEmpty, "\(film.title) \(day.date): cinema with no name")
                    XCTAssertFalse(cinema.showtimes.isEmpty, "\(film.title) @ \(cinema.cinema): cinema with no showtimes")
                    for slot in cinema.showtimes {
                        let ns = slot.time as NSString
                        let m = re.firstMatch(in: slot.time, range: NSRange(location: 0, length: ns.length))
                        XCTAssertNotNil(m, "non-HH:MM time \(slot.time) on \(film.title) @ \(cinema.cinema)")
                        checked += 1
                    }
                }
            }
        }
        XCTAssertGreaterThan(checked, 100,
                             "expected a meaningful number of showtimes from the fixture; got \(checked)")
    }

    func testAtLeastOneFilmCarriesAnImdbRating() throws {
        let (films, _) = try repertoire()
        let withImdb = films.filter { $0.ratings.imdb != nil }.count
        XCTAssertGreaterThan(withImdb, 0,
                             "no film in the repertoire carried an IMDb rating — ratings JSON drift?")
    }

    // ── groupedByCinema() over the live listing ──────────────────

    func testEveryFilmInSectionPlaysOnlyAtThatCinema() throws {
        let (films, _) = try repertoire()
        let sections = films.groupedByCinema()
        XCTAssertFalse(sections.isEmpty)
        for section in sections {
            let union = Set(section.films.flatMap(\.showings).flatMap(\.cinemas).map(\.cinema))
            XCTAssertEqual(union, [section.cinema],
                           "section \(section.cinema) leaked cinemas: \(union)")
        }
    }

    // ── /api/details ─────────────────────────────────────────────

    func testDetailsDecodeAndJoinListedFilmsByTitle() throws {
        let (films, _) = try repertoire()
        let details = try decodeCityAPI([FilmDetails].self, city: city, endpoint: "details").value
        XCTAssertFalse(details.isEmpty, "expected a non-empty details payload")
        // The server only emits rows with content; nothing empty slips through.
        for row in details {
            XCTAssertTrue(!(row.synopsis ?? "").isEmpty || !row.trailerURLs.isEmpty || row.originalTitle != nil,
                          "\(row.title): details row with no synopsis, trailer, or original title")
        }
        // `FilmDetailView` joins the two endpoints by title; an orphan details
        // row would silently never reach the UI.
        let titles = Set(films.map(\.title))
        let orphans = details.map(\.title).filter { !titles.contains($0) }
        XCTAssertTrue(orphans.isEmpty, "details rows must match a listed film by title; orphans=\(orphans)")
    }

    // ── /api/cinemas ─────────────────────────────────────────────

    func testCinemaCatalogCoversEveryCinemaInTheListing() throws {
        let (films, _) = try repertoire()
        let catalog = try decodeCityAPI(CinemaCatalog.self, city: city, endpoint: "cinemas").value
        XCTAssertFalse(catalog.cinemas.isEmpty, "expected a non-empty cinema universe")
        // Poznań is a flat city: the pill bar, not the area picker.
        XCTAssertFalse(catalog.isSplit, "poznan should be a flat city; areas=\(catalog.areas.map(\.slug))")
        let listed = Set(films.flatMap(\.showings).flatMap(\.cinemas).map(\.cinema))
        let missing = listed.subtracting(catalog.cinemas)
        XCTAssertTrue(missing.isEmpty,
                      "cinemas with showings that the catalog does not list (the filter pill would never appear): \(missing)")
    }

    // ── a path the server does not serve ─────────────────────────

    /// 404, not 500. The fixture server's route table used to throw on an
    /// in-city path it did not know, and the server reported that as a 500 —
    /// which `fetch` reads as "bad server response" and points at whichever
    /// endpoint a test was asking for, rather than at the route that was
    /// never taught. That is how the `/api/cinemas` case above first failed.
    func testAPathTheServerDoesNotServeIsNotFoundRatherThanAServerError() throws {
        XCTAssertEqual(try status(of: City.apiURL(base: baseURL, slug: city, endpoint: "no-such-endpoint")), 404)
    }
}
