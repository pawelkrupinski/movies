import XCTest
@testable import KinowoCore

final class HTMLParserTests: XCTestCase {

    func testParsesManyFilmsFromHomeFixture() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        XCTAssertGreaterThanOrEqual(films.count, 20, "expected the home fixture to yield a healthy slate of films")
    }

    func testEveryFilmHasNonEmptyTitle() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        XCTAssertFalse(films.isEmpty)
        for film in films {
            XCTAssertFalse(film.title.isEmpty, "film with empty title")
            XCTAssertFalse(film.title.contains("&amp;"), "title should be html-decoded: \(film.title)")
        }
    }

    func testMostFilmsHaveAPoster() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let withPoster = films.filter { $0.posterURL != nil }.count
        let ratio = Double(withPoster) / Double(films.count)
        XCTAssertGreaterThanOrEqual(ratio, 0.8, "only \(withPoster)/\(films.count) films had a poster")
    }

    func testTotalShowtimesAcrossFilmsIsSubstantial() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let total = films.reduce(0) { acc, film in
            acc + film.showings.reduce(0) { $0 + $1.cinemas.reduce(0) { $0 + $1.showtimes.count } }
        }
        XCTAssertGreaterThanOrEqual(total, 100, "expected ≥100 showtimes total; got \(total)")
    }

    func testPosterURLsDoNotCarryEscapedAmpersands() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let posters = films.compactMap { $0.posterURL?.absoluteString }
        XCTAssertFalse(posters.isEmpty)
        for url in posters {
            XCTAssertFalse(url.contains("&amp;"), "poster URL still HTML-escaped: \(url)")
        }
    }

    func testFallbackPosterChainsExistForAtLeastOneFilm() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let withFallbacks = films.filter { !$0.fallbackPosterURLs.isEmpty }.count
        XCTAssertGreaterThanOrEqual(withFallbacks, 1, "expected at least one film with poster fallbacks")
    }

    func testRuntimeParsedAsMinutes() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let runtimes = films.compactMap { $0.runtimeMinutes }
        XCTAssertFalse(runtimes.isEmpty, "expected at least some films to advertise a runtime")
        // Loose sanity bounds — most feature films fit in 60-240 min, but
        // the fixture also includes festival blocks and marathons that
        // legitimately exceed 6 h (`720min` etc.). We only care that the
        // parser isn't returning garbage like zero or 99999.
        for r in runtimes {
            XCTAssertGreaterThanOrEqual(r, 10)
            XCTAssertLessThanOrEqual(r, 1000)
        }
    }

    func testDirectorsAndCastParsedFromHomeFixture() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let withDirectors = films.filter { !$0.directors.isEmpty }.count
        let withCast = films.filter { !$0.cast.isEmpty }.count
        XCTAssertGreaterThanOrEqual(withDirectors, 1, "expected at least one film with directors; got \(withDirectors)")
        XCTAssertGreaterThanOrEqual(withCast, 1, "expected at least one film with cast; got \(withCast)")
    }

    func testDirectorAndCastParsedFromMinimalCard() throws {
        let chunk = """
        <div class="col" data-title="Test" data-countries="" data-director="Christopher Nolan" data-cast="DiCaprio, Tom Hardy, Elliot Page">
        </div>
        """
        let films = HTMLParser.parse(html: chunk)
        XCTAssertEqual(films.first?.directors, ["Christopher Nolan"])
        XCTAssertEqual(films.first?.cast, ["DiCaprio", "Tom Hardy", "Elliot Page"])
    }

    func testMissingDirectorAndCastDefaultToEmpty() throws {
        let chunk = """
        <div class="col" data-title="Test" data-countries="">
        </div>
        """
        let films = HTMLParser.parse(html: chunk)
        XCTAssertEqual(films.first?.directors, [])
        XCTAssertEqual(films.first?.cast, [])
    }

    func testRuntimeParseExamples() throws {
        // The home fixture contains "2h 7min", "2h 12min", "1h 49min", "1h 34min".
        // Reconstruct the smallest possible card to exercise the runtime branch
        // directly without depending on a particular film's row in the fixture.
        let chunk = """
        <div class="col" data-title="Test">
          <span class="pill runtime">2h 37min</span>
        </div>
        """
        let films = HTMLParser.parse(html: chunk)
        XCTAssertEqual(films.first?.runtimeMinutes, 157)
    }
}
