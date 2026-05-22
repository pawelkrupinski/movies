import XCTest
@testable import KinowoCore

final class RatingsParserTests: XCTestCase {

    func testParsesImdbDouble() {
        let chunk = """
        <div class="ratings">
          <a href="https://imdb.com/title/tt123" class="rating-imdb">
            <span class="rating-imdb-value">7.8</span>
          </a>
        </div>
        """
        let r = RatingsParser.parseRatings(in: chunk)
        XCTAssertEqual(r.imdb, 7.8)
        XCTAssertEqual(r.imdbURL?.absoluteString, "https://imdb.com/title/tt123")
    }

    func testParsesMetascoreInt() {
        // Markup mirrors current `_ratingBadges.scala.html` output: no inline
        // `onclick` (commit 6372afc dropped ~600 redundant `event.stopPropagation()`
        // attributes from card chrome and rating pills). The earlier version of
        // this test embedded the legacy `onclick`, and the regex was tightened to
        // require it — so MC scores silently stopped parsing on every page once
        // the template changed. Keep the fixture in lockstep with prod.
        let chunk = """
        <div class="ratings">
          <a href="https://www.metacritic.com/movie/foo" target="_blank" rel="noopener" class="rating-meta" title="Metacritic">62</a>
        </div>
        """
        let r = RatingsParser.parseRatings(in: chunk)
        XCTAssertEqual(r.metascore, 62)
        XCTAssertEqual(r.metacriticURL?.absoluteString, "https://www.metacritic.com/movie/foo")
    }

    func testParsesRottenTomatoesPercent() {
        let chunk = """
        <div class="ratings">
          <a href="https://rottentomatoes.com/m/foo" class="rating-rt fresh" title="Rotten Tomatoes">
            <span class="rating-rt-label">RT</span><span class="rating-rt-value">94%</span>
          </a>
        </div>
        """
        let r = RatingsParser.parseRatings(in: chunk)
        XCTAssertEqual(r.rottenTomatoes, 94)
        XCTAssertEqual(r.rottenTomatoesURL?.absoluteString, "https://rottentomatoes.com/m/foo")
    }

    func testParsesFilmwebDouble() {
        let chunk = """
        <div class="ratings">
          <a href="https://filmweb.pl/film/Foo" class="rating-fw" title="Filmweb">
            <span class="rating-fw-label">FW</span><span class="rating-fw-value">7.2</span>
          </a>
        </div>
        """
        let r = RatingsParser.parseRatings(in: chunk)
        XCTAssertEqual(r.filmweb, 7.2)
        XCTAssertEqual(r.filmwebURL?.absoluteString, "https://filmweb.pl/film/Foo")
    }

    func testParsesAllFourSourcesTogether() {
        let chunk = """
        <div class="ratings">
          <a href="https://imdb.com/title/tt1" class="rating-imdb"><span class="rating-imdb-label">IMDb</span><span class="rating-imdb-value">8.0</span></a>
          <a href="https://metacritic.com/movie/foo" class="rating-meta" title="Metacritic">55</a>
          <a href="https://rottentomatoes.com/m/foo" class="rating-rt fresh"><span class="rating-rt-label">RT</span><span class="rating-rt-value">72%</span></a>
          <a href="https://filmweb.pl/film/Foo" class="rating-fw"><span class="rating-fw-label">FW</span><span class="rating-fw-value">6.9</span></a>
        </div>
        """
        let r = RatingsParser.parseRatings(in: chunk)
        XCTAssertEqual(r.imdb, 8.0)
        XCTAssertEqual(r.metascore, 55)
        XCTAssertEqual(r.rottenTomatoes, 72)
        XCTAssertEqual(r.filmweb, 6.9)
        XCTAssertFalse(r.isEmpty)
    }

    /// Replay the captured production home page and assert metascores parse
    /// across many films — guards against future drift in `_ratingBadges`
    /// markup the same way the per-pill tests guard the regex itself.
    /// The fixture currently carries ~95 `.rating-meta` anchors; a stricter
    /// floor (≥10) leaves room for normal day-to-day churn in MC coverage.
    func testParsesMetascoresFromHomeFixture() throws {
        let html = try Fixtures.load("home")
        let films = HTMLParser.parse(html: html)
        let withMetascore = films.filter { $0.ratings.metascore != nil }
        XCTAssertGreaterThanOrEqual(
            withMetascore.count, 10,
            "expected the home fixture to surface metascores on many films; got \(withMetascore.count)"
        )
        for film in withMetascore {
            if let mc = film.ratings.metascore {
                XCTAssertTrue((0...100).contains(mc), "metascore out of range for \(film.title): \(mc)")
            }
        }
    }

    func testNoRatingsBlockYieldsEmpty() {
        let r = RatingsParser.parseRatings(in: "<p>no ratings here</p>")
        XCTAssertTrue(r.isEmpty)
        XCTAssertEqual(r, Film.Ratings.empty)
    }
}
