import XCTest
@testable import KinowoCore

/// Regression tests for the "multikino-only poster" shape.
///
/// Server context (see `app/tools/PosterProxy.scala`): the
/// `images.weserv.nl` proxy 403s on `www.multikino.pl` origins, so the
/// server emits the raw multikino URL as the primary `<img src>` and
/// omits the `data-fallbacks` chain when no other source (TMDB, IMDb,
/// another cinema) carries an alternative poster. The web's
/// `<img onerror>` walks the empty fallback chain straight to
/// `.no-poster`; iOS does the same via `PosterImage`.
///
/// User-visible symptom we're guarding against: a multikino-only film
/// renders fine in the browser (Multikino's CDN accepts a referer-
/// carrying browser request) but shows "Brak plakatu" in the iOS app
/// once `AsyncImage`'s URLSession request is rejected by the CDN.
///
/// What this test covers: the *parser* side of that chain — given the
/// production HTML shape (raw multikino URL, no data-fallbacks
/// attribute), `HTMLParser` still returns a non-nil `posterURL`.
/// That nails down "the iOS app does receive a URL to try"; if the
/// actual fetch fails on device, the next step is to widen the
/// server-side fallback (TMDB-search-derived URL) so the chain isn't
/// empty by default — out of scope for this spec.
///
/// "Żywot Briana Grupy Monty Pythona" is the canary the user
/// reported. The bundled `home.html` fixture happens to include this
/// exact film in the same multikino-direct shape, so we can assert
/// against it directly without inventing synthetic markup.
final class MultikinoDirectPosterTests: XCTestCase {

    private let canaryTitle = "Żywot Briana Grupy Monty Pythona"

    private func parseHome() throws -> [Film] {
        let html = try Fixtures.load("home")
        return HTMLParser.parse(html: html)
    }

    func testCanaryFilmAppearsInTheHomeFixture() throws {
        let films = try parseHome()
        XCTAssertTrue(
            films.contains { $0.title == canaryTitle },
            "Bundled home fixture must include the canary film '\(canaryTitle)' so this regression spec exercises real HTML. If the fixture is refreshed and the film drops out, replace `canaryTitle` with another multikino-only film."
        )
    }

    func testCanaryFilmExposesAMultikinoPosterURL() throws {
        let films = try parseHome()
        guard let film = films.first(where: { $0.title == canaryTitle }) else {
            XCTFail("canary film '\(canaryTitle)' missing from home fixture")
            return
        }
        // The whole point of this test: even though the server skipped
        // `images.weserv.nl` for this URL, the parser still surfaces a
        // URL the view layer can hand to `AsyncImage`.
        XCTAssertNotNil(film.posterURL,
                        "canary film has no posterURL — parser regression for raw multikino URLs?")
        let s = film.posterURL?.absoluteString ?? ""
        XCTAssertTrue(s.hasPrefix("https://www.multikino.pl/"),
                      "expected raw multikino origin URL; got '\(s)'")
        XCTAssertFalse(s.contains("&amp;"),
                       "URL still HTML-escaped (htmlDecoded() should have run): '\(s)'")
    }

    func testCanaryFilmHasNoFallbackChain() throws {
        // Records the production reality: multikino-only films ship
        // with an empty `data-fallbacks` attribute, so the iOS app has
        // nothing to walk to when AsyncImage's primary fetch fails.
        // This is the missing safety net behind the user-visible
        // symptom; if/when the server widens the fallback chain (e.g.
        // via a TMDB-search URL), this assertion will start failing
        // and we'll know to delete this test.
        let films = try parseHome()
        guard let film = films.first(where: { $0.title == canaryTitle }) else {
            XCTFail("canary film '\(canaryTitle)' missing from home fixture")
            return
        }
        XCTAssertTrue(film.fallbackPosterURLs.isEmpty,
                      "canary film has \(film.fallbackPosterURLs.count) fallback poster(s); this test was written when it had zero — the symptom should now be fixed and this spec can be deleted or reframed")
    }

    /// Pure-shape probe — same regex path, hand-rolled minimal HTML.
    /// Catches a parser change that handles the canary film by
    /// accident but breaks the general "raw multikino URL, no
    /// fallbacks" pattern.
    func testSyntheticMultikinoOnlyChunkParses() {
        let html = """
        <div class="col" data-title="Test Film">
          <div class="card">
            <div class="poster-wrap">
              <a href="/film?title=Test+Film">
                <img src="https://www.multikino.pl/path/poster.jpg?rev=abc123" alt="Test Film" loading="lazy">
                <div class="no-poster" style="display:none">Brak plakatu</div>
              </a>
            </div>
          </div>
        </div>
        """
        let films = HTMLParser.parse(html: html)
        XCTAssertEqual(films.count, 1)
        XCTAssertEqual(films[0].posterURL?.absoluteString,
                       "https://www.multikino.pl/path/poster.jpg?rev=abc123")
        XCTAssertEqual(films[0].fallbackPosterURLs, [])
    }
}
