import XCTest
@testable import KinowoCore

/// `FilmShareLink.url` must mirror the server's `controllers.FilmHref`, so a
/// link shared from the app is byte-identical to one copied off the website.
/// That is now the slug path `/<city>/movie/<slug>`; the legacy `?title=` builder
/// stays for films the server sent no slug for, and must keep its exact
/// encoding (spaces as `%20`, never the form `+`).
private let PolishOrigin = "https://kinowo.net"

final class FilmShareLinkTests: XCTestCase {

    /// The origin is the COUNTRY's. It used to be the Polish host, hardcoded, so
    /// every share from the UK, Germany, the US or Spain produced a dead link —
    /// a Barcelona film as `kinowo.net/barcelona/movie/…`, which 404s, because
    /// that city lives on `showtimes.cc/es`.
    func testUsesTheBrowsingCountrysOrigin() {
        XCTAssertEqual(
            FilmShareLink.url(origin: "https://showtimes.cc/es",
                              for: film(title: "La Patrulla Canina", slug: "la-patrulla-canina"),
                              citySlug: "barcelona").absoluteString,
            "https://showtimes.cc/es/barcelona/movie/la-patrulla-canina"
        )
        XCTAssertEqual(
            FilmShareLink.url(origin: "https://showtimes.cc/uk",
                              for: film(title: "Dune: Part Two", slug: "dune-part-two"),
                              citySlug: "london").absoluteString,
            "https://showtimes.cc/uk/london/movie/dune-part-two"
        )
    }

    func testQueryFallbackIsCountryScopedToo() {
        XCTAssertEqual(
            FilmShareLink.url(origin: "https://showtimes.cc/de",
                              for: film(title: "Oppenheimer", slug: nil),
                              citySlug: "berlin").absoluteString,
            "https://showtimes.cc/de/berlin/movie?title=Oppenheimer"
        )
    }

    /// No caller may reintroduce the constant.
    func testNeverEmitsThePolishHostForAnotherCountry() {
        let url = FilmShareLink.url(origin: "https://showtimes.cc/us",
                                    for: film(title: "Wicked", slug: "wicked"),
                                    citySlug: "san-francisco").absoluteString
        XCTAssertFalse(url.contains("kinowo.net"))
    }

    private func film(title: String, slug: String?) -> Film {
        Film(title: title, slug: slug, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: nil, releaseYear: nil, genres: [], ratings: .empty,
             countries: [], directors: [], cast: [], showings: [])
    }

    // MARK: the canonical slug link

    func testPrefersTheServerSuppliedSlug() {
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, for: film(title: "Diuna: Część druga", slug: "diuna-czesc-druga"),
                              citySlug: "wroclaw").absoluteString,
            "https://kinowo.net/wroclaw/movie/diuna-czesc-druga"
        )
    }

    func testSlugLinkCarriesNoQueryStringAtAll() {
        let url = FilmShareLink.url(origin: PolishOrigin, for: film(title: "Lilo & Stitch", slug: "lilo-stitch"),
                                    citySlug: "warszawa").absoluteString
        XCTAssertFalse(url.contains("?"))
        XCTAssertFalse(url.contains("%"))
    }

    func testFallsBackToTheQueryFormWhenTheServerSentNoSlug() {
        // An older server, or the legacy HTML parser path, leaves `slug` nil.
        // The query form still resolves server-side (301 → the slug address).
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, for: film(title: "Oppenheimer", slug: nil), citySlug: "poznan").absoluteString,
            "https://kinowo.net/poznan/movie?title=Oppenheimer"
        )
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, for: film(title: "Oppenheimer", slug: ""), citySlug: "poznan").absoluteString,
            "https://kinowo.net/poznan/movie?title=Oppenheimer"
        )
    }

    // MARK: the legacy query form

    func testPlainAsciiTitleIsLeftIntact() {
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, forTitle: "Oppenheimer", citySlug: "poznan").absoluteString,
            "https://kinowo.net/poznan/movie?title=Oppenheimer"
        )
    }

    func testCarriesTheCitySlugInThePath() {
        // The city the sharer is browsing scopes the link — a city-less
        // `/movie?title=…` has no server route and 404s.
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, forTitle: "Oppenheimer", citySlug: "bielsko-biala").absoluteString,
            "https://kinowo.net/bielsko-biala/movie?title=Oppenheimer"
        )
    }

    func testSpacesAndAmpersandEncode() {
        // Space → %20 (not `+`), `&` → %26.
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, forTitle: "Lilo & Stitch", citySlug: "warszawa").absoluteString,
            "https://kinowo.net/warszawa/movie?title=Lilo%20%26%20Stitch"
        )
    }

    func testColonAndPolishDiacriticsEncode() {
        XCTAssertEqual(
            FilmShareLink.url(origin: PolishOrigin, forTitle: "Diuna: Część druga", citySlug: "wroclaw").absoluteString,
            "https://kinowo.net/wroclaw/movie?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga"
        )
    }

    func testNeverEmitsFormPlusForSpace() {
        XCTAssertFalse(FilmShareLink.url(origin: PolishOrigin, forTitle: "Past Lives", citySlug: "poznan").absoluteString.contains("+"))
    }
}
