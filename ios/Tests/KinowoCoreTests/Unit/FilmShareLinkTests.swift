import XCTest
@testable import KinowoCore

/// `FilmShareLink.url` must mirror the server's `controllers.FilmHref`, so a
/// link shared from the app is byte-identical to one copied off the website.
/// That is now the slug path `/<city>/film/<slug>`; the legacy `?title=` builder
/// stays for films the server sent no slug for, and must keep its exact
/// encoding (spaces as `%20`, never the form `+`).
final class FilmShareLinkTests: XCTestCase {

    private func film(title: String, slug: String?) -> Film {
        Film(title: title, slug: slug, posterURL: nil, fallbackPosterURLs: [],
             runtimeMinutes: nil, releaseYear: nil, genres: [], ratings: .empty,
             countries: [], directors: [], cast: [], showings: [])
    }

    // MARK: the canonical slug link

    func testPrefersTheServerSuppliedSlug() {
        XCTAssertEqual(
            FilmShareLink.url(for: film(title: "Diuna: Część druga", slug: "diuna-czesc-druga"),
                              citySlug: "wroclaw").absoluteString,
            "https://kinowo.net/wroclaw/film/diuna-czesc-druga"
        )
    }

    func testSlugLinkCarriesNoQueryStringAtAll() {
        let url = FilmShareLink.url(for: film(title: "Lilo & Stitch", slug: "lilo-stitch"),
                                    citySlug: "warszawa").absoluteString
        XCTAssertFalse(url.contains("?"))
        XCTAssertFalse(url.contains("%"))
    }

    func testFallsBackToTheQueryFormWhenTheServerSentNoSlug() {
        // An older server, or the legacy HTML parser path, leaves `slug` nil.
        // The query form still resolves server-side (301 → the slug address).
        XCTAssertEqual(
            FilmShareLink.url(for: film(title: "Oppenheimer", slug: nil), citySlug: "poznan").absoluteString,
            "https://kinowo.net/poznan/film?title=Oppenheimer"
        )
        XCTAssertEqual(
            FilmShareLink.url(for: film(title: "Oppenheimer", slug: ""), citySlug: "poznan").absoluteString,
            "https://kinowo.net/poznan/film?title=Oppenheimer"
        )
    }

    // MARK: the legacy query form

    func testPlainAsciiTitleIsLeftIntact() {
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Oppenheimer", citySlug: "poznan").absoluteString,
            "https://kinowo.net/poznan/film?title=Oppenheimer"
        )
    }

    func testCarriesTheCitySlugInThePath() {
        // The city the sharer is browsing scopes the link — a city-less
        // `/film?title=…` has no server route and 404s.
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Oppenheimer", citySlug: "bielsko-biala").absoluteString,
            "https://kinowo.net/bielsko-biala/film?title=Oppenheimer"
        )
    }

    func testSpacesAndAmpersandEncode() {
        // Space → %20 (not `+`), `&` → %26.
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Lilo & Stitch", citySlug: "warszawa").absoluteString,
            "https://kinowo.net/warszawa/film?title=Lilo%20%26%20Stitch"
        )
    }

    func testColonAndPolishDiacriticsEncode() {
        XCTAssertEqual(
            FilmShareLink.url(forTitle: "Diuna: Część druga", citySlug: "wroclaw").absoluteString,
            "https://kinowo.net/wroclaw/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga"
        )
    }

    func testNeverEmitsFormPlusForSpace() {
        XCTAssertFalse(FilmShareLink.url(forTitle: "Past Lives", citySlug: "poznan").absoluteString.contains("+"))
    }
}
