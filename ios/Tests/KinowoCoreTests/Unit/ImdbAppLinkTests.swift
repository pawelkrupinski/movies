import XCTest
@testable import KinowoCore

/// `Film.Ratings.imdbAppURL` turns the public IMDb web URL into the IMDb app's
/// `imdb:///title/tt…` deep link, which `RatingBadgesView` opens first (falling
/// back to the web URL when the app isn't installed).
final class ImdbAppLinkTests: XCTestCase {

    func testBuildsAppSchemeFromWebURL() {
        let web = URL(string: "https://www.imdb.com/title/tt1234567/")!
        XCTAssertEqual(
            Film.Ratings.imdbAppURL(from: web)?.absoluteString,
            "imdb:///title/tt1234567/"
        )
    }

    func testExtractsTitleIDRegardlessOfTrailingPathOrQuery() {
        let web = URL(string: "https://imdb.com/title/tt0111161/?ref_=nv_sr_1")!
        XCTAssertEqual(Film.Ratings.imdbTitleID(from: web), "tt0111161")
    }

    func testNilWhenNoTitleID() {
        XCTAssertNil(Film.Ratings.imdbAppURL(from: URL(string: "https://www.imdb.com/")!))
        XCTAssertNil(Film.Ratings.imdbAppURL(from: nil))
    }

    func testInstancePropertyMirrorsImdbURL() {
        let ratings = Film.Ratings(
            imdb: 7.8,
            imdbURL: URL(string: "https://www.imdb.com/title/tt7654321/"),
            metascore: nil, metacriticURL: nil,
            rottenTomatoes: nil, rottenTomatoesURL: nil,
            filmweb: nil, filmwebURL: nil
        )
        XCTAssertEqual(ratings.imdbAppURL?.absoluteString, "imdb:///title/tt7654321/")
        XCTAssertNil(Film.Ratings.empty.imdbAppURL)
    }
}
