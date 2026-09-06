import XCTest
@testable import KinowoCore

/// A poster URL is copied verbatim from whatever CDN a cinema publishes. One the
/// server emits that Foundation refuses used to fail the whole `[Film]` decode —
/// the fixture-server contract test found it at index 14 of Poznań's repertoire —
/// so every film vanished for the sake of one poster. The poster is decoration;
/// the film and its showings are the content.
final class LenientPosterURLTests: XCTestCase {

    private func film(poster: String, fallbacks: [String] = []) -> String {
        let fallbackJSON = fallbacks.map { "\"\($0)\"" }.joined(separator: ",")
        return """
        {
          "title": "Film",
          "posterURL": \(poster),
          "fallbackPosterURLs": [\(fallbackJSON)],
          "runtimeMinutes": 100,
          "releaseYear": 2026,
          "genres": [],
          "ratings": {},
          "countries": [],
          "directors": [],
          "cast": [],
          "showings": []
        }
        """
    }

    private func decode(_ json: String) throws -> [Film] {
        try JSONDecoder().decode([Film].self, from: Data(json.utf8))
    }

    func testAPosterWithASpaceStillDecodesAsAURL() throws {
        let films = try decode("[\(film(poster: "\"https://cdn.example/plakaty/Plakat z filmu.jpg\""))]")
        XCTAssertEqual(films.count, 1)
        XCTAssertNotNil(films[0].posterURL)
        XCTAssertEqual(films[0].posterURL?.host, "cdn.example")
    }

    func testAPosterNoParserCanReadIsNilAndTheFilmSurvives() throws {
        let films = try decode("[\(film(poster: "\"   \"")), \(film(poster: "\"https://ok.example/p.jpg\""))]")
        XCTAssertEqual(films.count, 2)
        XCTAssertNil(films[0].posterURL)
        XCTAssertEqual(films[1].posterURL?.absoluteString, "https://ok.example/p.jpg")
    }

    func testAnAbsentOrNullPosterIsNil() throws {
        let absent = """
        [{"title": "Film", "fallbackPosterURLs": [], "runtimeMinutes": 100, "releaseYear": 2026,
          "genres": [], "ratings": {}, "countries": [], "directors": [], "cast": [], "showings": []}]
        """
        XCTAssertNil(try decode(absent)[0].posterURL)
        XCTAssertNil(try decode("[\(film(poster: "null"))]")[0].posterURL)
    }

    func testAnUnreadableFallbackIsDroppedNotFatal() throws {
        let films = try decode("[\(film(poster: "null", fallbacks: ["https://a.example/1.jpg", "", "https://b.example/ż ó.jpg"]))]")
        XCTAssertEqual(films[0].fallbackPosterURLs.count, 2)
        XCTAssertEqual(films[0].fallbackPosterURLs[0].host, "a.example")
        XCTAssertEqual(films[0].fallbackPosterURLs[1].host, "b.example")
    }

    func testEncodingRoundTrips() throws {
        let films = try decode("[\(film(poster: "\"https://ok.example/p.jpg\"", fallbacks: ["https://a.example/1.jpg"]))]")
        let again = try decode(String(decoding: JSONEncoder().encode(films), as: UTF8.self))
        XCTAssertEqual(again, films)
    }
}
