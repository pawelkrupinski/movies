import XCTest
@testable import KinowoCore

/// The `/api/repertoire` film JSON (decoded as `[Film]` by `RepertoireStore`)
/// carries an optional `ageRating` certificate string. It's present only for
/// films that actually hold one (UK / BBFC today) and absent for everything
/// else, so the key must decode when present and fall back to `nil` when the
/// server omits it — never failing the whole row.
final class AgeRatingDecodeTests: XCTestCase {

    /// Two films in the wire shape `/api/repertoire` serves: the first carries
    /// an `ageRating`, the second omits the key entirely (the common case).
    private let json = """
    [
      {
        "title": "Certificated Film",
        "posterURL": null,
        "fallbackPosterURLs": [],
        "runtimeMinutes": 118,
        "releaseYear": 2026,
        "genres": ["Drama"],
        "ageRating": "15",
        "ratings": {},
        "countries": ["United Kingdom"],
        "directors": [],
        "cast": [],
        "showings": []
      },
      {
        "title": "Uncertificated Film",
        "posterURL": null,
        "fallbackPosterURLs": [],
        "runtimeMinutes": 95,
        "releaseYear": 2026,
        "genres": ["Comedy"],
        "ratings": {},
        "countries": ["Poland"],
        "directors": [],
        "cast": [],
        "showings": []
      }
    ]
    """

    private func decodeFilms() throws -> [Film] {
        try JSONDecoder().decode([Film].self, from: Data(json.utf8))
    }

    func testDecodesAgeRatingWhenPresent() throws {
        let films = try decodeFilms()
        XCTAssertEqual(films.first?.ageRating, "15")
    }

    func testMissingAgeRatingKeyDecodesToNil() throws {
        let films = try decodeFilms()
        XCTAssertNil(films.last?.ageRating)
    }

    func testAllRowsDecodeRegardlessOfAgeRating() throws {
        // The absent key mustn't fail the whole array — both rows survive.
        XCTAssertEqual(try decodeFilms().count, 2)
    }
}
