import XCTest
@testable import KinowoCore

/// `/api/repertoire` serves each film's `searchTitles` (its original title and that title's
/// romanisation, server-side `SearchTitles`), what the search matches besides the title. A
/// server that predates the key must still decode, with nothing extra to match.
final class SearchTitlesDecodeTests: XCTestCase {

    private let json = """
    [
      {
        "title": "Zawieście czerwone latarnie",
        "searchTitles": ["大红灯笼高高挂", "da hong deng long gao gao gua"],
        "posterURL": null, "fallbackPosterURLs": [], "runtimeMinutes": 125, "releaseYear": 1991,
        "genres": [], "ratings": {}, "countries": [], "directors": [], "cast": [], "showings": []
      },
      {
        "title": "Older Server Film",
        "posterURL": null, "fallbackPosterURLs": [], "runtimeMinutes": 95, "releaseYear": 2026,
        "genres": [], "ratings": {}, "countries": [], "directors": [], "cast": [], "showings": []
      }
    ]
    """

    func testDecodesSearchTitlesAndToleratesTheirAbsence() throws {
        let films = try JSONDecoder().decode([Film].self, from: Data(json.utf8))
        XCTAssertEqual(films.map(\.searchTitles), [["大红灯笼高高挂", "da hong deng long gao gao gua"], nil])
        XCTAssertTrue(films[0].matchesSearch("gao gao"))
        XCTAssertFalse(films[1].matchesSearch("gao gao"))
        XCTAssertTrue(films[1].matchesSearch("older"))
    }
}
