import XCTest
@testable import KinowoCore

/// Decodes a sample `/api/details` array (captured shape:
/// `{ title, synopsis?, trailerURLs[] }`) through `FilmDetails` and
/// asserts the lookup map the `DetailsStore` builds.
final class FilmDetailsDecodingTests: XCTestCase {

    private func loadDetails() throws -> [FilmDetails] {
        let json = try Fixtures.load("api_details", ext: "json")
        let data = Data(json.utf8)
        return try JSONDecoder().decode([FilmDetails].self, from: data)
    }

    func testDecodesAllRows() throws {
        let details = try loadDetails()
        XCTAssertEqual(details.count, 4)
    }

    func testDecodesSynopsisAndTrailers() throws {
        let map = try loadDetails().keyedByTitle()
        let mando = map["Mandalorian i Grogu"]
        XCTAssertNotNil(mando)
        XCTAssertEqual(mando?.synopsis?.isEmpty, false)
        XCTAssertEqual(mando?.trailerURLs.count, 2)
        XCTAssertEqual(mando?.trailerURLs.first?.absoluteString,
                       "https://www.youtube.com/embed/abc123XYZ")
    }

    func testNullSynopsisDecodesToNil() throws {
        let map = try loadDetails().keyedByTitle()
        XCTAssertNil(map["Film z uszkodzonym linkiem"]?.synopsis)
    }

    func testEmptyTrailersDecodeToEmptyArray() throws {
        let map = try loadDetails().keyedByTitle()
        XCTAssertEqual(map["Film bez zwiastuna"]?.trailerURLs, [])
    }

    func testDecodesOriginalTitleWhenPresent() throws {
        let map = try loadDetails().keyedByTitle()
        XCTAssertEqual(map["Mandalorian i Grogu"]?.originalTitle, "The Mandalorian and Grogu")
    }

    func testMissingOriginalTitleDecodesToNil() throws {
        let map = try loadDetails().keyedByTitle()
        // Most rows omit it (the backend only sends a distinct original title),
        // and the absent key must decode to nil rather than failing the row.
        XCTAssertNil(map["Diuna: Część druga"]?.originalTitle)
    }

    func testInvalidTrailerURLIsDropped() throws {
        let map = try loadDetails().keyedByTitle()
        // The empty-string entry isn't a valid URL, so it's dropped
        // while the valid sibling survives — mirroring how `Film`
        // decodes URLs (drop-on-invalid, don't fail the whole row).
        let trailers = map["Film z uszkodzonym linkiem"]?.trailerURLs
        XCTAssertEqual(trailers?.count, 1)
        XCTAssertEqual(trailers?.first?.absoluteString,
                       "https://www.youtube.com/embed/valid123")
    }

    func testKeyedByTitleUsesTitleAsKey() throws {
        let map = try loadDetails().keyedByTitle()
        XCTAssertEqual(Set(map.keys), [
            "Mandalorian i Grogu",
            "Diuna: Część druga",
            "Film bez zwiastuna",
            "Film z uszkodzonym linkiem",
        ])
    }
}
