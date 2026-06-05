import XCTest
@testable import KinowoCore

final class DetailsCacheTests: XCTestCase {

    override func tearDown() {
        DetailsCache.save([], city: "", lastModified: nil)
        super.tearDown()
    }

    func testSaveAndLoadDetailsRoundTrips() {
        let details = [
            FilmDetails(title: "A", synopsis: "opis", trailerURLs: [URL(string: "https://x/embed/1")!]),
            FilmDetails(title: "B", synopsis: nil, trailerURLs: []),
        ]
        DetailsCache.save(details, city: "poznan", lastModified: nil)
        XCTAssertEqual(DetailsCache.load(), details)
    }

    func testSaveAndLoadLastModifiedForSameCity() {
        let value = "Sun, 31 May 2026 10:00:00 GMT"
        DetailsCache.save([], city: "poznan", lastModified: value)
        XCTAssertEqual(DetailsCache.lastModified(forCity: "poznan"), value)
    }

    /// See `RepertoireCacheLastModifiedTests`: the global server timestamp must
    /// not be replayed across a city switch.
    func testLastModifiedIsNilForADifferentCity() {
        DetailsCache.save([], city: "poznan", lastModified: "Sun, 31 May 2026 10:00:00 GMT")
        XCTAssertNil(DetailsCache.lastModified(forCity: "warszawa"))
    }

    func testLastModifiedReturnsNilWhenNotSaved() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("details-meta.txt")
        try? FileManager.default.removeItem(at: url)
        XCTAssertNil(DetailsCache.lastModified(forCity: "poznan"))
    }
}
