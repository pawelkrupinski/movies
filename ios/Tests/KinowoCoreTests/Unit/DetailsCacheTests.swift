import XCTest
@testable import KinowoCore

final class DetailsCacheTests: XCTestCase {

    override func tearDown() {
        DetailsCache.saveLastModified("")
        DetailsCache.save([])
        super.tearDown()
    }

    func testSaveAndLoadDetailsRoundTrips() {
        let details = [
            FilmDetails(title: "A", synopsis: "opis", trailerURLs: [URL(string: "https://x/embed/1")!]),
            FilmDetails(title: "B", synopsis: nil, trailerURLs: []),
        ]
        DetailsCache.save(details)
        XCTAssertEqual(DetailsCache.load(), details)
    }

    func testSaveAndLoadLastModified() {
        let value = "Sun, 31 May 2026 10:00:00 GMT"
        DetailsCache.saveLastModified(value)
        XCTAssertEqual(DetailsCache.loadLastModified(), value)
    }

    func testLoadLastModifiedReturnsNilWhenNotSaved() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("details-lastmodified.txt")
        try? FileManager.default.removeItem(at: url)
        XCTAssertNil(DetailsCache.loadLastModified())
    }
}
