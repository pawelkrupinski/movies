import XCTest
@testable import KinowoCore

final class RepertoireCacheLastModifiedTests: XCTestCase {

    override func tearDown() {
        // Reset the city-bound meta so cases don't leak into one another.
        RepertoireCache.save([], city: "", lastModified: nil)
        super.tearDown()
    }

    func testSaveAndLoadLastModifiedForSameCity() {
        let value = "Sun, 25 May 2026 10:00:00 GMT"
        RepertoireCache.save([], city: "poznan", lastModified: value)
        XCTAssertEqual(RepertoireCache.lastModified(forCity: "poznan"), value)
    }

    /// The server's `Last-Modified` is a single global value, so replaying
    /// poznań's timestamp while fetching warszawa would draw a 304 and strand
    /// the grid on the old city. A different city must therefore get no
    /// conditional header (nil).
    func testLastModifiedIsNilForADifferentCity() {
        RepertoireCache.save([], city: "poznan", lastModified: "Sun, 25 May 2026 10:00:00 GMT")
        XCTAssertNil(RepertoireCache.lastModified(forCity: "warszawa"))
    }

    func testLastModifiedReturnsNilWhenNotSaved() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("repertoire-meta.txt")
        try? FileManager.default.removeItem(at: url)
        XCTAssertNil(RepertoireCache.lastModified(forCity: "poznan"))
    }
}
