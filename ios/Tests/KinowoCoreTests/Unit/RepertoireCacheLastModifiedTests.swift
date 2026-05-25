import XCTest
@testable import KinowoCore

final class RepertoireCacheLastModifiedTests: XCTestCase {

    override func tearDown() {
        RepertoireCache.saveLastModified("")
        super.tearDown()
    }

    func testSaveAndLoadLastModified() {
        let value = "Sun, 25 May 2026 10:00:00 GMT"
        RepertoireCache.saveLastModified(value)
        XCTAssertEqual(RepertoireCache.loadLastModified(), value)
    }

    func testLoadLastModifiedReturnsNilWhenNotSaved() {
        let url = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("repertoire-lastmodified.txt")
        try? FileManager.default.removeItem(at: url)
        XCTAssertNil(RepertoireCache.loadLastModified())
    }
}
