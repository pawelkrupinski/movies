import XCTest
@testable import KinowoCore

/// The catalog is persisted to local storage (`UserDefaults`) so a relaunch
/// starts from the last fetch, not the bundled seed. A fresh `CatalogCache` over
/// the same store reads back exactly what a prior one saved — the relaunch path.
final class CatalogCacheTests: XCTestCase {

    private var defaults: UserDefaults!
    private static let suite = "CatalogCacheTests"

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: Self.suite)!
        defaults.removePersistentDomain(forName: Self.suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: Self.suite)
        super.tearDown()
    }

    func testEmptyUntilSaved() {
        XCTAssertNil(CatalogCache(defaults: defaults).load())
    }

    func testSaveThenLoadRoundTripsAcrossInstances() {
        let body = #"{"countries":[],"cities":[]}"#
        CatalogCache(defaults: defaults).save(body: body, etag: "\"abc123\"")

        // A fresh instance over the same store — exactly what the next launch does.
        let loaded = CatalogCache(defaults: defaults).load()
        XCTAssertEqual(loaded?.body, body)
        XCTAssertEqual(loaded?.etag, "\"abc123\"")
    }

    func testSaveOverwritesPrevious() {
        let cache = CatalogCache(defaults: defaults)
        cache.save(body: "old", etag: "\"1\"")
        cache.save(body: "new", etag: "\"2\"")
        let loaded = CatalogCache(defaults: defaults).load()
        XCTAssertEqual(loaded?.body, "new")
        XCTAssertEqual(loaded?.etag, "\"2\"")
    }
}
