import XCTest
@testable import KinowoCore

final class PosterStoreTests: XCTestCase {
    private var dir: URL!

    override func setUpWithError() throws {
        dir = FileManager.default.temporaryDirectory
            .appendingPathComponent("PosterStoreTests-\(UUID().uuidString)", isDirectory: true)
    }

    override func tearDownWithError() throws {
        try? FileManager.default.removeItem(at: dir)
    }

    private func url(_ s: String) -> URL { URL(string: s)! }
    private func fileExists(_ url: URL) -> Bool {
        FileManager.default.fileExists(
            atPath: dir.appendingPathComponent(PosterStore.fileName(for: url)).path
        )
    }

    // MARK: - Keying

    func testCacheKeyIsStableAndURLSpecific() {
        let a = PosterStore.fileName(for: url("https://img/x.jpg"))
        let again = PosterStore.fileName(for: url("https://img/x.jpg"))
        let other = PosterStore.fileName(for: url("https://img/y.jpg"))
        XCTAssertEqual(a, again, "same URL must hash to the same on-disk key across calls")
        XCTAssertNotEqual(a, other, "different URLs must not collide")
        XCTAssertTrue(a.hasSuffix(".img"))
    }

    func testRetryTokenIsStrippedFromKey() {
        let canonical = PosterStore.fileName(for: url("https://img/x.jpg"))
        let tokened = PosterStore.fileName(for: url("https://img/x.jpg?_kinowo_t=3"))
        XCTAssertEqual(canonical, tokened, "the _kinowo_t retry token must not fork the cache key")
        let realParam = PosterStore.fileName(for: url("https://img/x.jpg?v=2"))
        XCTAssertNotEqual(canonical, realParam, "a genuine query param still keys distinctly")
    }

    // MARK: - Disk-first caching

    func testDownloadsOnceThenServesFromDisk() async {
        let counter = CallCounter()
        let store = PosterStore(directory: dir, fetch: { _ in
            await counter.bump()
            return Data("poster-bytes".utf8)
        })
        let first = await store.data(for: url("https://img/a.jpg"))
        let second = await store.data(for: url("https://img/a.jpg"))
        XCTAssertEqual(first, Data("poster-bytes".utf8))
        XCTAssertEqual(second, first)
        let calls = await counter.value
        XCTAssertEqual(calls, 1, "the second load must come off disk, not the network")
    }

    func testFailedDownloadIsNotCached() async {
        let counter = CallCounter()
        let store = PosterStore(directory: dir, fetch: { _ in
            await counter.bump()
            return nil // simulate a non-2xx / transport failure
        })
        let first = await store.data(for: url("https://img/b.jpg"))
        let second = await store.data(for: url("https://img/b.jpg"))
        XCTAssertNil(first)
        XCTAssertNil(second)
        let calls = await counter.value
        XCTAssertEqual(calls, 2, "a failure must not be persisted as a blank — the URL is retried")
    }

    // MARK: - Daily purge

    func testReconcileDeletesDepartedFilmsAndKeepsCurrentOnes() async {
        let store = PosterStore(directory: dir, fetch: { _ in Data("x".utf8) })
        let keep = url("https://img/keep.jpg")
        let drop = url("https://img/drop.jpg")
        _ = await store.data(for: keep)
        _ = await store.data(for: drop)
        XCTAssertTrue(fileExists(keep))
        XCTAssertTrue(fileExists(drop))

        await store.reconcile(keepURLs: [keep])

        XCTAssertTrue(fileExists(keep), "a film still in the repertoire keeps its poster")
        XCTAssertFalse(fileExists(drop), "a film with no future screening loses its poster")
    }

    func testReconcileMatchesTokenedCacheAgainstCanonicalKeepURL() async {
        // A poster fetched on a retry is stored under a tokened URL; the
        // reconcile only knows the canonical film URL. The token-stripped
        // key must let the canonical keep-URL protect that cached file.
        let store = PosterStore(directory: dir, fetch: { _ in Data("x".utf8) })
        let tokened = url("https://img/c.jpg?_kinowo_t=5")
        _ = await store.data(for: tokened)

        await store.reconcile(keepURLs: [url("https://img/c.jpg")])

        XCTAssertTrue(fileExists(url("https://img/c.jpg")),
                      "a retry-fetched poster must survive a reconcile keyed on the canonical URL")
    }
}

/// Async-safe call tally for the injected fetch stub.
private actor CallCounter {
    private(set) var value = 0
    func bump() { value += 1 }
}
