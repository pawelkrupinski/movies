import XCTest
@testable import KinowoCore

final class PosterStoreTests: XCTestCase {
    private var directory: URL!

    override func setUpWithError() throws {
        directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("PosterStoreTests-\(UUID().uuidString)", isDirectory: true)
    }

    override func tearDownWithError() throws {
        try? FileManager.default.removeItem(at: directory)
    }

    private func url(_ s: String) -> URL { URL(string: s)! }
    private func fileExists(_ url: URL) -> Bool {
        FileManager.default.fileExists(
            atPath: directory.appendingPathComponent(PosterStore.fileName(for: url)).path
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

    // MARK: - Disk-first caching

    /// The caching specs below store stand-in byte strings rather than real
    /// artwork, so they say "this is a poster" explicitly instead of leaning
    /// on `decodesAsImage`'s non-UIKit `!data.isEmpty` fallback — which is
    /// what the macOS/Linux toolchain `swift test` runs on today, but would
    /// silently fail every one of them the moment these run against an iOS
    /// destination.
    private let anyBytesAreArtwork: (Data) -> Bool = { !$0.isEmpty }

    func testDownloadsOnceThenServesFromDisk() async {
        let counter = CallCounter()
        let store = PosterStore(directory: directory, fetch: { _ in
            await counter.bump()
            return Data("poster-bytes".utf8)
        }, isImage: anyBytesAreArtwork)
        let first = await store.data(for: url("https://img/a.jpg"))
        let second = await store.data(for: url("https://img/a.jpg"))
        XCTAssertEqual(first, Data("poster-bytes".utf8))
        XCTAssertEqual(second, first)
        let calls = await counter.value
        XCTAssertEqual(calls, 1, "the second load must come off disk, not the network")
    }

    func testSeededPosterIsServedWithoutTouchingTheNetwork() async {
        // The UI-test fixture hook (`KinowoApp.seedUITestPoster`) leans on
        // this: a seeded poster has to be indistinguishable from a downloaded
        // one, so a screen reading the cache renders it even though the URL is
        // unreachable. `DetailPosterCacheUITests` is what that buys us.
        let counter = CallCounter()
        let store = PosterStore(directory: directory, fetch: { _ in
            await counter.bump()
            return nil // the seeded URL must never be fetched
        }, isImage: anyBytesAreArtwork)
        let seeded = url("https://poster.invalid/fixture-poster.png")
        store.seed(Data("seeded-bytes".utf8), for: seeded)

        let loaded = await store.data(for: seeded)
        XCTAssertEqual(loaded, Data("seeded-bytes".utf8))
        let calls = await counter.value
        XCTAssertEqual(calls, 0, "a seeded poster must come off disk without a download")
    }

    // MARK: - Bodies that aren't artwork

    /// A Cloudflare-fronted origin answers a bot challenge with `200
    /// text/html`, which `networkFetch`'s status-code check waves through.
    /// Caching that poisons the entry for good: `reconcile` keeps the file
    /// (the URL is still in the repertoire) and every later read serves the
    /// same undecodable bytes, so the caller's backoff retry never reaches
    /// the network again.
    func testUndecodableDownloadIsNotCached() async {
        let counter = CallCounter()
        let store = PosterStore(
            directory: directory,
            fetch: { _ in
                await counter.bump()
                return Data("<html>Just a moment…</html>".utf8)
            },
            isImage: { $0.starts(with: Data("PNG".utf8)) }
        )
        let challenged = url("https://img/challenged.jpg")

        let first = await store.data(for: challenged)
        XCTAssertNil(first, "an HTML challenge body is not a poster")
        XCTAssertFalse(fileExists(challenged), "it must not reach the cache")

        let second = await store.data(for: challenged)
        XCTAssertNil(second)
        let calls = await counter.value
        XCTAssertEqual(calls, 2, "nothing was cached, so the second read must retry the network")
    }

    /// The recovery leg: an entry already poisoned by an older build is
    /// evicted on the next read and re-downloaded, rather than blanking the
    /// poster on every screen until the film leaves the repertoire.
    func testPoisonedCacheEntryIsEvictedAndRefetched() async {
        let counter = CallCounter()
        let store = PosterStore(
            directory: directory,
            fetch: { _ in
                await counter.bump()
                return Data("PNGgood".utf8)
            },
            isImage: { $0.starts(with: Data("PNG".utf8)) }
        )
        let poisoned = url("https://img/poisoned.jpg")
        store.seed(Data("<html>Just a moment…</html>".utf8), for: poisoned)

        let loaded = await store.data(for: poisoned)
        XCTAssertEqual(loaded, Data("PNGgood".utf8), "the bad bytes must be replaced, not served")
        let calls = await counter.value
        XCTAssertEqual(calls, 1, "the poisoned entry must fall through to a download")

        let again = await store.data(for: poisoned)
        XCTAssertEqual(again, Data("PNGgood".utf8))
        let callsAfter = await counter.value
        XCTAssertEqual(callsAfter, 1, "the replacement is cached like any other poster")
    }

    func testFailedDownloadIsNotCached() async {
        let counter = CallCounter()
        let store = PosterStore(directory: directory, fetch: { _ in
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
        let store = PosterStore(
            directory: directory,
            fetch: { _ in Data("x".utf8) },
            isImage: anyBytesAreArtwork
        )
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
}

/// Async-safe call tally for the injected fetch stub.
private actor CallCounter {
    private(set) var value = 0
    func bump() { value += 1 }
}
