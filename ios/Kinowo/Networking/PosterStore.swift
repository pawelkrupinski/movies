import Foundation
#if canImport(FoundationNetworking)
// On Linux (swift-corelibs-foundation, used by `swift test` in CI)
// URLSession/URLRequest live in this separate module, not Foundation.
import FoundationNetworking
#endif

/// Disk-backed cache for poster images so each poster downloads at most
/// once and survives across launches — the iOS counterpart of Android's
/// Coil disk cache. SwiftUI's `AsyncImage` leans on `URLCache`, which is
/// memory-biased and small, so posters were re-fetched far more than they
/// should be; this stores the bytes as one file per poster URL under
/// `Caches/Posters` and serves them straight off disk on the next load.
///
/// `reconcile(keepURLs:)` is the once-a-day purge: it keeps only the
/// posters whose URL is still in the repertoire (films with a future
/// screening) and deletes the rest, so the cache can't grow without bound
/// as films finish their run.
///
/// Foundation-only — it deals in raw `Data`, never `UIImage` — so it
/// builds in the `KinowoCore` SPM target and is unit-tested without a
/// simulator. The SwiftUI glue that decodes the bytes into an `Image`
/// lives in `CachedAsyncImage`.
final class PosterStore: @unchecked Sendable {
    /// Production singleton — caches under the app's Caches directory and
    /// downloads through a cache-bypassing `URLSession`.
    static let shared = PosterStore()

    private let directory: URL
    private let fetch: (URL) async -> Data?

    /// - Parameters:
    ///   - directory: where poster files live. Defaults to
    ///     `Caches/Posters`. Tests pass a throwaway temp directory.
    ///   - fetch: downloads the bytes for a URL, or returns `nil` on any
    ///     non-2xx / transport error. Defaults to a cache-bypassing
    ///     `URLSession` (this disk store *is* the cache); tests inject a
    ///     stub so the cache logic is exercised without the network.
    init(
        directory: URL = PosterStore.defaultDirectory,
        fetch: @escaping (URL) async -> Data? = PosterStore.networkFetch
    ) {
        self.directory = directory
        self.fetch = fetch
        try? FileManager.default.createDirectory(
            at: directory, withIntermediateDirectories: true
        )
    }

    /// Bytes for `url`, disk-first. On a miss we download via `fetch`,
    /// persist the bytes, and return them. A failed download returns `nil`
    /// and writes nothing — a transient 4xx must not be cached as a
    /// permanent blank; the caller walks its fallback chain instead and we
    /// retry the URL next time.
    func data(for url: URL) async -> Data? {
        let file = fileURL(for: url)
        if let cached = try? Data(contentsOf: file) {
            return cached
        }
        guard let downloaded = await fetch(url) else { return nil }
        try? downloaded.write(to: file, options: .atomic)
        return downloaded
    }

    /// The daily purge. `keepURLs` is every poster + fallback URL across
    /// the films currently in the repertoire; any cached file whose URL
    /// isn't among them is deleted. Because the repertoire is already
    /// pruned to films with future screenings, this drops the posters of
    /// films that have finished their run — and also any orphan left by an
    /// old URL a still-showing film has since rotated away from.
    ///
    /// `async` (though the work is synchronous file IO) so callers on the
    /// main actor run the directory walk off the main thread.
    func reconcile(keepURLs: [URL]) async {
        let keepNames = Set(keepURLs.map { Self.fileName(for: $0) })
        let fm = FileManager.default
        guard let entries = try? fm.contentsOfDirectory(
            at: directory, includingPropertiesForKeys: nil
        ) else { return }
        for entry in entries where !keepNames.contains(entry.lastPathComponent) {
            try? fm.removeItem(at: entry)
        }
    }

    // MARK: - Keying

    private func fileURL(for url: URL) -> URL {
        directory.appendingPathComponent(Self.fileName(for: url))
    }

    /// Stable, process-independent filename for a poster URL. Swift's
    /// `Hasher` is seeded per launch so it can't key an on-disk cache;
    /// FNV-1a 64-bit is deterministic across runs. The retry-bust query
    /// param (`_kinowo_t`, added by `PosterImage` to dodge a stale failure
    /// on retry) is stripped first, so a poster fetched on a retry caches
    /// under the same key as its canonical URL — and so `reconcile`, which
    /// only ever sees canonical film URLs, still matches it.
    static func fileName(for url: URL) -> String {
        let key = canonicalString(for: url)
        var hash: UInt64 = 0xcbf2_9ce4_8422_2325
        for byte in key.utf8 {
            hash ^= UInt64(byte)
            hash = hash &* 0x0000_0100_0000_01b3
        }
        return String(hash, radix: 16) + ".img"
    }

    /// `url` with our own `_kinowo_t` retry token removed, so the canonical
    /// and tokened forms collapse to one cache key.
    private static func canonicalString(for url: URL) -> String {
        guard var comps = URLComponents(url: url, resolvingAgainstBaseURL: false),
              let items = comps.queryItems else {
            return url.absoluteString
        }
        let kept = items.filter { $0.name != "_kinowo_t" }
        comps.queryItems = kept.isEmpty ? nil : kept
        return comps.url?.absoluteString ?? url.absoluteString
    }

    // MARK: - Production defaults

    static var defaultDirectory: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("Posters", isDirectory: true)
    }

    private static let session: URLSession = {
        let config = URLSessionConfiguration.default
        // This disk store is the cache; don't double-cache through URLCache.
        config.urlCache = nil
        config.requestCachePolicy = .reloadIgnoringLocalCacheData
        return URLSession(configuration: config)
    }()

    static func networkFetch(_ url: URL) async -> Data? {
        var request = URLRequest(url: url)
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        guard let (data, response) = try? await session.data(for: request),
              let http = response as? HTTPURLResponse,
              (200..<300).contains(http.statusCode),
              !data.isEmpty else {
            return nil
        }
        return data
    }
}
