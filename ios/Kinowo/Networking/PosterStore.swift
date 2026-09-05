import Foundation
#if canImport(UIKit)
import UIKit
#endif
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
    private let isImage: (Data) -> Bool

    /// - Parameters:
    ///   - directory: where poster files live. Defaults to
    ///     `Caches/Posters`. Tests pass a throwaway temp directory.
    ///   - fetch: downloads the bytes for a URL, or returns `nil` on any
    ///     non-2xx / transport error. Defaults to a cache-bypassing
    ///     `URLSession` (this disk store *is* the cache); tests inject a
    ///     stub so the cache logic is exercised without the network.
    ///   - isImage: whether a body is artwork we can actually render.
    ///     Defaults to a real decode on iOS. `networkFetch` only checks
    ///     the status code, and a Cloudflare-fronted origin answers a bot
    ///     challenge with `200 text/html` — bytes that would otherwise sit
    ///     in the cache forever, blanking the poster on every screen that
    ///     reads it (`reconcile` keeps them: the URL is still in the
    ///     repertoire).
    init(
        directory: URL = PosterStore.defaultDirectory,
        fetch: @escaping (URL) async -> Data? = PosterStore.networkFetch,
        isImage: @escaping (Data) -> Bool = PosterStore.decodesAsImage
    ) {
        self.directory = directory
        self.fetch = fetch
        self.isImage = isImage
        try? FileManager.default.createDirectory(
            at: directory, withIntermediateDirectories: true
        )
    }

    /// Bytes for `url`, disk-first. On a miss we download via `fetch`,
    /// persist the bytes, and return them. A failed download returns `nil`
    /// and writes nothing — a transient 4xx must not be cached as a
    /// permanent blank; the caller walks its fallback chain instead and we
    /// retry the URL next time.
    ///
    /// A body that isn't decodable artwork is treated the same way as a
    /// failed download, on both legs: never cached, and evicted if it
    /// somehow already is. Without that, one 200-with-an-HTML-challenge
    /// poisons the entry permanently — the caller's backoff retry re-reads
    /// the same bad bytes off disk and never reaches the network again.
    func data(for url: URL) async -> Data? {
        let file = fileURL(for: url)
        if let cached = try? Data(contentsOf: file) {
            if isImage(cached) { return cached }
            try? FileManager.default.removeItem(at: file)
        }
        guard let downloaded = await fetch(url), isImage(downloaded) else { return nil }
        try? downloaded.write(to: file, options: .atomic)
        return downloaded
    }

    /// Put `data` in the cache for `url` without going near the network.
    ///
    /// The one production caller is the UI-test fixture hook, which primes a
    /// poster the app can then only render by reading the cache — that's what
    /// makes "does this screen use the cache?" observable from a UI test at
    /// all. `data(for:)` reads the same file, so a seeded poster is
    /// indistinguishable from a downloaded one.
    func seed(_ data: Data, for url: URL) {
        try? data.write(to: fileURL(for: url), options: .atomic)
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
    /// FNV-1a 64-bit is deterministic across runs.
    static func fileName(for url: URL) -> String {
        var hash: UInt64 = 0xcbf2_9ce4_8422_2325
        for byte in url.absoluteString.utf8 {
            hash ^= UInt64(byte)
            hash = hash &* 0x0000_0100_0000_01b3
        }
        return String(hash, radix: 16) + ".img"
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

    /// Does `data` decode as an image? Real ImageIO on the app's platforms;
    /// on the Linux toolchain `swift test` runs against there's no image
    /// framework, so it falls back to "not empty" — the tests that care
    /// inject their own predicate rather than relying on the default.
    static func decodesAsImage(_ data: Data) -> Bool {
        #if canImport(UIKit)
        return UIImage(data: data) != nil
        #else
        return !data.isEmpty
        #endif
    }

    static func networkFetch(_ url: URL) async -> Data? {
        var request = URLRequest(url: url)
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        // Completion-handler `dataTask` bridged through a continuation
        // rather than the async `data(for:)` — the latter isn't available
        // on the Linux swift-corelibs-foundation toolchain CI compiles
        // KinowoCore against. `dataTask` exists on both.
        return await withCheckedContinuation { continuation in
            let task = session.dataTask(with: request) { data, response, _ in
                guard let data,
                      let http = response as? HTTPURLResponse,
                      (200..<300).contains(http.statusCode),
                      !data.isEmpty else {
                    continuation.resume(returning: nil)
                    return
                }
                continuation.resume(returning: data)
            }
            task.resume()
        }
    }
}
