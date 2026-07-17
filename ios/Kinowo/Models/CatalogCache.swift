import Foundation

/// Durable local persistence of the last fetched catalog — the raw
/// `{countries,cities}` body plus its ETag — in `UserDefaults`. So a relaunch
/// starts from the last fetch (not the bundled seed), and the first fetch
/// revalidates with the persisted ETag (a 304 when nothing changed).
///
/// `UserDefaults`-backed (not the caches directory) so the OS never evicts it
/// under storage pressure. The store injects a throwaway suite in tests to
/// round-trip without touching the real defaults.
struct CatalogCache {
    private let defaults: UserDefaults
    private let bodyKey = "catalogBody"
    private let etagKey = "catalogEtag"

    init(defaults: UserDefaults = .standard) {
        self.defaults = defaults
    }

    /// The persisted `(body, etag)`, or `nil` when nothing has been saved yet
    /// (a fresh install, before the first successful fetch).
    func load() -> (body: String, etag: String?)? {
        guard let body = defaults.string(forKey: bodyKey) else { return nil }
        return (body, defaults.string(forKey: etagKey))
    }

    /// Persist the freshly-fetched body + its ETag, overwriting the prior one.
    func save(body: String, etag: String?) {
        defaults.set(body, forKey: bodyKey)
        defaults.set(etag, forKey: etagKey)
    }
}
