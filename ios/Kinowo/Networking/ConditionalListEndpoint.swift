import Foundation
// See `RepertoireClient.swift`: a no-op in the Xcode app's flat module, true
// only in the `KinowoNetworking` SPM target that lets `swift test` drive this.
#if canImport(KinowoCore)
@testable import KinowoCore
#endif

/// What one `ConditionalListEndpoint.fetch` came back with.
enum ConditionalFetchOutcome<Payload> {
    /// A 2xx body, already written to the disk cache.
    case fresh([Payload])
    /// 304: the cached body is current. Carries that body when the caller
    /// said it holds nothing (see `ConditionalPayloadCache.bodyForNotModified`),
    /// nil when the caller's copy should stand.
    case notModified(cached: [Payload]?)
    /// The endpoint was re-pointed (city/country switch) while this request
    /// was in flight — its answer belongs to the OLD city and must be dropped.
    case superseded
}

/// The `/{city}/api/{endpoint}` list fetch `RepertoireStore` and
/// `DetailsStore` share: which deployment + city it points at, the
/// Last-Modified conditional GET against a `ConditionalPayloadCache`, the
/// freshness stamp behind `reloadIfStale`, and dropping a response that
/// lands after a switch re-pointed it. The stores keep only their own
/// published state and what they do with a payload.
@MainActor
final class ConditionalListEndpoint<Payload: Codable> {
    private(set) var base: URL
    private(set) var citySlug: String
    private(set) var url: URL
    private let endpoint: String
    private let cache: ConditionalPayloadCache<Payload>
    private let session: URLSession
    private var lastReloadedAt: Date?

    private let staleAfter: TimeInterval = 60

    init(base: URL, citySlug: String, endpoint: String,
         cache: ConditionalPayloadCache<Payload>, session: URLSession) {
        self.base = base
        self.citySlug = citySlug
        self.endpoint = endpoint
        self.cache = cache
        self.session = session
        self.url = City.apiURL(base: base, slug: citySlug, endpoint: endpoint)
    }

    /// Re-point at `base` + `citySlug` (either may be unchanged). Returns
    /// false — and changes nothing — when that is the URL already in use;
    /// otherwise forgets the freshness stamp so the next reload fetches.
    func repoint(base: URL? = nil, citySlug: String? = nil) -> Bool {
        let nextBase = base ?? self.base
        let nextCity = citySlug ?? self.citySlug
        let next = City.apiURL(base: nextBase, slug: nextCity, endpoint: endpoint)
        guard next != url else { return false }
        self.base = nextBase
        self.citySlug = nextCity
        url = next
        lastReloadedAt = nil
        return true
    }

    /// Whether the last successful load is older than a minute (or never happened).
    func isStale(now: Date) -> Bool {
        guard let last = lastReloadedAt else { return true }
        return now.timeIntervalSince(last) >= staleAfter
    }

    /// Stamp a load that bypassed `fetch` (the UI-test fixture path).
    func markReloaded(now: Date) { lastReloadedAt = now }

    /// The disk-cached body for the current deployment + city, if any.
    func cachedBody() -> [Payload]? { cache.load(deployment: base, city: citySlug) }

    /// Conditional GET of the current URL. `callerIsEmpty` is asked once the
    /// response lands, so a 304 hands back the cached body exactly when the
    /// caller is holding nothing. A failure after the endpoint was re-pointed
    /// is reported as `.superseded`, not thrown: it belongs to the old city.
    func fetch(now: Date, callerIsEmpty: () -> Bool) async throws -> ConditionalFetchOutcome<Payload> {
        let requestURL = url
        let city = citySlug
        let deployment = base
        do {
            var request = URLRequest(url: requestURL)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            if let lm = cache.lastModified(deployment: deployment, city: city) {
                request.setValue(lm, forHTTPHeaderField: "If-Modified-Since")
            }
            let (data, response) = try await session.data(for: request)
            guard url == requestURL else { return .superseded }
            guard let http = response as? HTTPURLResponse else { throw URLError(.badServerResponse) }
            if http.statusCode == 304 {
                lastReloadedAt = now
                return .notModified(cached: cache.bodyForNotModified(
                    callerIsEmpty: callerIsEmpty(), deployment: deployment, city: city))
            }
            guard (200..<300).contains(http.statusCode) else { throw URLError(.badServerResponse) }
            let decoded = try JSONDecoder().decode([Payload].self, from: data)
            lastReloadedAt = now
            let lm = http.value(forHTTPHeaderField: "Last-Modified")
            let cache = self.cache
            Task.detached { cache.save(decoded, deployment: deployment, city: city, lastModified: lm) }
            return .fresh(decoded)
        } catch {
            guard url == requestURL else { return .superseded }
            throw error
        }
    }
}
