import Foundation

/// Fetches `/{city}/api/details` and exposes a title → details lookup the
/// detail screen reads synopsis + trailers from. Analogous to
/// `RepertoireStore`: same User-Agent, cache policy, and Last-Modified
/// conditional-GET + on-disk caching pattern, but keyed into a map so
/// `FilmDetailView` can resolve a single film in O(1).
///
/// The business logic — JSON decode and `keyedByTitle` — lives on
/// `[FilmDetails]` in `KinowoCore`, so this stays a thin URLSession
/// shim and the map-building rule is unit-tested without the network.
@MainActor
final class DetailsStore: ObservableObject {
    /// title → details. Sparse: the backend only ships films that have
    /// a synopsis or a trailer, so a missing key is the common case.
    @Published private(set) var byTitle: [String: FilmDetails] = [:]

    private var base: URL
    private var url: URL
    private var citySlug: String
    private let session: URLSession
    private var lastReloadedAt: Date?

    private let staleAfter: TimeInterval = 60

    /// `base` is the bare host; the fetch URL is `…/{citySlug}/api/details`.
    /// Same city-qualification contract as `RepertoireStore`.
    init(base: URL = kinowoBaseURL, citySlug: String = City.default.slug, session: URLSession = .shared) {
        self.base = base
        self.citySlug = citySlug
        self.url = City.apiURL(base: base, slug: citySlug, endpoint: "details")
        self.session = session
    }

    /// Re-point at a different country's deployment: rebuild the base + URL and
    /// force the next fetch (see `RepertoireStore.use(country:)`).
    func use(country: Country) {
        let next = City.apiURL(base: country.baseURL, slug: citySlug, endpoint: "details")
        guard next != url else { return }
        base = country.baseURL
        url = next
        lastReloadedAt = nil
        Task { await reload() }
    }

    /// Re-point at a different city (see `RepertoireStore.use`).
    func use(citySlug: String) {
        let next = City.apiURL(base: base, slug: citySlug, endpoint: "details")
        guard next != url else { return }
        url = next
        self.citySlug = citySlug
        lastReloadedAt = nil
        Task { await reload() }
    }

    /// Synopsis + trailers for a listing title, or `nil` when the
    /// backend had neither for this film (or details haven't loaded yet).
    func details(for title: String) -> FilmDetails? { byTitle[title] }

    func loadCachedData() {
        if byTitle.isEmpty, let cached = DetailsCache.load() {
            byTitle = cached.keyedByTitle()
        }
    }

    func reload(now: Date = Date()) async {
        do {
            var request = URLRequest(url: url)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            if let lm = DetailsCache.lastModified(forCity: citySlug) {
                request.setValue(lm, forHTTPHeaderField: "If-Modified-Since")
            }
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            if http.statusCode == 304 {
                self.lastReloadedAt = now
                return
            }
            guard (200..<300).contains(http.statusCode) else {
                throw URLError(.badServerResponse)
            }
            let decoded = try JSONDecoder().decode([FilmDetails].self, from: data)
            self.byTitle = decoded.keyedByTitle()
            self.lastReloadedAt = now
            let lm = http.value(forHTTPHeaderField: "Last-Modified")
            let copy = decoded
            let city = citySlug
            Task.detached { DetailsCache.save(copy, city: city, lastModified: lm) }
        } catch {
            // Details are non-essential — the listing still renders the
            // film without synopsis/trailers. Swallow the error rather
            // than surfacing it; a stale or empty map is acceptable.
        }
    }

    func reloadIfStale(now: Date = Date()) async {
        if let last = lastReloadedAt, now.timeIntervalSince(last) < staleAfter {
            return
        }
        await reload(now: now)
    }

    /// Build a store pre-seeded with details, bypassing the network — used by
    /// the non-prod tuning pager's Film page so the synopsis / trailer
    /// typography has real text to render. Never reached in a shipping run
    /// (only `ShowtimeTuningScreen` calls it), but not `#if DEBUG`-gated
    /// because that screen itself compiles in every configuration.
    static func seeded(_ details: [FilmDetails]) -> DetailsStore {
        let store = DetailsStore()
        store.byTitle = details.keyedByTitle()
        return store
    }
}
