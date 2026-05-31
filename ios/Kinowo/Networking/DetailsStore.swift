import Foundation

let kinowoDetailsURL = URL(string: "https://kinowo.fly.dev/api/details")!

/// Fetches `/api/details` and exposes a title → details lookup the
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

    private let url: URL
    private let session: URLSession
    private var lastReloadedAt: Date?

    private let staleAfter: TimeInterval = 60

    init(url: URL = kinowoDetailsURL, session: URLSession = .shared) {
        self.url = url
        self.session = session
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
            if let lm = DetailsCache.loadLastModified() {
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
            if let lm = http.value(forHTTPHeaderField: "Last-Modified") {
                Task.detached { DetailsCache.saveLastModified(lm) }
            }
            let copy = decoded
            Task.detached { DetailsCache.save(copy) }
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
}
