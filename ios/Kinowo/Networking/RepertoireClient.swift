import Foundation

/// Production URL. Top-level so it isn't main-actor-isolated and can be used
/// as a default argument from a nonisolated init context.
let kinowoProductionURL = URL(string: "https://kinowo.fly.dev/")!

/// Fetches the kinowo.fly.dev `/` page and hands the body to `HTMLParser`.
/// Keeps the parsed films in `@Published` state so views auto-refresh.
@MainActor
final class RepertoireStore: ObservableObject {
    @Published var films: [Film] = []
    @Published var isLoading: Bool = false
    @Published var error: Error? = nil

    private let url: URL
    private let session: URLSession

    init(url: URL = kinowoProductionURL, session: URLSession = .shared) {
        self.url = url
        self.session = session
    }

    func reload() async {
        isLoading = true
        error = nil
        defer { isLoading = false }
        do {
            var request = URLRequest(url: url)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            let (data, response) = try await session.data(for: request)
            if let http = response as? HTTPURLResponse,
               !(200..<300).contains(http.statusCode) {
                throw URLError(.badServerResponse)
            }
            guard let html = String(data: data, encoding: .utf8) else {
                throw URLError(.cannotDecodeContentData)
            }
            self.films = HTMLParser.parse(html: html)
        } catch {
            self.error = error
        }
    }

    /// Drop showtimes that have slipped into the past since the cached
    /// payload was loaded (and re-sort cinemas inside each day by the
    /// new earliest slot). The server already does this at fetch time,
    /// so a fresh `reload()` is a no-op here; the call earns its keep
    /// when the app comes back from background hours after the last
    /// fetch — the web's same `now - 30min` rule then runs locally,
    /// without a round-trip.
    func pruneStaleShowings(now: Date = Date()) {
        let pruned = films.prunedPastShowings(now: now)
        if pruned != films { films = pruned }
    }
}
