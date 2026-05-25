import Foundation

let kinowoProductionURL = URL(string: "https://kinowo.fly.dev/api/repertoire")!

@MainActor
final class RepertoireStore: ObservableObject {
    @Published var films: [Film] = []
    @Published var isLoading: Bool = false
    @Published var error: Error? = nil

    private let url: URL
    private let session: URLSession
    private var lastReloadedAt: Date?

    private let staleAfter: TimeInterval = 60

    init(url: URL = kinowoProductionURL, session: URLSession = .shared) {
        self.url = url
        self.session = session
    }

    func loadCachedData(now: Date = Date()) {
        if films.isEmpty, let cached = RepertoireCache.load() {
            films = cached
        }
    }

    func reload(now: Date = Date()) async {
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
            let decoded = try JSONDecoder().decode([Film].self, from: data)
            self.films = decoded
            self.lastReloadedAt = now
            let filmsCopy = decoded
            Task.detached { RepertoireCache.save(filmsCopy) }
        } catch {
            self.error = error
        }
    }

    func reloadIfStale(now: Date = Date()) async {
        if let last = lastReloadedAt, now.timeIntervalSince(last) < staleAfter {
            return
        }
        await reload(now: now)
    }

    func pruneStaleShowings(now: Date = Date()) {
        let pruned = films.prunedPastShowings(now: now)
        if pruned != films { films = pruned }
    }
}
