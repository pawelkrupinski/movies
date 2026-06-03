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
        // Warm-start UI-test hook: deliver the fixture synchronously so the grid
        // mounts at first paint, exactly like a warm disk cache.
        if RepertoireStore.uiTestFixtureEnabled {
            films = RepertoireStore.uiTestFixture
            return
        }
        if films.isEmpty, let cached = RepertoireCache.load() {
            films = cached
        }
    }

    func reload(now: Date = Date()) async {
        isLoading = true
        error = nil
        defer { isLoading = false }
        // UI tests run against the in-memory fixture, never the network.
        if RepertoireStore.uiTestFixtureEnabled {
            lastReloadedAt = now
            return
        }
        do {
            var request = URLRequest(url: url)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            if let lm = RepertoireCache.loadLastModified() {
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
            let decoded = try JSONDecoder().decode([Film].self, from: data)
            self.films = decoded
            self.lastReloadedAt = now
            if let lm = http.value(forHTTPHeaderField: "Last-Modified") {
                Task.detached { RepertoireCache.saveLastModified(lm) }
            }
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

    /// Once a day, after the repertoire has loaded, drop cached posters for
    /// films that no longer have any future screening. `films` is already
    /// pruned (server-side and by `prunedPastShowings`) to films with a
    /// future showing, so its poster + fallback URLs are exactly the set
    /// worth keeping; `PosterStore.reconcile` deletes every other cached
    /// poster. Guarded on a non-empty list so a failed cold load (no
    /// network, no disk cache) can't wipe the whole poster cache.
    func reconcilePostersIfNeeded(now: Date = Date()) async {
        guard !films.isEmpty else { return }
        let today = DateFilter.iso(now)
        let defaults = UserDefaults.standard
        guard defaults.string(forKey: Self.posterPurgeDayKey) != today else { return }
        let keepURLs = films.flatMap { film -> [URL] in
            (film.posterURL.map { [$0] } ?? []) + film.fallbackPosterURLs
        }
        await PosterStore.shared.reconcile(keepURLs: keepURLs)
        defaults.set(today, forKey: Self.posterPurgeDayKey)
    }

    private static let posterPurgeDayKey = "posterPurgeLastDay"
}

// MARK: - UI-test fixture hook

extension RepertoireStore {
    /// When `KINOWO_UITEST_FIXTURE=1`, the store serves a deterministic
    /// in-memory repertoire instead of the network — the grid mounts at first
    /// paint like a warm cache, independent of real network timing (the live
    /// repertoire is empty late at night). `false` in every normal run, so the
    /// production code path is untouched.
    static var uiTestFixtureEnabled: Bool {
        ProcessInfo.processInfo.environment["KINOWO_UITEST_FIXTURE"] == "1"
    }

    /// Deterministic stand-in repertoire served when the fixture hook is on —
    /// enough cards to fill the grid well past the first row so the top
    /// content inset actually matters. No poster URLs: the cards lay out from
    /// their fixed aspect ratio, keeping the test fully offline.
    static var uiTestFixture: [Film] {
        (1...12).map { n in
            Film(
                title: "Film \(n)",
                posterURL: nil,
                fallbackPosterURLs: [],
                runtimeMinutes: 120,
                releaseYear: 2026,
                genres: ["Dramat"],
                ratings: .empty,
                countries: ["Polska"],
                directors: [],
                cast: [],
                showings: [
                    // Dated today so the default "Dziś" filter shows it at
                    // launch, with the day's last slot so `pruneStaleShowings`
                    // keeps it.
                    DayShowings(
                        date: DateFilter.iso(Date()),
                        label: "Dziś",
                        cinemas: [
                            CinemaShowings(
                                cinema: "Kino", cinemaURL: nil,
                                showtimes: [Showtime(time: "23:59", format: "2D", room: nil, bookingURL: nil)]
                            )
                        ]
                    )
                ]
            )
        }
    }
}
