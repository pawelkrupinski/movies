import Foundation

@MainActor
final class RepertoireStore: ObservableObject {
    @Published var films: [Film] = []
    @Published var isLoading: Bool = false
    @Published var error: Error? = nil
    /// The city whose repertoire `films` currently holds, or nil before the
    /// first successful load. `films` isn't cleared on a city switch (no empty
    /// flash), so it briefly holds the PREVIOUS city's list mid-switch — a deep
    /// link's film lookup waits for this to equal its target slug rather than
    /// for `films` to merely be non-empty.
    @Published private(set) var loadedCitySlug: String?

    private let base: URL
    private var url: URL
    private var citySlug: String
    private let session: URLSession
    private var lastReloadedAt: Date?

    private let staleAfter: TimeInterval = 60

    /// `base` is the bare host (`https://kinowo.fly.dev`); the fetch URL is
    /// `…/{citySlug}/api/repertoire`. `citySlug` defaults to the fallback
    /// city so the existing default-init call sites (UI-test fixture, tuning
    /// screen) keep working; the app points it at the resolved city via
    /// `use(citySlug:)` once the first-launch gate lands.
    init(base: URL = kinowoBaseURL, citySlug: String = City.default.slug, session: URLSession = .shared) {
        self.base = base
        self.citySlug = citySlug
        self.url = City.apiURL(base: base, slug: citySlug, endpoint: "repertoire")
        self.session = session
    }

    /// Re-point at a different city: rebuild the URL, drop the freshness
    /// stamp so the next `reload`/`reloadIfStale` actually fetches, and
    /// kick a reload so the grid swaps to the new city's repertoire.
    func use(citySlug: String) {
        let next = City.apiURL(base: base, slug: citySlug, endpoint: "repertoire")
        guard next != url else { return }
        url = next
        self.citySlug = citySlug
        lastReloadedAt = nil
        Task { await reload() }
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
            loadedCitySlug = citySlug
            return
        }
        do {
            var request = URLRequest(url: url)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            if let lm = RepertoireCache.lastModified(forCity: citySlug) {
                request.setValue(lm, forHTTPHeaderField: "If-Modified-Since")
            }
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            if http.statusCode == 304 {
                // `films` already holds this city's data (the conditional header
                // is city-bound), so mark it loaded for the deep-link gate.
                self.loadedCitySlug = citySlug
                self.lastReloadedAt = now
                return
            }
            guard (200..<300).contains(http.statusCode) else {
                throw URLError(.badServerResponse)
            }
            let decoded = try JSONDecoder().decode([Film].self, from: data)
            self.films = decoded
            self.loadedCitySlug = citySlug
            self.lastReloadedAt = now
            let lm = http.value(forHTTPHeaderField: "Last-Modified")
            let filmsCopy = decoded
            let city = citySlug
            Task.detached { RepertoireCache.save(filmsCopy, city: city, lastModified: lm) }
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
                // Today AND tomorrow, so the carousel's neighbour day is
                // populated too: a swipe can commit to a day that actually has
                // cards, so the "did the next day's cards land on screen" path is
                // exercised (DaySwipeCardsVisibleUITests). A today-only fixture
                // left every neighbour empty.
                showings: [
                    fixtureDay(offsetDays: 0, label: "Dziś"),
                    fixtureDay(offsetDays: 1, label: "Jutro")
                ]
            )
        }
    }

    /// One fixture day, `offsetDays` from now, carrying a single late slot so it
    /// survives `pruneStaleShowings`.
    private static func fixtureDay(offsetDays: Int, label: String) -> DayShowings {
        DayShowings(
            date: DateFilter.iso(Date().addingTimeInterval(Double(offsetDays) * 86_400)),
            label: label,
            cinemas: [
                CinemaShowings(
                    cinema: "Kino", cinemaURL: nil,
                    showtimes: [Showtime(time: "23:59", format: "2D", room: nil, bookingURL: nil)]
                )
            ]
        )
    }
}
