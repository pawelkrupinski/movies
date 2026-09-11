import Foundation
// The Xcode app compiles this file into one flat module alongside every
// Model/Auth file, where no module named `KinowoCore`/`KinowoAuth` exists —
// `canImport` is false there, so this compiles to nothing and every symbol
// below resolves exactly as it does today. Only the standalone `KinowoNetworking`
// SPM target (see `Package.swift`) sees these as true, which is what lets
// `swift test` drive `RepertoireStore` directly instead of only through the
// Combine-free decoder line `LocalServerRepertoireTests` exercises.
#if canImport(KinowoCore)
@testable import KinowoCore
#endif
#if canImport(KinowoAuth)
@testable import KinowoAuth
#endif

@MainActor
final class RepertoireStore: ObservableObject {
    @Published var films: [Film] = []
    @Published var isLoading: Bool = false
    @Published var error: Error? = nil
    /// The city whose repertoire `films` currently holds, or nil before the
    /// first successful load OR while a switch (`use(citySlug:)`/`use(country:)`,
    /// via `resetForCitySwitch`) has dropped it. A deep link's film lookup
    /// waits for this to equal its target slug — nil is just another shade of
    /// "not yet" alongside the wrong slug, so clearing it mid-switch is safe.
    @Published private(set) var loadedCitySlug: String?
    /// The current city's cinema universe + area grouping (`/api/cinemas`).
    /// `.empty` (flat) until fetched; a split city (London) drives the
    /// multi-select area picker off `catalog.areas`, a flat city ignores it and
    /// keeps the single-select pill bar.
    @Published private(set) var catalog: CinemaCatalog = .empty

    private var base: URL
    private var url: URL
    private var citySlug: String
    /// The zone the on-foreground re-prune drops past showtimes on — the CITY's
    /// wall-clock (London on Europe/London, not Warsaw). Set from the country in
    /// `use(country:)`, which is the fallback, and overridden per city by
    /// `use(citySlug:timeZone:)`: a country has one zone to offer and the US
    /// needs six, so pruning a Knoxville showtime on the country's would drop it
    /// three hours early. Defaults to Warsaw for the Poland-default init.
    private var timeZone: TimeZone = .warsaw
    private let session: URLSession
    private var lastReloadedAt: Date?
    /// The city `catalog` was fetched for, so a static catalog isn't re-fetched
    /// on every stale-repertoire reload — only on an actual city/country switch.
    private var catalogCitySlug: String?

    private let staleAfter: TimeInterval = 60

    /// `base` is the bare host (`https://kinowo.net`); the fetch URL is
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

    /// Re-point at a different country's deployment: rebuild the base + URL,
    /// drop the freshness stamp, and reload so the grid swaps to the new
    /// country's repertoire. The city slug is preserved; when it isn't served by
    /// the new deployment the fetch simply comes back empty until the user picks
    /// a city the new country serves.
    func use(country: Country) {
        // Always adopt the country's zone (even if the URL is unchanged) so the
        // re-prune reasons in the right wall-clock.
        timeZone = country.timeZone
        let next = City.apiURL(base: country.baseURL, slug: citySlug, endpoint: "repertoire")
        guard next != url else { return }
        base = country.baseURL
        url = next
        lastReloadedAt = nil
        // The new deployment has its own cinema roster/areas — drop the stale one.
        catalog = .empty
        catalogCitySlug = nil
        resetForCitySwitch()
        Task { await reload() }
    }

    /// Re-point at a different city: rebuild the URL, drop the freshness
    /// stamp so the next `reload`/`reloadIfStale` actually fetches, and
    /// kick a reload so the grid swaps to the new city's repertoire.
    /// `timeZone` is the new city's own (`[City].zone(ofSlug:fallback:)`), which
    /// callers resolve against the catalog. It is adopted even when the URL is
    /// unchanged — re-selecting the same slug on a different country lands here —
    /// so the re-prune always reasons in the city actually being shown.
    func use(citySlug: String, timeZone: TimeZone? = nil) {
        if let timeZone { self.timeZone = timeZone }
        let next = City.apiURL(base: base, slug: citySlug, endpoint: "repertoire")
        guard next != url else { return }
        url = next
        self.citySlug = citySlug
        lastReloadedAt = nil
        // A new city has its own cinemas/areas — clear so the split panel doesn't
        // briefly show the previous city's areas mid-switch.
        catalog = .empty
        catalogCitySlug = nil
        resetForCitySwitch()
        Task { await reload() }
    }

    /// Drop the OUTGOING city/deployment's repertoire so `content`'s
    /// `isLoading && films.isEmpty` branch shows the loading state instead of
    /// the previous city's films while the new one's fetch is in flight — a
    /// switch used to leave `films` populated across it (documented on
    /// `loadedCitySlug` below as "no empty flash"), which read as the WRONG
    /// city's grid hanging around for a beat rather than as a clean load.
    /// Immediately re-checks the disk cache for the new city right after, so a
    /// warm cache still shows instantly instead of a spinner; only a
    /// genuinely cold one waits on the network `reload()` the caller kicks off
    /// next.
    private func resetForCitySwitch() {
        films = []
        loadedCitySlug = nil
        loadCachedData()
    }

    func loadCachedData(now: Date = Date()) {
        // Warm-start UI-test hook: deliver the fixture synchronously so the grid
        // mounts at first paint, exactly like a warm disk cache.
        if RepertoireStore.uiTestFixtureEnabled {
            films = RepertoireStore.uiTestFixture
            return
        }
        if films.isEmpty, let cached = RepertoireCache.load(deployment: base, city: citySlug) {
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
        // The cinema catalog (universe + areas) is static per city; fetch it once
        // per city alongside the repertoire, independent of the listing's success.
        await fetchCatalogIfNeeded()
        do {
            var request = URLRequest(url: url)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            request.cachePolicy = .reloadIgnoringLocalCacheData
            if let lm = RepertoireCache.lastModified(deployment: base, city: citySlug) {
                request.setValue(lm, forHTTPHeaderField: "If-Modified-Since")
            }
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse else {
                throw URLError(.badServerResponse)
            }
            if http.statusCode == 304 {
                // 304 says the CACHED body is current — which is only the same
                // thing as "`films` is current" if the cache was actually read
                // into it. On a cold launch the disk read happens before the
                // deep link re-points the store, so it can be skipped for the
                // wrong city and leave `films` empty; taking 304 at face value
                // then strands an empty grid on a city that has a full listing.
                // Hydrate from the entry the conditional header spoke for.
                //
                // The disk entry holds the RAW payload from whenever it was
                // last fetched, so it can carry screenings that have since
                // crossed the 30-minute cutoff — re-prune it against the
                // caller's own clock rather than trusting its age.
                if let cached = RepertoireCache.bodyForNotModified(
                    callerIsEmpty: films.isEmpty, deployment: base, city: citySlug) {
                    self.films = cached.prunedPastShowings(now: now, zone: timeZone)
                }
                self.loadedCitySlug = citySlug
                self.lastReloadedAt = now
                return
            }
            guard (200..<300).contains(http.statusCode) else {
                throw URLError(.badServerResponse)
            }
            let decoded = try JSONDecoder().decode([Film].self, from: data)
            // Re-prune locally rather than trusting the server's own cutoff:
            // a foreground reload runs `pruneStaleShowings()` first (see
            // ContentView's scenePhase handler), and a response generated
            // even a little earlier than it's applied here would otherwise
            // silently undo that prune with a payload the server considered
            // fresh at request time but that has since aged past 30 minutes.
            self.films = decoded.prunedPastShowings(now: now, zone: timeZone)
            self.loadedCitySlug = citySlug
            self.lastReloadedAt = now
            let lm = http.value(forHTTPHeaderField: "Last-Modified")
            let filmsCopy = decoded
            let city = citySlug
            let deployment = base
            Task.detached { RepertoireCache.save(filmsCopy, deployment: deployment, city: city, lastModified: lm) }
        } catch {
            self.error = error
        }
    }

    /// Fetch `/api/cinemas` once per city. Best-effort: on any failure the prior
    /// `catalog` (or `.empty` = flat) stands, so the flat pill-bar path still
    /// works even if this call fails.
    private func fetchCatalogIfNeeded() async {
        guard catalogCitySlug != citySlug else { return }
        let catalogURL = City.apiURL(base: base, slug: citySlug, endpoint: "cinemas")
        do {
            var request = URLRequest(url: catalogURL)
            request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
            let (data, response) = try await session.data(for: request)
            guard let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else { return }
            self.catalog = try JSONDecoder().decode(CinemaCatalog.self, from: data)
            self.catalogCitySlug = citySlug
        } catch {
            // Leave the current catalog; the flat pill-bar path is unaffected.
        }
    }

    func reloadIfStale(now: Date = Date()) async {
        if let last = lastReloadedAt, now.timeIntervalSince(last) < staleAfter {
            return
        }
        await reload(now: now)
    }

    func pruneStaleShowings(now: Date = Date()) {
        let pruned = films.prunedPastShowings(now: now, zone: timeZone)
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
        let today = DateFilter.iso(now, zone: timeZone)
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

    /// A poster URL that can only ever resolve from `PosterStore`'s on-disk
    /// cache: `.invalid` is the reserved TLD that never resolves, so any
    /// screen that re-downloads instead of reading the cache shows "Brak
    /// plakatu". `KINOWO_UITEST_SEED_POSTER=1` primes the cache for it (see
    /// `seedUITestPoster`) and hands it to the fixture films.
    static let uiTestSeededPosterURL = URL(string: "https://poster.invalid/fixture-poster.png")!

    /// Off by default: giving every fixture card a poster changes the grid's
    /// layout, which the spacing/inset suites measure. Only the poster-cache
    /// suite turns it on.
    static var uiTestPosterSeedEnabled: Bool {
        ProcessInfo.processInfo.environment["KINOWO_UITEST_SEED_POSTER"] == "1"
    }

    /// Deterministic stand-in repertoire served when the fixture hook is on —
    /// enough cards to fill the grid well past the first row so the top
    /// content inset actually matters. No poster URLs unless
    /// `KINOWO_UITEST_SEED_POSTER=1`: the cards lay out from their fixed
    /// aspect ratio, keeping the test fully offline.
    static var uiTestFixture: [Film] {
        let poster = uiTestPosterSeedEnabled ? uiTestSeededPosterURL : nil
        return (1...12).map { n in
            Film(
                title: "Film \(n)",
                posterURL: poster,
                fallbackPosterURLs: [],
                runtimeMinutes: 120,
                releaseYear: 2026,
                genres: ["Dramat"],
                // A certificate so the age-rating badge is exercised on the
                // grid (CardsVisibleUITests). Absence is covered by the
                // AgeRatingDecodeTests unit path.
                ageRating: "15",
                ratings: .empty,
                countries: ["Polska"],
                // Non-empty so the detail screen's director / cast meta blocks
                // actually render — `metaBlock` omits a block whose value is
                // empty, and LocalizationUITests asserts on their captions.
                directors: ["Jan Kowalski"],
                cast: ["Anna Nowak", "Piotr Wiśniewski"],
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
            cinemas: fixtureCinemas
        )
    }

    /// The fixture day's cinemas. Normally one URL-less cinema, which keeps
    /// every card the same height for the layout suites.
    ///
    /// `KINOWO_UITEST_SHARED_CINEMA_URL=1` swaps in the shape a real listing
    /// has and this one didn't: two venues of one chain behind a SINGLE
    /// per-film URL, plus a cinema with a URL of its own.
    /// CinemaLinkRowUITests drives the detail screen's link row with it.
    private static var fixtureCinemas: [CinemaShowings] {
        let slot = [Showtime(time: "23:59", format: "2D", room: nil, bookingURL: nil)]
        guard ProcessInfo.processInfo.environment["KINOWO_UITEST_SHARED_CINEMA_URL"] == "1" else {
            return [CinemaShowings(cinema: "Kino", cinemaURL: nil, showtimes: slot)]
        }
        let chain = URL(string: "https://www.multikino.example/filmy/fixture")
        return [
            CinemaShowings(cinema: "Multikino Alfa", cinemaURL: chain, showtimes: slot),
            CinemaShowings(cinema: "Multikino Beta", cinemaURL: chain, showtimes: slot),
            CinemaShowings(cinema: "Kino Solo",
                           cinemaURL: URL(string: "https://kinosolo.example/film"),
                           showtimes: slot),
        ]
    }
}
