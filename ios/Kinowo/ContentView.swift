import SwiftUI

struct ContentView: View {
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var details: DetailsStore
    @EnvironmentObject var prefs: UserPreferences
    @Environment(\.scenePhase) private var scenePhase

    @State private var dateFilter: DateFilter = .today
    @State private var formatFilter: FormatFilter = .empty
    @State private var excludedCountries: Set<String> = []
    @State private var excludedGenres: Set<String> = []
    @State private var excludedDirectors: Set<String> = []
    @State private var excludedCast: Set<String> = []
    @State private var search: String = ""
    @State private var sortOption: SortOption = .earliest
    /// Active tab — swipe-left/right on the TabView flips between
    /// `.films` (`/`) and `.cinemas` (`/kina`). Each writes a brief
    /// label overlay so the user knows which screen they landed on
    /// when there's no other visual cue (no tab bar, just swipe).
    @State private var tab: Tab = .films
    /// Kina-tab cinema pin. Equivalent to the web's `_kinaPinned` —
    /// single-cinema filter that ignores Filtry's persistent
    /// `disabledCinemas`. `nil` = show all cinemas.
    @State private var pinnedCinema: String? = nil
    @State private var showFilters: Bool = false
    @FocusState private var searchFocused: Bool

    /// Live viewport width, tracked by a backing GeometryReader so the
    /// search placement re-evaluates on rotation / iPad split-view resize.
    @State private var viewportWidth: CGFloat = UIScreen.main.bounds.width
    /// Wide screens host search inline on the top bar; narrow ones keep it
    /// as the floating bottom pill. See `TopBarLayout`.
    private var searchInline: Bool { TopBarLayout.searchInline(width: viewportWidth) }

    @State private var tabLabel: String? = nil
    @State private var tabLabelTask: Task<Void, Never>?

    /// Once-a-day swipe onboarding hint (see `SwipeHint`). Evaluated once per
    /// appearance, the moment the first repertoire load lands.
    @State private var showSwipeHint = false
    @State private var swipeHintEvaluated = false
    @State private var swipeHintTask: Task<Void, Never>?

    /// "You're nearer another city — switch?" prompt. Populated by the
    /// authorized-only location check on open / foreground; presenting the
    /// alert when non-nil. The resolver is held as state so it survives
    /// across the async fix.
    @State private var citySwitchSuggestion: City.CitySwitchSuggestion?
    @StateObject private var locationResolver = LocationCityResolver()

    enum Tab: Hashable { case films, cinemas }

    private func tabLabel(for tab: Tab) -> String {
        switch tab {
        case .films:      return "Filmy"
        case .cinemas:    return "Kina"
        }
    }

    var body: some View {
        NavigationStack {
            // The bar and the paged grids are a VStack, NOT an overlay /
            // safeAreaInset: the bar must sit ABOVE the TabView in the layout,
            // so the paged scroll view's frame — and its pan gesture — starts
            // below the bar. Overlaying the bar (or insetting it) left the
            // TabView spanning the full height, so its scroll/page gesture
            // reached up under the pill row and stole the day-pill taps (a
            // still tap fired, a tap with the slightest movement, or one
            // arriving while the grid coasted, got swallowed by the scroll).
            VStack(spacing: 0) {
                // No native nav bar: it clips a horizontal ScrollView to a few
                // dozen points of width, crushing the pill row. We render the
                // bar ourselves so the HStack gets the full screen width. The
                // opaque backing bleeds up under the status bar so it (and the
                // bar) read as solid app-background.
                TopBar(
                    dateFilter: $dateFilter,
                    search: $search,
                    searchFocused: $searchFocused,
                    searchInline: searchInline,
                    filtersActive: filtersActive,
                    onTapFilters: { showFilters = true }
                )
                .background(Color(.systemBackground).ignoresSafeArea(edges: .top))

                content
                    // Gradient FADE (a scrim, not a frost): the grid scrolling
                    // up gently fades to the app-background over ~56pt as it
                    // reaches the bar, so rows dissolve into the bar rather than
                    // meeting a hard clip edge. Opaque at the bar's bottom edge
                    // (top of the content), clear 56pt below.
                    .overlay(alignment: .top) {
                        LinearGradient(
                            colors: [Color(.systemBackground), .clear],
                            startPoint: .top, endPoint: .bottom
                        )
                        .frame(height: 56)
                        .allowsHitTesting(false)
                    }
            }
                // Tap-to-dismiss: a transparent backdrop sits behind the
                // grid content and only intercepts taps while the search
                // is focused. Hit-testing falls through to film cards /
                // pills / toolbar buttons (they paint above this layer),
                // so the dismiss only fires on taps that land in empty
                // grid space — exactly the "click outside" UX the user
                // asked for.
                .background(
                    Color.clear
                        .contentShape(Rectangle())
                        .onTapGesture { searchFocused = false }
                        .allowsHitTesting(searchFocused)
                )
                .ignoresSafeArea(edges: [.bottom, .horizontal])
                .toolbar(.hidden, for: .navigationBar)
                // No frost and no separate strip: the under-bar treatment is the
                // gradient fade on the grid's top edge (see `content`). A true
                // Settings-style *blur* (content gently blurred, not just
                // dimmed) would need a variable-blur Metal shader (Variablur),
                // which renders black in the Simulator and so can't be verified
                // here without a device.
                // Narrow screens float search at the bottom; wide screens
                // host it inline on the top bar instead (see TopBar).
                .overlay(alignment: .bottom) {
                    if !searchInline {
                        SearchBar(search: $search, focused: $searchFocused)
                    }
                }
                // Track the live width so `searchInline` follows rotation
                // and resize. A clear backing layer — no layout effect.
                .background(
                    GeometryReader { geo in
                        Color.clear
                            .onAppear { viewportWidth = geo.size.width }
                            .onChange(of: geo.size.width) { viewportWidth = $0 }
                    }
                )
                .sheet(isPresented: $showFilters) {
                    FiltersSheet(
                        sortOption: $sortOption,
                        formatFilter: $formatFilter,
                        excludedCountries: $excludedCountries,
                        excludedGenres: $excludedGenres,
                        excludedDirectors: $excludedDirectors,
                        excludedCast: $excludedCast,
                        prefs: prefs,
                        allCinemas: allCinemas,
                        allCountries: allCountries,
                        allGenres: allGenres,
                        allDirectors: allDirectors,
                        allCast: allCast,
                        showCinemaSection: tab == .films
                    )
                }
        }
        // Overlay on the NavigationStack so the label aligns to the
        // device's screen centre, not the safe-area-inset content area.
        // The hint takes precedence over the momentary tab label so the two
        // pills never stack on top of each other.
        .overlay {
            if showSwipeHint {
                SwipeHintOverlay()
                    .allowsHitTesting(false)
                    .transition(.opacity.combined(with: .scale(scale: 0.9)))
            } else if let label = tabLabel {
                TabLabelOverlay(text: label)
                    .allowsHitTesting(false)
                    .transition(.opacity.combined(with: .scale(scale: 0.9)))
            }
        }
        // "Jesteś bliżej miasta X" — offer to switch the repertoire to the
        // nearer supported city. `citySwitchSuggestion` is set (and the
        // prompt-key remembered) only by the authorized-only location check,
        // so it never fires without an existing location grant.
        .alert(
            citySwitchSuggestion.map { "Jesteś bliżej miasta \($0.target.name)" } ?? "",
            isPresented: Binding(
                get: { citySwitchSuggestion != nil },
                set: { if !$0 { citySwitchSuggestion = nil } }
            ),
            presenting: citySwitchSuggestion
        ) { suggestion in
            Button("Przełącz") {
                prefs.setCity(suggestion.target.slug)
                store.use(citySlug: suggestion.target.slug)
                details.use(citySlug: suggestion.target.slug)
            }
            Button("Nie teraz", role: .cancel) {}
        } message: { suggestion in
            Text("Przełączyć repertuar na \(suggestion.target.name)?")
        }
        .task {
            store.loadCachedData()
            store.pruneStaleShowings()
            details.loadCachedData()
            // A warm cache fills `store.films` synchronously above, before any
            // `films.isEmpty` change can fire — so evaluate the hint here too.
            maybeShowSwipeHint()
            // Fetch listing + details concurrently. The grid renders the
            // moment `store.reload()` lands (synopsis/trailers are
            // non-essential), so details never block the first paint —
            // they populate the detail screen whenever they arrive.
            async let repertoire: Void = store.reload()
            async let detailsLoad: Void = details.reload()
            _ = await (repertoire, detailsLoad)
            maybeShowSwipeHint()
            // Once-a-day poster-cache purge: now that the fresh repertoire
            // has landed, drop cached posters for films that have left it.
            await store.reconcilePostersIfNeeded()
        }
        // Cold start: the first non-empty `store.films` is the first
        // repertoire load completing.
        .onChange(of: store.films.isEmpty) { isEmpty in
            if !isEmpty { maybeShowSwipeHint() }
        }
        .onAppear {
            // Briefly name the starting tab so the user sees the same
            // label-on-arrival affordance the swipe-driven changes get.
            // Idempotent — running on every appear is fine; the task
            // cancels the previous fade-out cleanly.
            showTabLabel(tabLabel(for: tab))
            Task { await maybeSuggestCitySwitch() }
        }
        .onChange(of: scenePhase) { phase in
            // App came back from background — drop screenings that
            // slipped into the past while we were idle and re-sort
            // cinemas by their earliest remaining slot of the day.
            // Mirrors the server's `now - 30min` cutoff from
            // `MovieController.toSchedules` so cached and freshly
            // fetched payloads age identically.
            //
            // `reloadIfStale` then fires a real network fetch when
            // the cached payload is more than 60s old, picking up
            // any server-side changes (new screenings, swapped
            // poster URLs from a cinema rotating their CDN, …)
            // without the user having to pull-to-refresh. The local
            // prune still runs first so any time-only update lands
            // before the network roundtrip completes.
            if phase == .active {
                store.pruneStaleShowings()
                Task { await store.reloadIfStale() }
                Task { await details.reloadIfStale() }
                // Re-check on every foreground so travelling between cities
                // mid-session offers the switch as soon as the app returns.
                Task { await maybeSuggestCitySwitch() }
            }
        }
    }

    /// Authorized-only "you're nearer another city" check. No-op until a city
    /// is chosen; never prompts for location permission (`resolveIfAuthorized`
    /// returns `nil` unless already granted). Remembers the prompted pair the
    /// moment it decides to show, so the alert fires at most once per
    /// `chosen→nearest` pair whether the user accepts or declines.
    private func maybeSuggestCitySwitch() async {
        guard let chosen = prefs.selectedCity,
              citySwitchSuggestion == nil,
              let fix = await locationResolver.resolveIfAuthorized(),
              let suggestion = City.switchSuggestion(
                chosenSlug: chosen,
                lat: fix.lat, lon: fix.lon,
                lastPromptKey: prefs.citySwitchPromptKey
              )
        else { return }
        prefs.setCitySwitchPromptKey(suggestion.key)
        citySwitchSuggestion = suggestion
    }

    @ViewBuilder
    private var content: some View {
        if store.isLoading && store.films.isEmpty {
            loadingState
        } else if let error = store.error, store.films.isEmpty {
            errorState(error)
        } else {
            TabView(selection: $tab) {
                FilmGridView(films: filmsForFilmsTab)
                    .refreshable { await store.reload() }
                    .tag(Tab.films)
                cinemasPage
                    .tag(Tab.cinemas)
            }
            // `.page(indexDisplayMode: .never)` gives horizontal swipe
            // between pages without the dot indicator at the bottom —
            // the floating Filmy / Kina label is the only "where am I"
            // affordance.
            .tabViewStyle(.page(indexDisplayMode: .never))
            // Ignore only bottom/horizontal — NOT the top. The TabView sits
            // below the bar in ContentView's VStack, so its paged scroll view
            // (and pan gesture) starts at the bar's bottom edge, never reaching
            // up under the pill row to steal the day-pill taps. Ignoring `.top`
            // here would let the paged scroll view span the full height again,
            // putting its gesture back under the pills. Bottom/horizontal are
            // ignored so pages run edge-to-edge and under the floating search
            // pill; the grids pin their own content inset (see FilmGridView) so
            // the first row still rests a clear gap below the bar.
            .ignoresSafeArea(edges: [.bottom, .horizontal])
            // Resolves NavigationLink(value: Film) from both grids to
            // the per-film detail screen, the iOS counterpart of
            // /film?title=… on the web.
            .navigationDestination(for: Film.self) { film in
                FilmDetailView(film: film)
            }
            .onChange(of: tab) { new in
                showTabLabel(tabLabel(for: new))
                // First-ever swipe retires the onboarding hint for good.
                // `markSwiped()` is idempotent, so running it on every swipe
                // is harmless.
                swipeHintTask?.cancel()
                withAnimation(.easeInOut(duration: 0.2)) { showSwipeHint = false }
                prefs.markSwiped()
            }
        }
    }

    @ViewBuilder
    private var cinemasPage: some View {
        CinemaSectionedGridView(
            sections: filmsForCinemasTab.groupedByCinema(),
            // A pinned cinema is already named by its pill, so drop the
            // now-redundant per-section header; "Wszystkie" keeps them.
            showSectionHeaders: pinnedCinema == nil,
            header: {
                CinemaPillsRow(
                    allCinemas: allCinemas,
                    pinnedCinema: $pinnedCinema
                )
            }
        )
        .refreshable { await store.reload() }
    }

    private var loadingState: some View {
        VStack(spacing: 12) {
            ProgressView()
            Text("Ładowanie repertuaru…").font(.callout).foregroundStyle(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func errorState(_ error: Error) -> some View {
        VStack(spacing: 12) {
            Image(systemName: "exclamationmark.triangle").font(.largeTitle).foregroundStyle(.orange)
            Text("Nie udało się pobrać repertuaru.")
            Text(error.localizedDescription)
                .font(.caption).foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal)
            Button("Spróbuj ponownie") {
                Task { await store.reload() }
            }
            .buttonStyle(.borderedProminent)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    // Sorted, de-duplicated cinema names that appear at least once
    // somewhere in `store.films`. Drives the Kina list in FiltersSheet,
    // the "Wszystkie kina" master toggle, and the Kina-tab pill row.
    private var allCinemas: [String] {
        var seen = Set<String>()
        var out: [String] = []
        for film in store.films {
            for day in film.showings {
                for c in day.cinemas where seen.insert(c.cinema).inserted {
                    out.append(c.cinema)
                }
            }
        }
        return out.sorted { CinemaSection.pillName(for: $0) < CinemaSection.pillName(for: $1) }
    }

    /// Count how often each value (country / genre / director / actor)
    /// appears across the loaded films, sorted by descending frequency then
    /// name — the shape every Filtry name-list expects. Shared by the four
    /// `all*` accessors so a tweak to the ordering rule lands in one place.
    private func nameCounts(_ values: (Film) -> [String]) -> [(name: String, count: Int)] {
        var counts: [String: Int] = [:]
        for film in store.films {
            for v in values(film) {
                counts[v, default: 0] += 1
            }
        }
        return counts.map { (name: $0.key, count: $0.value) }
            .sorted {
                if $0.count != $1.count { return $0.count > $1.count }
                return $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending
            }
    }

    private var allCountries: [(name: String, count: Int)] { nameCounts { $0.countries } }
    private var allGenres:    [(name: String, count: Int)] { nameCounts { $0.genres } }
    private var allDirectors: [(name: String, count: Int)] { nameCounts { $0.directors } }
    private var allCast:      [(name: String, count: Int)] { nameCounts { $0.cast } }

    private var filtersActive: Bool {
        !formatFilter.isEmpty
            || !prefs.disabledCinemas.isEmpty
            || !prefs.hiddenFilms.isEmpty
            || !excludedCountries.isEmpty
            || !excludedGenres.isEmpty
            || !excludedDirectors.isEmpty
            || !excludedCast.isEmpty
    }

    private var filmsForFilmsTab: [Film] {
        store.films.filteredFor(
            date: dateFilter,
            format: formatFilter,
            query: search,
            hidden: prefs.hiddenFilms,
            disabledCinemas: prefs.disabledCinemas,
            excludedCountries: excludedCountries,
            excludedGenres: excludedGenres,
            excludedDirectors: excludedDirectors,
            excludedCast: excludedCast
        )
        .sorted(by: sortOption)
    }

    private var filmsForCinemasTab: [Film] {
        // Web's `/kina` ignores `disabledCinemas` localStorage — the
        // pill row is the single source of cinema-truth. Pinning one
        // cinema is equivalent to disabling every other; no pin
        // = empty disabled set = show everything.
        let disabled: Set<String> = {
            guard let pin = pinnedCinema else { return [] }
            return Set(allCinemas).subtracting([pin])
        }()
        return store.films.filteredFor(
            date: dateFilter,
            format: formatFilter,
            query: search,
            hidden: prefs.hiddenFilms,
            disabledCinemas: disabled,
            excludedCountries: excludedCountries,
            excludedGenres: excludedGenres,
            excludedDirectors: excludedDirectors,
            excludedCast: excludedCast
        )
        .sorted(by: sortOption)
    }

    /// Flash the given label in the middle of the screen for ~0.7 s,
    /// then fade it out over 0.2 s. Cancels any previous fade-out task
    /// so back-to-back swipes (Filmy → Kina → Filmy) don't leave the
    /// label stuck.
    private func showTabLabel(_ text: String) {
        tabLabelTask?.cancel()
        withAnimation(.easeInOut(duration: 0.1)) { tabLabel = text }
        tabLabelTask = Task { @MainActor in
            try? await Task.sleep(nanoseconds: 700_000_000)
            if Task.isCancelled { return }
            withAnimation(.easeInOut(duration: 0.2)) { tabLabel = nil }
        }
    }

    /// Evaluate (at most once per appearance) whether to flash the swipe hint,
    /// and if so show it for ~2.5 s. No-op until the first repertoire load has
    /// landed; the date gate + first-swipe rule live in `SwipeHint`.
    private func maybeShowSwipeHint() {
        guard !swipeHintEvaluated, !store.films.isEmpty else { return }
        swipeHintEvaluated = true
        let today = SwipeHint.dayKey(Date())
        guard SwipeHint.shouldShow(
            hasSwiped: prefs.hasSwipedScreens,
            lastShownDate: prefs.swipeHintShownDate,
            today: today
        ) else { return }
        prefs.markSwipeHintShown(today)
        withAnimation(.easeInOut(duration: 0.2)) { showSwipeHint = true }
        swipeHintTask?.cancel()
        swipeHintTask = Task { @MainActor in
            try? await Task.sleep(nanoseconds: 2_500_000_000)
            if Task.isCancelled { return }
            withAnimation(.easeInOut(duration: 0.3)) { showSwipeHint = false }
        }
    }
}
