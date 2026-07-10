import SwiftUI

struct ContentView: View {
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var details: DetailsStore
    @EnvironmentObject var prefs: UserPreferences
    @EnvironmentObject var authService: AuthService
    @EnvironmentObject var deepLink: DeepLinkCoordinator
    @Environment(\.scenePhase) private var scenePhase
    /// iPhone portrait is `.regular` height, landscape `.compact` — so this
    /// flips on every portrait⇄landscape rotation. It's resolved from the
    /// UIKit trait collection, NOT from SwiftUI's layout pass, so it stays
    /// correct even when the layout cache goes stale after a rotation. Keying
    /// the root view's `.id` on it forces a full re-layout against the real
    /// window size — without it, a landscape⇄portrait flip can leave the whole
    /// hierarchy (TopBar, grids, every GeometryReader) stuck reporting the old
    /// orientation's width, so the grid overflows the portrait screen.
    @Environment(\.verticalSizeClass) private var vSizeClass

    /// The film-detail push stack. Held as ContentView state — NOT inside the
    /// NavigationStack — so it survives the `.id(vSizeClass)` rebuild on every
    /// rotation. `.id` re-identifies (and tears down) only the NavigationStack
    /// subtree, leaving ContentView's own state intact; the rebuilt stack then
    /// restores its pushes from this binding. With a link-driven stack the
    /// push state lived *inside* the recreated subtree, so a rotation while on
    /// a film detail popped the user back to the grid (guarded by
    /// RotationColumnsUITests.testStaysOnFilmDetailAfterRotation).
    @State private var navPath: [Film] = []

    @State private var dateFilter: DateFilter = .today
    @State private var formatFilter: FormatFilter = .empty
    @State private var excludedCountries: Set<String> = []
    @State private var excludedGenres: Set<String> = []
    @State private var excludedDirectors: Set<String> = []
    @State private var excludedCast: Set<String> = []
    @State private var search: String = ""
    @State private var sortOption: SortOption = .earliest
    @State private var showFilters: Bool = false
    @FocusState private var searchFocused: Bool

    /// A deep link whose film push / multi-value filters couldn't be applied yet
    /// because the repertoire (hence the value universe and the `Film` to push)
    /// hadn't loaded. Re-applied the moment the first repertoire load lands.
    @State private var pendingDeepLink: DeepLink?

    /// Live viewport width, tracked by a backing GeometryReader so the
    /// search placement re-evaluates on rotation / iPad split-view resize.
    @State private var viewportWidth: CGFloat = UIScreen.main.bounds.width
    /// Wide screens host search inline on the top bar; narrow ones keep it
    /// as the floating bottom pill. See `TopBarLayout`.
    private var searchInline: Bool { TopBarLayout.searchInline(width: viewportWidth) }

    /// Brief day-name label flashed mid-screen on each swipe-driven day change
    /// (and on launch), so the user gets feedback on which day they landed on.
    @State private var dayLabel: String? = nil
    @State private var dayLabelTask: Task<Void, Never>?

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

    /// Run after the carousel commits a day change (it has already advanced
    /// `dateFilter`): flash the new day's name and retire the onboarding hint.
    private func didChangeDay() {
        showDayLabel(dateFilter.label)
        // First-ever swipe retires the onboarding hint for good. `markSwiped()`
        // is idempotent, so running it on every swipe is harmless.
        swipeHintTask?.cancel()
        withAnimation(.easeInOut(duration: 0.2)) { showSwipeHint = false }
        prefs.markSwiped()
    }

    var body: some View {
        NavigationStack(path: $navPath) {
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

                // Slim expand-handle + horizontally-scrolling cinema pills,
                // directly under the top bar. Hidden until the repertoire (hence
                // the cinema universe) has loaded, so it never shows an empty
                // handle over the loading / error state.
                if !allCinemas.isEmpty {
                    CinemaPillBar(
                        cinemas: allCinemas,
                        selectedCinema: prefs.selectedCinema,
                        onSelect: { prefs.setSelectedCinema($0) }
                    )
                    .background(Color(.systemBackground))
                }

                content
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
                // No under-bar treatment: the grid meets the opaque app-background
                // bar with a hard edge — no frost, no separate strip, no gradient
                // fade. A true Settings-style *blur* (content gently blurred, not
                // just dimmed) would need a variable-blur Metal shader (Variablur),
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
                        allCountries: allCountries,
                        allGenres: allGenres,
                        allDirectors: allDirectors,
                        allCast: allCast
                    )
                }
        }
        // Force a full re-layout on every portrait⇄landscape rotation. After a
        // rotation the SwiftUI hierarchy can stay stuck reporting the previous
        // orientation's width (the grid then overflows the portrait screen,
        // "zoomed-in"); rebuilding on the trait-derived size class re-resolves
        // every GeometryReader against the real window. See `vSizeClass`.
        .id(vSizeClass)
        // Overlay on the NavigationStack so the label aligns to the
        // device's screen centre, not the safe-area-inset content area.
        // The hint takes precedence over the momentary day label so the two
        // pills never stack on top of each other.
        .overlay {
            if showSwipeHint {
                SwipeHintOverlay()
                    .allowsHitTesting(false)
                    .transition(.opacity.combined(with: .scale(scale: 0.9)))
            } else if let label = dayLabel {
                DayLabelOverlay(text: label)
                    .allowsHitTesting(false)
                    .transition(.opacity.combined(with: .scale(scale: 0.9)))
            }
        }
        // "Jesteś bliżej miasta X" — offer to switch the repertoire to the
        // nearer supported city. `citySwitchSuggestion` is set (and the
        // prompt-key remembered) only by the authorized-only location check,
        // so it never fires without an existing location grant.
        .alert(
            citySwitchSuggestion.map { String(format: String(localized: "switch.nearer_title"), $0.target.name) } ?? "",
            isPresented: Binding(
                get: { citySwitchSuggestion != nil },
                set: { if !$0 { citySwitchSuggestion = nil } }
            ),
            presenting: citySwitchSuggestion
        ) { suggestion in
            Button("switch.confirm") {
                prefs.setCity(suggestion.target.slug)
                store.use(citySlug: suggestion.target.slug)
                details.use(citySlug: suggestion.target.slug)
            }
            Button("switch.not_now", role: .cancel) {}
        } message: { suggestion in
            Text(String(format: String(localized: "switch.question"), suggestion.target.name))
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
        // A parked link's film push + multi-value filters land once the TARGET
        // city's repertoire is the loaded one — not merely when films turn
        // non-empty, which on a warm app / cached cold-start is still the
        // PREVIOUS city's list (so the film lookup would miss).
        .onChange(of: store.loadedCitySlug) { _ in applyPendingDeepLinkIfReady() }
        // A link that arrived while the app was already running.
        .onChange(of: deepLink.pending) { link in
            if let link { consumeDeepLink(link) }
        }
        .onAppear {
            // A link parked before this view mounted (cold launch via a link):
            // `onChange` won't fire for a value set before we subscribed.
            if let link = deepLink.pending { consumeDeepLink(link) }
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
        // A web / Apple sign-in just returned to the foreground — skip the one
        // check that would re-surface the prompt the user already answered.
        if authService.citySwitchSuppressor.consumeShouldSkip() { return }
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

    // MARK: Deep link application

    /// Apply an inbound deep link. The city was already switched by
    /// `KinowoApp.onOpenURL`; here we land its filters and film. Scalar filters
    /// apply immediately; the film push and multi-value (country/genre/…/cinema)
    /// filters need the TARGET city's loaded repertoire, so they park in
    /// `pendingDeepLink` until `store.loadedCitySlug` matches.
    private func consumeDeepLink(_ link: DeepLink) {
        deepLink.pending = nil
        applyScalarFilters(link.filters)
        pendingDeepLink = link
        applyPendingDeepLinkIfReady()
    }

    /// Land the parked link's film + multi-value filters once the loaded
    /// repertoire is the link's target city — guarding against matching the
    /// film against the previous city's (or a stale cached) list, which would
    /// silently drop the film push (the bug MIUI hits, since it keeps the app
    /// warm with the prior city already loaded).
    private func applyPendingDeepLinkIfReady() {
        guard let link = pendingDeepLink, store.loadedCitySlug == link.citySlug else { return }
        applyRepertoireDependent(link)
    }

    /// Filters that map straight onto state with no dependency on the loaded
    /// repertoire. Absent axes are left untouched (a `?dim=2D` link doesn't wipe
    /// the user's date or search).
    private func applyScalarFilters(_ filters: DeepLinkFilters) {
        if let date = filters.date { dateFilter = date }
        formatFilter = filters.formatFilter(base: formatFilter)
        if let query = filters.query { search = query }
        if let sort = filters.sort { sortOption = sort }
    }

    /// The film push + the multi-value exclusion filters, which both need the
    /// repertoire: the `Film` to push, and the value universe to invert the
    /// link's inclusion lists into the app's exclusion sets.
    private func applyRepertoireDependent(_ link: DeepLink) {
        pendingDeepLink = nil
        let filters = link.filters
        if !filters.includedCountries.isEmpty {
            excludedCountries = filters.excluded(filters.includedCountries, universe: Set(allCountries.map(\.name)))
        }
        if !filters.includedGenres.isEmpty {
            excludedGenres = filters.excluded(filters.includedGenres, universe: Set(allGenres.map(\.name)))
        }
        if !filters.includedDirectors.isEmpty {
            excludedDirectors = filters.excluded(filters.includedDirectors, universe: Set(allDirectors.map(\.name)))
        }
        if !filters.includedCast.isEmpty {
            excludedCast = filters.excluded(filters.includedCast, universe: Set(allCast.map(\.name)))
        }
        // `?cinema=` deep links still write the web-compat `disabledCinemas`
        // mirror (single global set across cities), preserving any disabled in
        // other cities. iOS filtering itself is driven by the `selectedCinema`
        // pill, not this set — so the write only affects account state that
        // syncs back to the web (see `UserPreferences.disabledCinemas`).
        let cityCinemas = Set(allCinemas)
        if let disabledHere = filters.disabledCinemas(allCinemas: cityCinemas) {
            let others = prefs.disabledCinemas.subtracting(cityCinemas)
            prefs.setDisabledCinemas(others.union(disabledHere))
        }
        // Push the film onto a fresh stack so the detail screen is what the user
        // sees. Match the way the web's film lookup does — by normalized title
        // (Arabic→Roman fold) so a link to "…Prady 2" finds the stored
        // "…Prady II" — not byte-for-byte. A title not in the repertoire (left
        // the listing) just no-ops.
        if let title = link.filmTitle,
           let film = store.films.first(where: { DeepLinkTitle.matches($0.title, title) }) {
            navPath = [film]
        }
    }

    @ViewBuilder
    private var content: some View {
        if store.isLoading && store.films.isEmpty {
            loadingState
        } else if let error = store.error, store.films.isEmpty {
            errorState(error)
        } else {
            // One native paged `TabView` page per day preset (Dziś / Jutro /
            // 7 dni / Wszystkie); a horizontal swipe pages between them. This
            // restores the hardware-smooth `UIPageViewController` swipe the app
            // had with the old Filmy / Kina tabs. The hand-rolled carousel that
            // briefly replaced it re-filtered the whole repertoire and rebuilt
            // three grids on every drag frame (`@GestureState` invalidates the
            // body per touch-move), which dropped frames and felt choppy. The
            // trade-off of going back to the native pager is no wrap-around and
            // no mid-drag day-pill preview (the carousel's extras).
            //
            // Every page's `scrollResetToken` is keyed on the *selected* day, so
            // changing day (swipe or pill tap) snaps each grid back to row one —
            // preserving the "land at the top of the new day" behaviour the
            // single grid had (guarded by DayChangeScrollResetUITests), rather
            // than stranding the user mid-scroll on the day they swiped to.
            TabView(selection: $dateFilter) {
                ForEach(DateFilter.presets, id: \.self) { preset in
                    FilmGridView(films: films(for: preset),
                                 showCinemaHeaders: showCinemaHeaders,
                                 scrollResetToken: AnyHashable(dateFilter))
                        .refreshable { await store.reload() }
                        .tag(preset)
                }
            }
            // `.page(indexDisplayMode: .never)` gives the horizontal swipe with
            // no dot indicator — the date-pill row is the "which day" affordance.
            .tabViewStyle(.page(indexDisplayMode: .never))
            // Ignore only bottom/horizontal — NOT the top: the TabView sits below
            // the bar in the VStack, so its paged scroll view (and pan gesture)
            // starts at the bar's bottom edge and never reaches up under the pill
            // row to steal the day-pill taps. See ContentView's VStack comment.
            .ignoresSafeArea(edges: [.bottom, .horizontal])
            // Resolve NavigationLink(value: Film) from the grids to the per-film
            // detail screen — the container owns the destination so the push
            // survives a page change.
            //
            // The grids hand us a `filteredFor` copy whose showings were pruned
            // to the tapped day-page; the detail screen is a full-schedule view,
            // so re-resolve the complete, all-days film by title from the
            // unfiltered `store.films` (matching the deep-link push and Android).
            .navigationDestination(for: Film.self) { film in
                FilmDetailView(film: store.films.fullFilm(for: film))
            }
            // Flash the day name and retire the swipe hint whenever the day
            // changes — by a swipe OR a date-pill tap (both move this selection).
            .onChange(of: dateFilter) { _ in didChangeDay() }
        }
    }

    private var loadingState: some View {
        VStack(spacing: 12) {
            ProgressView()
            Text("content.loading").font(.callout).foregroundStyle(.secondary)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func errorState(_ error: Error) -> some View {
        VStack(spacing: 12) {
            Image(systemName: "exclamationmark.triangle").font(.largeTitle).foregroundStyle(.orange)
            Text("content.error")
            Text(error.localizedDescription)
                .font(.caption).foregroundStyle(.secondary)
                .multilineTextAlignment(.center)
                .padding(.horizontal)
            Button("content.retry") {
                Task { await store.reload() }
            }
            .buttonStyle(.borderedProminent)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    // Sorted, de-duplicated cinema names that appear at least once somewhere in
    // `store.films`, ordered by their short pill label. Drives the cinema pill
    // bar and scopes the `?cinema=` deep-link's web-compat write.
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
        // Only the filters Wyczyść clears light the bar — NOT cinema selection
        // or hidden films, which persist and aren't part of the reset.
        ActiveFilters.any(
            format: formatFilter,
            excludedCountries: excludedCountries,
            excludedGenres: excludedGenres,
            excludedDirectors: excludedDirectors,
            excludedCast: excludedCast
        )
    }

    /// The filtered, sorted film list for a given day filter — one call per
    /// `TabView` page (one per `DateFilter` preset), all from the same rule.
    private func films(for date: DateFilter) -> [Film] {
        store.films.filteredFor(
            date: date,
            format: formatFilter,
            query: search,
            hidden: prefs.hiddenFilms,
            selectedCinema: prefs.selectedCinema,
            excludedCountries: excludedCountries,
            excludedGenres: excludedGenres,
            excludedDirectors: excludedDirectors,
            excludedCast: excludedCast
        )
        .sorted(by: sortOption)
    }

    /// Distinct cinemas that actually appear in the currently filtered film
    /// list (after the user's per-cinema Filtry selection). When this is ≤ 1
    /// — the filter has been narrowed to a single cinema, or the city only
    /// has one — the per-card cinema label is redundant, so it's suppressed.
    private var selectedCinemaCount: Int {
        var seen = Set<String>()
        for film in films(for: dateFilter) {
            for day in film.showings {
                for c in day.cinemas { seen.insert(c.cinema) }
            }
        }
        return seen.count
    }

    /// Show the per-card cinema label only when more than one cinema is in
    /// view; a single-cinema listing names the cinema everywhere identically,
    /// so the label is just noise.
    private var showCinemaHeaders: Bool { selectedCinemaCount > 1 }

    /// Flash the given day label in the middle of the screen for ~0.7 s,
    /// then fade it out over 0.2 s. Cancels any previous fade-out task so
    /// back-to-back swipes don't leave the label stuck.
    private func showDayLabel(_ text: String) {
        dayLabelTask?.cancel()
        withAnimation(.easeInOut(duration: 0.1)) { dayLabel = text }
        dayLabelTask = Task { @MainActor in
            try? await Task.sleep(nanoseconds: 700_000_000)
            if Task.isCancelled { return }
            withAnimation(.easeInOut(duration: 0.2)) { dayLabel = nil }
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
