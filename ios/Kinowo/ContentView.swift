import SwiftUI

struct ContentView: View {
    @Environment(RepertoireStore.self) var store
    @Environment(DetailsStore.self) var details
    @Environment(UserPreferences.self) var prefs
    @Environment(CatalogStore.self) var catalog
    @Environment(AuthService.self) var authService
    @Environment(DeepLinkCoordinator.self) var deepLink
    @Environment(\.stateSync) var sync
    @Environment(\.scenePhase) private var scenePhase
    @Environment(\.locale) private var locale
    /// Whether the window scene is in a landscape interface orientation, read
    /// from UIKit by `InterfaceOrientationReader` — NOT from SwiftUI's layout
    /// pass, so it stays correct even when the layout cache goes stale after a
    /// rotation. Each change bumps `rotationGeneration`, the root view's `.id`,
    /// forcing a full re-layout against the real window size: without it a
    /// landscape⇄portrait flip can leave every geometry measurement stuck at the
    /// old orientation's width. Not the vertical size class: the unfolded iPhone
    /// Duo's inner display (669 x 951 pt) is regular both ways, so it kept
    /// landscape's four columns, clipped at both edges, rotated to portrait.
    @State private var interfaceLandscape: Bool?
    /// Bumped on every change of `interfaceLandscape` after its first report:
    /// the launch's first report must not rebuild the tree mid-layout.
    @State private var rotationGeneration = 0

    /// The film-detail push stack. Held as ContentView state — NOT inside the
    /// NavigationStack — so it survives the `.id(rotationGeneration)` rebuild on every
    /// rotation. `.id` re-identifies (and tears down) only the NavigationStack
    /// subtree, leaving ContentView's own state intact; the rebuilt stack then
    /// restores its pushes from this binding. With a link-driven stack the
    /// push state lived *inside* the recreated subtree, so a rotation while on
    /// a film detail popped the user back to the grid (guarded by
    /// RotationColumnsUITests.testStaysOnFilmDetailAfterRotation).
    @State private var navPath: [Film] = []

    @State private var dateFilter: DateFilter = .today
    /// The Filtry sheet's axes — what its Wyczyść resets (`ListingFilters`).
    @State private var filters = ListingFilters()
    @State private var search: String = ""
    /// The search's day (`SearchDay`): what clearing the search goes back to, and which days it
    /// brought in.
    @State private var searchDay = SearchDay()
    /// Set while the search itself moves `dateFilter`, so that move is not taken for a pick.
    @State private var dayMovedBySearch = false
    @State private var showFilters: Bool = false
    /// First-visit area picker for a split city (presented once — see below).
    @State private var showAreaPicker: Bool = false
    @FocusState private var searchFocused: Bool

    /// A deep link whose film push / multi-value filters couldn't be applied yet
    /// because the repertoire (hence the value universe and the `Film` to push)
    /// hadn't loaded. Re-applied the moment the first repertoire load lands.
    @State private var pendingDeepLink: DeepLink?

    /// Live viewport width, tracked by `onGeometryChange` so the
    /// search placement re-evaluates on rotation / iPad split-view resize.
    @State private var viewportWidth: CGFloat = UIScreen.main.bounds.width
    /// The window's safe-area width and horizontal insets, from which follow
    /// the listing's width (published to the grid cards, the detail poster and
    /// the top bar as `listingViewportWidth`) and whether the grid runs under
    /// the horizontal insets (the top bar never does). See the NavigationStack's
    /// measurement for why it isn't `viewportWidth`, and `ListingSafeArea` for
    /// when it does.
    @State private var listingSafeArea = ListingSafeArea(
        safeWidth: UIScreen.main.bounds.width, leading: 0, trailing: 0)
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

    /// "You're nearer another city — switch?" prompt (`CitySwitchAlert`).
    /// Populated by the authorized-only location check on open / foreground;
    /// presenting the alert when non-nil.
    @State private var citySwitchSuggestion: City.CitySwitchSuggestion?
    /// The app's one resolver, built in `KinowoApp`.
    @EnvironmentObject private var locationResolver: LocationCityResolver

    /// Run after the carousel commits a day change (it has already advanced
    /// `dateFilter`): flash the new day's name and retire the onboarding hint
    /// (`markSwiped()` is idempotent, so running it on every swipe is harmless).
    private func didChangeDay() {
        showDayLabel(dateFilter.label(locale: locale))
        swipeHintTask?.cancel()
        withAnimation(.easeInOut(duration: 0.2)) { showSwipeHint = false }
        prefs.markSwiped()
    }

    var body: some View {
        withLifecycle(
            NavigationStack(path: $navPath) { withSheets(screen) }
                // Force a full re-layout on every portrait⇄landscape rotation. After a
                // rotation the SwiftUI hierarchy can stay stuck reporting the previous
                // orientation's width (the grid then overflows the portrait screen,
                // "zoomed-in"); rebuilding on UIKit's interface orientation re-resolves
                // every geometry measurement against the real window. See
                // `interfaceLandscape`.
                .id(rotationGeneration)
                .background(InterfaceOrientationReader { adoptOrientation(landscape: $0) })
                // Card widths, the detail poster and the top bar's scale all derive
                // from the live window width rather than `UIScreen` at launch, so they
                // follow rotation and iPhone Duo fold / unfold.
                .environment(\.listingViewportWidth, listingSafeArea.listingWidth)
                // Measured on the NavigationStack, which takes the window's size, not
                // on its content: the top bar scales with this width, so a content
                // measurement feeds back — a bar still at landscape scale after
                // rotating to portrait overflows the screen and keeps the width wide.
                // The grid bleeds into symmetric horizontal insets (a landscape
                // phone's) but not a one-sided system column (the iPhone Duo's status
                // bar); see `ListingSafeArea`.
                .onGeometryChange(for: ListingSafeArea.self, of: { geo in
                    ListingSafeArea(safeWidth: geo.size.width,
                                    leading: geo.safeAreaInsets.leading,
                                    trailing: geo.safeAreaInsets.trailing)
                }) { listingSafeArea = $0 }
                .overlay { transientOverlay }
                .modifier(CitySwitchAlert(suggestion: $citySwitchSuggestion))
        )
    }

    /// The bar and the paged grids are a VStack, NOT an overlay /
    /// safeAreaInset: the bar must sit ABOVE the TabView in the layout,
    /// so the paged scroll view's frame — and its pan gesture — starts
    /// below the bar. Overlaying the bar (or insetting it) left the
    /// TabView spanning the full height, so its scroll/page gesture
    /// reached up under the pill row and stole the day-pill taps (a
    /// still tap fired, a tap with the slightest movement, or one
    /// arriving while the grid coasted, got swallowed by the scroll).
    private var screen: some View {
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

            // Nothing sits between the top bar and the grid: the cinema
            // filter lives in the Filtry sheet ("Kina"), not in a pulldown
            // bar here.
            content
        }
        // Tap-to-dismiss: a transparent backdrop sits behind the grid content and only
        // intercepts taps while the search is focused. Hit-testing falls through to film
        // cards / pills / toolbar buttons (they paint above this layer), so the dismiss
        // only fires on taps that land in empty grid space.
        .background(
            Color.clear
                .contentShape(Rectangle())
                .onTapGesture { searchFocused = false }
                .allowsHitTesting(searchFocused)
        )
        // The bottom only: the top bar stays inside the horizontal
        // safe area, clear of a landscape phone's Dynamic Island; the
        // paged grid below bleeds past it itself (`ignoredEdges`).
        .ignoresSafeArea(edges: .bottom)
        .toolbar(.hidden, for: .navigationBar)
        // No under-bar treatment: the grid meets the opaque app-background bar with a
        // hard edge — no frost, no separate strip, no gradient fade.
        // Narrow screens float search at the bottom; wide screens
        // host it inline on the top bar instead (see TopBar).
        .overlay(alignment: .bottom) {
            if !searchInline {
                SearchBar(search: $search, focused: $searchFocused)
            }
        }
        // Track the live width so `searchInline` follows rotation
        // and resize. Fires with the initial width too — no layout effect.
        .onGeometryChange(for: CGFloat.self, of: { $0.size.width }) { viewportWidth = $0 }
    }

    /// The Filtry sheet and, on the first visit to a split city (London), the area picker:
    /// it fires when the catalog resolves as split and this city hasn't been seen, shown
    /// once (markAreaPickerSeen); a no-op on flat cities.
    private func withSheets(_ view: some View) -> some View {
        view
            .sheet(isPresented: $showFilters) {
                FiltersSheet(filters: $filters, prefs: prefs, facets: facets)
            }
            .onChange(of: store.catalog) { _, catalog in offerAreaPickerIfFirstVisit(to: catalog) }
            .sheet(isPresented: $showAreaPicker) {
                AreaPickerSheet(catalog: store.catalog) { applyAreaPickerChoice(disabled: $0) }
            }
    }

    /// The swipe hint, which takes precedence over the momentary day label so the two
    /// pills never stack. An overlay on the NavigationStack so it aligns to the device's
    /// screen centre, not the safe-area-inset content area.
    @ViewBuilder
    private var transientOverlay: some View {
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

    /// The load-on-open task and the observers that react to loads, deep links and
    /// foregrounding.
    private func withLifecycle(_ view: some View) -> some View {
        view
            .task { await loadOnOpen() }
            // Cold start: the first non-empty `store.films` is the first
            // repertoire load completing.
            .onChange(of: store.films.isEmpty) { _, isEmpty in
                if !isEmpty {
                    maybeShowSwipeHint()
                    openFiltersIfRequested()
                }
            }
            // A parked link's film push + multi-value filters land once the TARGET
            // city's repertoire is the loaded one — not merely when films turn
            // non-empty, which on a warm app / cached cold-start is still the
            // PREVIOUS city's list (so the film lookup would miss).
            .onChange(of: store.loadedCitySlug) { applyPendingDeepLinkIfReady() }
            // A link that arrived while the app was already running.
            .onChange(of: deepLink.pending) { _, link in
                if let link { consumeDeepLink(link) }
            }
            .onAppear {
                // A link parked before this view mounted (cold launch via a link):
                // `onChange` won't fire for a value set before we subscribed.
                if let link = deepLink.pending { consumeDeepLink(link) }
                Task { await maybeSuggestCitySwitch() }
            }
            .onChange(of: scenePhase) { _, phase in
                if phase == .active { refreshOnForeground() }
            }
    }

    /// Revalidate the catalog, show the cached data, then fetch listing + details concurrently.
    private func loadOnOpen() async {
        // Conditional GET; a 304 costs only headers. Non-blocking — the UI already
        // renders from the seeded/persisted catalog.
        Task { await catalog.reload(baseURL: kinowoBaseURL) }
        store.loadCachedData()
        store.pruneStaleShowings()
        details.loadCachedData()
        // A warm cache fills `store.films` synchronously above, before any
        // `films.isEmpty` change can fire — so evaluate the hint here too.
        maybeShowSwipeHint()
        // The grid renders the moment `store.reload()` lands (synopsis/trailers are
        // non-essential), so details never block the first paint.
        async let repertoire: Void = store.reload()
        async let detailsLoad: Void = details.reload()
        _ = await (repertoire, detailsLoad)
        maybeShowSwipeHint()
        // Once-a-day poster-cache purge: drop cached posters for films that have left.
        await store.reconcilePostersIfNeeded()
    }

    /// App came back from background — drop screenings that slipped into the past while
    /// idle and re-sort cinemas by their earliest remaining slot of the day (the server's
    /// `now - 30min` cutoff, so cached and fresh payloads age identically), then
    /// `reloadIfStale` fetches when the cached payload is more than 60s old. The local
    /// prune runs first so a time-only update lands before the network roundtrip.
    private func refreshOnForeground() {
        store.pruneStaleShowings()
        Task { await store.reloadIfStale() }
        Task { await details.reloadIfStale() }
        Task { await catalog.reloadIfStale(baseURL: kinowoBaseURL) }
        // Re-check on every foreground so travelling between cities
        // mid-session offers the switch as soon as the app returns.
        Task { await maybeSuggestCitySwitch() }
        // A REMOTE hiddenFilms change (another device, or the web) only reaches this one
        // on the next reconcile; without this it would stay invisible until a cold start.
        Task { await sync?.reconcileCurrentCountry() }
    }

    private func adoptOrientation(landscape: Bool) {
        if let previous = interfaceLandscape, previous != landscape { rotationGeneration += 1 }
        interfaceLandscape = landscape
    }

    private func offerAreaPickerIfFirstVisit(to catalog: CinemaCatalog) {
        if catalog.isSplit,
           let city = prefs.selectedCity,
           !prefs.areaPickerSeenCities.contains(city) {
            showAreaPicker = true
        }
    }

    /// Reset this city's slice, then disable the unchecked areas' cinemas; other cities'
    /// entries are preserved.
    private func applyAreaPickerChoice(disabled: [String]) {
        var s = prefs.disabledCinemas.subtracting(store.catalog.cinemas)
        s.formUnion(disabled)
        prefs.setDisabledCinemas(s)
        if let city = prefs.selectedCity { prefs.markAreaPickerSeen(city) }
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
              let suggestion = catalog.switchSuggestion(
                chosenSlug: chosen,
                lat: fix.lat, lon: fix.lon,
                lastPromptKey: prefs.citySwitchPromptKey,
                inCountry: prefs.selectedCountry.code
              )
        else { return }
        prefs.setCitySwitchPromptKey(suggestion.key)
        citySwitchSuggestion = suggestion
    }

    // MARK: Deep link application

    /// Open the Filtry sheet on launch when `KINOWO_UITEST_OPEN_FILTERS` is set,
    /// so the store-screenshot driver can capture it. Every other screen it
    /// shoots is reached by deep link, but the sheet has no link route — it is
    /// app state, not a web URL — and `simctl` cannot tap, so a launch hook is
    /// the only way in (the same shape as `KINOWO_UITEST_DEEPLINK`).
    ///
    /// Called once the first repertoire load lands, because the sheet's
    /// sections are built from the loaded films: opened any earlier it would
    /// render with empty genre / country / cast lists.
    private func openFiltersIfRequested() {
        #if DEBUG
        guard ProcessInfo.processInfo.environment["KINOWO_UITEST_OPEN_FILTERS"] != nil else { return }
        showFilters = true
        #endif
    }

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
    private func applyScalarFilters(_ linked: DeepLinkFilters) {
        if let date = linked.date { dateFilter = date }
        filters.applyScalars(of: linked)
        if let query = linked.query { search = query }
    }

    /// The film push + the multi-value exclusion filters, which both need the
    /// repertoire: the `Film` to push, and the value universe to invert the
    /// link's inclusion lists into the app's exclusion sets.
    private func applyRepertoireDependent(_ link: DeepLink) {
        pendingDeepLink = nil
        filters.applyInclusions(of: link.filters, facets: facets)
        // `?cinema=` deep links write the `disabledCinemas` set (one global set
        // across cities), preserving any disabled in other cities. That set is
        // also what drives the listing, so the link narrows the grid as well as
        // the account state that syncs back to the web.
        let cityCinemas = Set(allCinemas)
        if let disabledHere = link.filters.disabledCinemas(allCinemas: cityCinemas) {
            let others = prefs.disabledCinemas.subtracting(cityCinemas)
            prefs.setDisabledCinemas(others.union(disabledHere))
        }
        // Push the film onto a fresh stack so the detail screen is what the user
        // sees. A film no longer in the listing just no-ops.
        if let film = store.films.deepLinkTarget(link) {
            navPath = [film]
        }
    }

    @ViewBuilder
    private var content: some View {
        if store.isLoading && store.films.isEmpty {
            ListingLoadingView()
        } else if let error = store.error, store.films.isEmpty {
            ListingErrorView(error: error) { Task { await store.reload() } }
        } else {
            pager
        }
    }

    private var pager: some View {
        DayPager(
            dateFilter: $dateFilter,
            films: films(for:),
            outsideDays: { preset in
                preset == .anytime ? searchDay.outsideDays(now: Date(), zone: cityZone) : nil
            },
            ignoredEdges: listingSafeArea.ignoredEdges
        )
        // Flash the day name and retire the swipe hint whenever the day
        // changes — by a swipe OR a date-pill tap (both move this selection).
        .onChange(of: dateFilter) {
            // A day moved by the search itself is no pick; any other (a swipe, a pill) is.
            if dayMovedBySearch { dayMovedBySearch = false } else { searchDay.dayPicked() }
            didChangeDay()
        }
        .onChange(of: search) {
            if let day = searchDay.queryChanged(to: search, showing: dateFilter) {
                dayMovedBySearch = true
                dateFilter = day
            }
        }
    }

    // Sorted, de-duplicated cinema names that appear at least once somewhere in
    // `store.films`, ordered by their short pill label. Drives the cinema pill
    // bar and scopes the `?cinema=` deep-link's web-compat write.
    private var allCinemas: [String] { store.films.allCinemas() }

    /// The loaded repertoire's name universes — the Filtry lists, and what a
    /// deep link's inclusion lists invert against.
    private var facets: FilterFacets { FilterFacets(films: store.films) }

    private var filtersActive: Bool {
        filters.isActive(over: store.films, cityCinemas: allCinemas,
                         disabledCinemas: prefs.disabledCinemas, hidden: prefs.hiddenFilms)
    }

    /// The listing of one `TabView` page (one per `DateFilter` preset).
    private func films(for date: DateFilter) -> [Film] {
        filters.films(from: store.films, date: date, query: search, hidden: prefs.hiddenFilms,
                      disabledCinemas: prefs.disabledCinemas, zone: cityZone)
    }

    /// The selected city's zone: what "today" means for its listings.
    private var cityZone: TimeZone { catalog.zone(ofSlug: prefs.selectedCity, inCountry: prefs.selectedCountry) }

    /// Flash the given day label in the middle of the screen for ~0.7 s,
    /// then fade it out over 0.2 s. Cancels any previous fade-out task so
    /// back-to-back swipes don't leave the label stuck.
    private func showDayLabel(_ text: String) {
        dayLabelTask?.cancel()
        withAnimation(.easeInOut(duration: 0.1)) { dayLabel = text }
        dayLabelTask = Task { @MainActor in
            try? await Task.sleep(for: .milliseconds(700))
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
            try? await Task.sleep(for: .milliseconds(2500))
            if Task.isCancelled { return }
            withAnimation(.easeInOut(duration: 0.3)) { showSwipeHint = false }
        }
    }
}
