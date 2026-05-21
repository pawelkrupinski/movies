import SwiftUI

struct ContentView: View {
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var prefs: UserPreferences

    @State private var dateFilter: DateFilter = .today
    @State private var formatFilter: FormatFilter = .empty
    @State private var search: String = ""
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

    @State private var tabLabel: String? = nil
    @State private var tabLabelTask: Task<Void, Never>?

    enum Tab: Hashable { case films, cinemas }

    var body: some View {
        NavigationStack {
            content
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
                // The SwiftUI nav-bar's `.navigationBarLeading` slot
                // clips a horizontal ScrollView to a few dozen points of
                // width, which crushed the date-pill row down to one
                // half-visible pill. Hide the native nav bar entirely
                // and render our own top chrome via `safeAreaInset(top)`,
                // so the HStack gets the full screen width to spread
                // across.
                .toolbar(.hidden, for: .navigationBar)
                .safeAreaInset(edge: .top, spacing: 0) {
                    TopBar(
                        dateFilter: $dateFilter,
                        filtersActive: filtersActive,
                        onTapFilters: { showFilters = true }
                    )
                }
                .safeAreaInset(edge: .bottom, spacing: 0) {
                    SearchBar(search: $search, focused: $searchFocused)
                }
                .sheet(isPresented: $showFilters) {
                    FiltersSheet(
                        formatFilter: $formatFilter,
                        prefs: prefs,
                        allCinemas: allCinemas,
                        // Hide Filtry's Kina checkbox section on /kina —
                        // that screen owns its own pinned-cinema picker
                        // via the pill row, so duplicating the choice
                        // inside Filtry would be confusing.
                        showCinemaSection: tab == .films
                    )
                }
        }
        // Overlay on the NavigationStack so the label aligns to the
        // device's screen edges, not the safe-area-inset content area.
        // `.padding(.bottom, 110)` clears the bottom safeAreaInset
        // (~70pt search pill + 14pt dip + 24pt breathing buffer).
        .overlay(alignment: .bottom) {
            if let label = tabLabel {
                TabLabelOverlay(text: label)
                    .padding(.bottom, 110)
                    .allowsHitTesting(false)
                    .transition(.opacity.combined(with: .scale(scale: 0.9)))
            }
        }
        .task {
            if store.films.isEmpty { await store.reload() }
        }
        .onAppear {
            // Briefly name the starting tab so the user sees the same
            // label-on-arrival affordance the swipe-driven changes get.
            // Idempotent — running on every appear is fine; the task
            // cancels the previous fade-out cleanly.
            showTabLabel(tab == .films ? "Filmy" : "Kina")
        }
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
            .onChange(of: tab) { new in
                showTabLabel(new == .films ? "Filmy" : "Kina")
            }
        }
    }

    @ViewBuilder
    private var cinemasPage: some View {
        VStack(spacing: 0) {
            CinemaPillsRow(
                allCinemas: allCinemas,
                pinnedCinema: $pinnedCinema
            )
            FilmGridView(films: filmsForCinemasTab)
                .refreshable { await store.reload() }
        }
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
        return out.sorted()
    }

    // Filtry button uses its `.fill` variant whenever any of the
    // axes Filtry owns is active — cinemas filtered, format
    // constrained, or any hidden films queued. Last one is here so
    // the user can spot "I've hidden a film" at a glance: the Ukryte
    // filmy list lives inside Filtry, so the only outward cue that
    // hidden films exist is the filled button.
    private var filtersActive: Bool {
        !formatFilter.isEmpty
            || !prefs.disabledCinemas.isEmpty
            || !prefs.hiddenFilms.isEmpty
    }

    private var filmsForFilmsTab: [Film] {
        store.films.filteredFor(
            date: dateFilter,
            format: formatFilter,
            query: search,
            hidden: prefs.hiddenFilms,
            disabledCinemas: prefs.disabledCinemas
        )
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
            disabledCinemas: disabled
        )
    }

    /// Flash the given label at the bottom of the screen for ~0.7 s,
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
}
