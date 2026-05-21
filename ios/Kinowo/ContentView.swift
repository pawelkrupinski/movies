import SwiftUI

struct ContentView: View {
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var prefs: UserPreferences

    @State private var dateFilter: DateFilter = .today
    @State private var formatFilter: FormatFilter = .empty
    @State private var search: String = ""
    @State private var showFilters: Bool = false
    @FocusState private var searchFocused: Bool

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
                        allCinemas: allCinemas
                    )
                }
        }
        .task {
            if store.films.isEmpty { await store.reload() }
        }
    }

    @ViewBuilder
    private var content: some View {
        if store.isLoading && store.films.isEmpty {
            VStack(spacing: 12) {
                ProgressView()
                Text("Ładowanie repertuaru…").font(.callout).foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else if let error = store.error, store.films.isEmpty {
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
        } else {
            FilmGridView(films: filteredFilms)
                .refreshable { await store.reload() }
        }
    }

    // Sorted, de-duplicated cinema names that appear at least once
    // somewhere in `store.films`. Drives the Kina list in FiltersSheet
    // and the "Wszystkie kina" master toggle.
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
    // the user can spot "I've hidden a film" at a glance now that
    // the eye toolbar button is gone (the Ukryte filmy list lives
    // inside Filtry).
    private var filtersActive: Bool {
        !formatFilter.isEmpty
            || !prefs.disabledCinemas.isEmpty
            || !prefs.hiddenFilms.isEmpty
    }

    private var filteredFilms: [Film] {
        let query = search
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
        return store.films.compactMap { film in
            if prefs.hiddenFilms.contains(film.title) { return nil }
            if !query.isEmpty && !film.title.lowercased().contains(query) { return nil }

            // Per-day / per-cinema / per-showtime filtering: drop a day
            // whose every cinema is filtered out, drop a cinema whose
            // every showtime fails the format/from-hour filter. The
            // film disappears only when no day has any surviving
            // showtime — matching the web's `applyFilters()` semantics
            // (a movie card stays visible as long as one badge passes).
            let filteredDays: [DayShowings] = film.showings.compactMap { day in
                if !dateFilter.matches(date: day.date) { return nil }
                let filteredCinemas: [CinemaShowings] = day.cinemas.compactMap { cg in
                    if prefs.disabledCinemas.contains(cg.cinema) { return nil }
                    let times = formatFilter.isEmpty
                        ? cg.showtimes
                        : cg.showtimes.filter { formatFilter.matches(showtime: $0) }
                    guard !times.isEmpty else { return nil }
                    return CinemaShowings(cinema: cg.cinema, cinemaURL: cg.cinemaURL, showtimes: times)
                }
                guard !filteredCinemas.isEmpty else { return nil }
                return DayShowings(date: day.date, label: day.label, cinemas: filteredCinemas)
            }
            if filteredDays.isEmpty { return nil }
            return Film(
                title: film.title,
                posterURL: film.posterURL,
                runtimeMinutes: film.runtimeMinutes,
                ratings: film.ratings,
                showings: filteredDays
            )
        }
    }
}
