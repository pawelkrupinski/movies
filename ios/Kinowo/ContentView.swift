import SwiftUI

struct ContentView: View {
    @EnvironmentObject var store: RepertoireStore
    @EnvironmentObject var prefs: UserPreferences

    @State private var dateFilter: DateFilter = .anytime
    @State private var search: String = ""
    @State private var showHidden: Bool = false

    var body: some View {
        NavigationStack {
            content
                .navigationTitle("Repertuar Poznań")
                .navigationBarTitleDisplayMode(.inline)
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        Button {
                            showHidden = true
                        } label: {
                            Image(systemName: prefs.hiddenFilms.isEmpty ? "eye.slash" : "eye.slash.fill")
                        }
                        .disabled(prefs.hiddenFilms.isEmpty)
                    }
                }
                .safeAreaInset(edge: .top, spacing: 0) {
                    FiltersBar(dateFilter: $dateFilter, search: $search)
                }
                .sheet(isPresented: $showHidden) {
                    HiddenFilmsView()
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

    private var filteredFilms: [Film] {
        let query = search
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
        return store.films.compactMap { film in
            if prefs.hiddenFilms.contains(film.title) { return nil }
            if !query.isEmpty && !film.title.lowercased().contains(query) { return nil }
            let days = film.showings.filter { dateFilter.matches(date: $0.date) }
            if days.isEmpty { return nil }
            return Film(
                title: film.title,
                posterURL: film.posterURL,
                runtimeMinutes: film.runtimeMinutes,
                ratings: film.ratings,
                showings: days
            )
        }
    }
}
