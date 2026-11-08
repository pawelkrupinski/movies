import SwiftUI

// Pushed from Filtry's "Ukryte filmy" row. Lists every hidden title with
// a per-row show button and a destructive show-all bulk action.
// When the set drains to empty, pop back to Filtry automatically — there
// is nothing left to manage and the row itself would also have disappeared
// from the parent screen.
struct HiddenFilmsList: View {
    var prefs: UserPreferences
    @Environment(\.dismiss) private var dismiss
    @State private var query = ""

    private var visibleTitles: [String] {
        HiddenFilmsFilter.visibleTitles(in: prefs.hiddenFilms, query: query)
    }

    var body: some View {
        Form {
            Section {
                ForEach(visibleTitles, id: \.self) { title in
                    HiddenFilmRow(title: title, prefs: prefs)
                }
            }

            Section {
                Button(role: .destructive) {
                    withAnimation { prefs.unhideAll() }
                } label: {
                    Text("hiddenfilms.show_all")
                        .frame(maxWidth: .infinity)
                }
            }
        }
        .searchable(text: $query, placement: .navigationBarDrawer(displayMode: .always), prompt: Text("search.placeholder"))
        .navigationTitle("filtersheet.hidden_films")
        .navigationBarTitleDisplayMode(.inline)
        .onChange(of: prefs.hiddenFilms) { _, new in
            if new.isEmpty { dismiss() }
        }
    }
}

private struct HiddenFilmRow: View {
    let title: String
    var prefs: UserPreferences

    var body: some View {
        HStack {
            Text(title).lineLimit(1)
            Spacer()
            Button("hiddenfilms.show") {
                withAnimation { prefs.unhide(title) }
            }
            .buttonStyle(.bordered)
            .controlSize(.small)
        }
    }
}
