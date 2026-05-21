import SwiftUI

struct HiddenFilmsView: View {
    @EnvironmentObject var prefs: UserPreferences
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        NavigationStack {
            Group {
                if prefs.hiddenFilms.isEmpty {
                    VStack(spacing: 8) {
                        Image(systemName: "eye").font(.largeTitle).foregroundStyle(.secondary)
                        Text("Brak ukrytych filmów.").foregroundStyle(.secondary)
                    }
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                } else {
                    List {
                        ForEach(Array(prefs.hiddenFilms).sorted(by: { $0.localizedCaseInsensitiveCompare($1) == .orderedAscending }), id: \.self) { title in
                            HStack {
                                Text(title)
                                Spacer()
                                Button("Pokaż") {
                                    withAnimation { prefs.unhide(title) }
                                }
                                .buttonStyle(.bordered)
                            }
                        }
                    }
                }
            }
            .navigationTitle("Ukryte filmy")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .confirmationAction) {
                    Button("Gotowe") { dismiss() }
                }
            }
        }
    }
}
