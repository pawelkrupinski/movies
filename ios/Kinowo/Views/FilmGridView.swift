import SwiftUI

struct FilmGridView: View {
    let films: [Film]

    private let columns = [GridItem(.adaptive(minimum: 160, maximum: 220), spacing: 12, alignment: .top)]

    var body: some View {
        if films.isEmpty {
            VStack(spacing: 8) {
                Image(systemName: "film").font(.largeTitle).foregroundStyle(.secondary)
                Text("Brak repertuaru.").foregroundStyle(.secondary)
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            ScrollView {
                LazyVGrid(columns: columns, alignment: .leading, spacing: 12) {
                    ForEach(films) { film in
                        FilmCardView(film: film)
                    }
                }
                .padding(.horizontal, 12)
                .padding(.bottom, 24)
            }
            // Drag-to-dismiss the search keyboard: scrolling the grid at
            // all hides the keyboard immediately. Tap-to-dismiss is
            // handled by the background overlay in ContentView.
            .scrollDismissesKeyboard(.immediately)
        }
    }
}
