import SwiftUI

// Top safe-area inset: horizontally-scrolling pill row for the date
// filter. The companion `SearchBar` below docks to the bottom inset
// (so the keyboard slides it up cleanly on focus instead of covering
// it at the top).
struct FiltersBar: View {
    @Binding var dateFilter: DateFilter

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 6) {
                ForEach(DateFilter.presets, id: \.self) { f in
                    Button {
                        dateFilter = f
                    } label: {
                        Text(f.label)
                            .font(.system(size: 13, weight: .medium))
                            .padding(.horizontal, 12)
                            .padding(.vertical, 6)
                            .background(
                                dateFilter == f
                                    ? Color.accentColor.opacity(0.75)
                                    : Color.white.opacity(0.10),
                                in: Capsule()
                            )
                            .foregroundColor(.white)
                    }
                    .buttonStyle(.plain)
                }
            }
            .padding(.horizontal, 12)
        }
        .padding(.vertical, 8)
        .background(.ultraThinMaterial)
    }
}

// Bottom safe-area inset: search field. Lives at the thumb-zone edge
// of the screen so reaching it on a 6.7" phone doesn't require a
// stretch, and so the iOS keyboard slides the field up into view
// directly (vs. the keyboard covering a top-of-screen field).
struct SearchBar: View {
    @Binding var search: String

    var body: some View {
        HStack(spacing: 6) {
            Image(systemName: "magnifyingglass").foregroundStyle(.secondary)
            TextField("Szukaj filmu", text: $search)
                .textInputAutocapitalization(.never)
                .autocorrectionDisabled()
                .foregroundColor(.white)
            if !search.isEmpty {
                Button {
                    search = ""
                } label: {
                    Image(systemName: "xmark.circle.fill").foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
            }
        }
        .padding(8)
        .background(Color.white.opacity(0.08), in: RoundedRectangle(cornerRadius: 8))
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(.ultraThinMaterial)
    }
}
