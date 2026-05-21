import SwiftUI

struct FiltersBar: View {
    @Binding var dateFilter: DateFilter
    @Binding var search: String

    var body: some View {
        VStack(spacing: 8) {
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
                .padding(.horizontal, 1)
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .background(.ultraThinMaterial)
    }
}
