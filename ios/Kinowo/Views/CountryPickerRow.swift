import SwiftUI

/// One selectable pill per country, in a horizontally scrolling row.
/// A segmented `Picker` squeezed every segment into an equal share of the
/// row width, so "United Kingdom" / "United States" clipped to an ellipsis
/// the moment a fourth or fifth country was deployed. Pills instead render
/// at each label's own intrinsic width — never truncated — and the row
/// scrolls to fit however many countries the catalog carries, mirroring
/// the Android `CountryPicker`.
struct CountryPickerRow: View {
    let countries: [Country]
    let selected: Country
    let onSelect: (Country) -> Void
    @Environment(\.locale) private var locale

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 8) {
                ForEach(countries, id: \.code) { country in
                    pill(country, isSelected: country.code == selected.code)
                }
            }
            .padding(.vertical, 2)
        }
    }

    private func pill(_ country: Country, isSelected: Bool) -> some View {
        Button {
            guard country != selected else { return }
            onSelect(country)
        } label: {
            // Looked up by code, not `country.displayName` — a country's NAME follows the
            // resolved UI language (unlike a city's own name, which never translates).
            // `Text(String)` renders verbatim; see `CountryDisplayName` for why that is a
            // literal switch, not a dynamic key.
            Text(CountryDisplayName.localized(country.code, locale: locale))
                .lineLimit(1)
                .fixedSize()
                .font(.subheadline.weight(isSelected ? .semibold : .regular))
                .padding(.horizontal, 14)
                .padding(.vertical, 8)
                .background(
                    isSelected ? Color.accentColor.opacity(0.85) : Color(.secondarySystemFill),
                    in: Capsule()
                )
                .foregroundStyle(isSelected ? Color.white : Color.primary)
        }
        .buttonStyle(.plain)
    }
}
