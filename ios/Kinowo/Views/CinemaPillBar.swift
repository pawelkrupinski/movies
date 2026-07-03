import SwiftUI

// A slim, low-emphasis control sitting directly under the top bar. Collapsed,
// it's a thin full-width handle row naming the current cinema selection with a
// chevron; tapping anywhere on it unfolds a single horizontally-scrolling row
// of cinema pills. Exactly one pill is active at a time — the leading
// "Wszystkie" (nil selection, no cinema constraint) or one cinema. Mirrors the
// Android app's parallel cinema-pill selector.
//
// The pills reuse `DatePillsRow`'s visual language (capsule, accent tint when
// selected, `TopBar.viewportScale`) so they read as the same family as the day
// pills above them — just scrolling instead of justified, since a city can have
// far more cinemas than fit a row.
struct CinemaPillBar: View {
    /// Every cinema in the current city (full names), already sorted by the
    /// caller in the same short-name order the pills display.
    let cinemas: [String]
    /// The active selection (nil = Wszystkie). Bound to `UserPreferences`.
    let selectedCinema: String?
    /// Commit a new selection (nil = Wszystkie).
    let onSelect: (String?) -> Void

    @State private var expanded = false

    private var scale: CGFloat { TopBar.viewportScale }

    /// What the collapsed handle names: the selected cinema's short label, or
    /// "Wszystkie kina" when nothing is chosen. A stale cross-city selection
    /// (not among `cinemas`) reads as "Wszystkie kina" too — it isn't filtering
    /// anything (see `filteredFor`'s guard), so naming it would mislead.
    private var handleLabel: String {
        if let selectedCinema, cinemas.contains(selectedCinema) {
            return CinemaSection.pillName(for: selectedCinema)
        }
        return "Wszystkie kina"
    }

    var body: some View {
        VStack(spacing: 0) {
            handleRow
            if expanded {
                pillRow
                    .transition(.move(edge: .top).combined(with: .opacity))
            }
        }
        // Clip the unfolding pills to the bar's bounds so they slide out from
        // under the handle rather than popping in over the grid.
        .clipped()
    }

    private var handleRow: some View {
        Button {
            withAnimation(.easeInOut(duration: 0.22)) { expanded.toggle() }
        } label: {
            HStack(spacing: 6 * scale) {
                Text(handleLabel)
                    .font(.system(size: 13 * scale, weight: .medium))
                    .lineLimit(1)
                Spacer(minLength: 0)
                Image(systemName: "chevron.down")
                    .font(.system(size: 11 * scale, weight: .semibold))
                    .rotationEffect(.degrees(expanded ? 180 : 0))
            }
            .foregroundStyle(.secondary)
            .padding(.horizontal, 14 * scale)
            .padding(.vertical, 6 * scale)
            .contentShape(Rectangle())
        }
        .buttonStyle(.plain)
        .accessibilityIdentifier(A11y.CinemaBar.handle)
        .accessibilityLabel("Kino: \(handleLabel)")
    }

    private var pillRow: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            LazyHStack(spacing: 6 * scale) {
                pill(label: "Wszystkie",
                     isSelected: selectedCinema == nil || !cinemas.contains(selectedCinema ?? ""),
                     identifier: A11y.CinemaBar.allPill) {
                    onSelect(nil)
                }
                ForEach(cinemas, id: \.self) { cinema in
                    pill(label: CinemaSection.pillName(for: cinema),
                         isSelected: selectedCinema == cinema,
                         identifier: "\(A11y.CinemaBar.pillPrefix).\(cinema)") {
                        // Tapping the active cinema again clears back to Wszystkie.
                        onSelect(selectedCinema == cinema ? nil : cinema)
                    }
                }
            }
            .padding(.horizontal, 14 * scale)
            .padding(.bottom, 6 * scale)
        }
    }

    private func pill(
        label: String,
        isSelected: Bool,
        identifier: String,
        action: @escaping () -> Void
    ) -> some View {
        Button(action: action) {
            Text(label)
                .font(.system(size: 14 * scale, weight: .medium))
                .lineLimit(1)
                .fixedSize()
                .padding(.horizontal, 12 * scale)
                .padding(.vertical, 7 * scale)
                .background(
                    isSelected ? Color.accentColor.opacity(0.85) : Color.clear,
                    in: Capsule()
                )
                .foregroundColor(isSelected ? .white : .primary)
        }
        .buttonStyle(.plain)
        .accessibilityIdentifier(identifier)
        .accessibilityAddTraits(isSelected ? .isSelected : [])
    }
}
