import SwiftUI
import UIKit

// The magnifier + text field + clear-button row shared by both search
// placements: the floating bottom `SearchBar` and the inline
// `InlineSearchField` on the top bar. Glyph size and font scale with the
// placement so the inline field matches the date-pill height.
struct SearchFieldContent: View {
    @Binding var search: String
    @FocusState.Binding var focused: Bool
    var iconSize: CGFloat = 17
    var font: Font = .body

    var body: some View {
        HStack(spacing: 10) {
            Image(systemName: "magnifyingglass")
                .font(.system(size: iconSize))
                .foregroundStyle(.secondary)
            TextField("search.placeholder", text: $search)
                .font(font)
                .textInputAutocapitalization(.never)
                .autocorrectionDisabled()
                .focused($focused)
                .foregroundColor(.primary)
                .accessibilityIdentifier(A11y.Search.field)
            if !search.isEmpty {
                Button {
                    search = ""
                } label: {
                    Image(systemName: "xmark.circle.fill")
                        .font(.system(size: iconSize))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
                .accessibilityIdentifier(A11y.Search.clear)
            }
        }
    }
}

// Inline search on the top bar (wide screens): the shared field in a
// translucent capsule sized to the date pills, sitting between the pills
// and the Filtry button. Sizes track the same `scale` as the rest of the
// bar so it matches the pill height.
struct InlineSearchField: View {
    @Binding var search: String
    @FocusState.Binding var focused: Bool
    let scale: CGFloat

    var body: some View {
        SearchFieldContent(
            search: $search,
            focused: $focused,
            iconSize: 15 * scale,
            font: .system(size: 14 * scale)
        )
        .padding(.horizontal, 12 * scale)
        .padding(.vertical, 6 * scale)
        .modifier(GlassyPillBackground())
    }
}

// Bottom safe-area inset: a single floating search pill, styled like
// the native iOS search field (Settings / Contacts / Mail). No outer
// chrome container — the pill sits over the grid content with a
// translucent `GlassyPillBackground`, so the grid scrolls visibly
// behind it.
struct SearchBar: View {
    @Binding var search: String
    @FocusState.Binding var focused: Bool

    var body: some View {
        SearchFieldContent(search: $search, focused: $focused)
        .padding(.horizontal, 18)
        .padding(.vertical, 14)
        .modifier(GlassyPillBackground())
        .padding(.horizontal, 24)
        .padding(.top, 14)
        // The pill is placed at the bottom safe-area edge by ContentView's
        // `.overlay(alignment: .bottom)`, so its gap to the physical screen
        // bottom is the home-indicator inset. Push down by a third of that
        // inset to shave a third off the gap — sits a little lower.
        .padding(.bottom, -Self.bottomSafeInset / 3)
    }

    /// Bottom safe-area inset of the key window (the home-indicator strip),
    /// read once per launch — constant per device on iPhone.
    private static let bottomSafeInset: CGFloat = {
        UIApplication.shared.connectedScenes
            .compactMap { $0 as? UIWindowScene }
            .flatMap(\.windows)
            .first { $0.isKeyWindow }?
            .safeAreaInsets.bottom ?? 0
    }()
}

// Translucent capsule background for the search pill: the Liquid-Glass
// `.glassEffect` modifier from iOS 27, `.ultraThinMaterial` before it.
// Not the glass on iOS 26: there it hands a tap on anything in the pill
// but the text field through to the listing behind it, so the clear "x"
// never fired (Order independence #30, SearchDayUITests on iOS 26.5).
// iOS 27 delivers the tap to the pill.
private struct GlassyPillBackground: ViewModifier {
    @ViewBuilder
    func body(content: Content) -> some View {
        if #available(iOS 27.0, macOS 27.0, *) {
            content.glassEffect(in: Capsule())
        } else {
            content.background(.ultraThinMaterial, in: Capsule())
        }
    }
}
