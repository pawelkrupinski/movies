import SwiftUI
import UIKit

// Top safe-area inset: 🎬 brand mark + four date-filter pills + (on wide
// screens) the search field + Filtry button on the right. Built as a plain
// HStack instead of SwiftUI `ToolbarItem`s because the native nav bar
// clips its contents to a few dozen points of width.
//
// `searchInline` (decided by `TopBarLayout` from the viewport width) moves
// search onto this row, between the pills and Filtry; on narrow screens it
// stays a floating bottom pill (`SearchBar`) and this row holds only the
// pills. The search field is capped at a sensible max width so it doesn't
// sprawl across a wide screen; the date pills always spread to fill the
// leftover row width.
//
// Every numeric size below is multiplied by `scale` (`TopBarLayout.viewportScale`, viewportWidth / 393)
// (iPhone 17 reference width). Smaller phones get a proportionally
// smaller row, larger phones get a taller / fontier one — the whole
// bar tracks the device's viewport rather than rendering one fixed
// size that has to fit the smallest screen.
struct TopBar: View {
    @Binding var dateFilter: DateFilter
    @Binding var search: String
    @FocusState.Binding var searchFocused: Bool
    let searchInline: Bool
    let filtersActive: Bool
    let onTapFilters: () -> Void
    @Environment(\.listingViewportWidth) private var viewportWidth

    var body: some View {
        let scale = TopBarLayout.viewportScale(width: viewportWidth)
        // `spacing: 6 * scale` matches the inter-pill gap inside
        // `DatePillsRow`, so every gap on the bar — 🎬 → pills,
        // pills → search/Filtry — reads as the same width as the
        // gaps between the four pills.
        TopBarRow(spacing: 6 * scale, searchCap: 240 * scale) {
            Text(verbatim: "🎬")
                .font(.system(size: 24 * scale))
            DatePillsRow(dateFilter: $dateFilter, scale: scale)
                .frame(maxWidth: .infinity)
                // A fresh row when search moves inline: its measured width (and
                // so equal-width mode) must not carry over from the wider slot it
                // had without the field, which truncated "Wszystkie".
                .id(searchInline)
            if searchInline {
                // `TopBarRow` sizes it: capped so it stays a comfortable
                // type-into width rather than eating the whole row, and giving
                // way to the pills when the row is tight.
                InlineSearchField(search: $search, focused: $searchFocused, scale: scale)
            }
            Button(action: onTapFilters) {
                // `line.3.horizontal.decrease.circle` is the funnel-in-
                // circle SF Symbol the iOS app shipped with — the circle
                // is baked into the glyph itself, no painted background.
                // `.fill` variant swaps in when any filter axis is
                // active so the icon tells you at a glance.
                //
                // Font size 30pt at scale 1.0 visually matches a pill's
                // total height (14pt text + 2×7pt v-padding ≈ 28pt) — the
                // SF Symbol's glyph height is a hair smaller than its font
                // size, so 30pt lands the circle on the same baseline as
                // the pill capsules either side.
                Image(systemName: filtersActive
                      ? "line.3.horizontal.decrease.circle.fill"
                      : "line.3.horizontal.decrease.circle")
                    .font(.system(size: 30 * scale))
                    .foregroundStyle(filtersActive ? Color.accentColor : .primary)
            }
            .buttonStyle(BounceButtonStyle())
            // The Image has no text label so XCUITest can't find the
            // button by its accessibility label — give it both a label
            // and an identifier. The label is also useful for
            // VoiceOver users in production.
            .accessibilityLabel("filtersheet.title")
            .accessibilityIdentifier(A11y.TopBar.filtryButton)
            // The lit state, for VoiceOver ("selected") and the UI tests.
            .accessibilityAddTraits(filtersActive ? .isSelected : [])
        }
        .padding(.horizontal, 10 * scale)
        // Tiny top padding so the pills hug the status bar; the
        // safeAreaInset already reserves the strip above. A small bottom
        // padding keeps a slim breathing buffer between the bar and the
        // grid scrolling beneath it.
        .padding(.top, 2 * scale)
        .padding(.bottom, 4 * scale)
        // No background here on purpose. The bar's backing is the opaque
        // app-background drawn in `ContentView` (`.background(Color(
        // .systemBackground)…)`); the grid meets it with a hard edge — no
        // frosted material and no gradient fade.
        // Zero-visual automation anchor at the bar's true bottom edge. The
        // pills / Filtry button sit on the row above, inside the bottom
        // padding, so they can't stand in for the bar bottom in UI tests.
        .overlay(alignment: .bottom) {
            Color.clear
                .frame(maxWidth: .infinity, maxHeight: 1)
                .accessibilityElement()
                .accessibilityIdentifier(A11y.TopBar.bottomEdge)
        }
    }
}

/// The top bar's row: 🎬, the date pills, the inline search field when there
/// is one, and Filtry, left to right with one gap. 🎬 and Filtry take their
/// own width, the search field what `TopBarLayout.inlineSearchWidth` leaves it
/// beside the pills at their text-fitting width, and the pills the rest. An
/// HStack sized the field and the pills together, so the field kept its full
/// cap while the pills kept their text width, and the bar ran wider than the
/// unfolded iPhone Duo's 669 pt portrait screen.
struct TopBarRow: Layout {
    let spacing: CGFloat
    let searchCap: CGFloat

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        let ideals = subviews.map { $0.sizeThatFits(.unspecified) }
        let idealWidth = ideals.map(\.width).reduce(0, +) + gaps(subviews)
        return CGSize(width: proposal.width ?? idealWidth,
                      height: ideals.map(\.height).max() ?? 0)
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        var x = bounds.minX
        for (subview, width) in zip(subviews, widths(rowWidth: bounds.width, subviews: subviews)) {
            subview.place(at: CGPoint(x: x, y: bounds.midY), anchor: .leading,
                          proposal: ProposedViewSize(width: width, height: nil))
            x += width + spacing
        }
    }

    private func gaps(_ subviews: Subviews) -> CGFloat {
        spacing * CGFloat(max(0, subviews.count - 1))
    }

    /// 🎬, pills, [search,] Filtry.
    private func widths(rowWidth: CGFloat, subviews: Subviews) -> [CGFloat] {
        let ideals = subviews.map { $0.sizeThatFits(.unspecified).width }
        guard ideals.count >= 3 else { return ideals }
        let fixed = ideals[0] + ideals[ideals.count - 1]
        let search = ideals.count == 4
            ? TopBarLayout.inlineSearchWidth(rowWidth: rowWidth, fixedWidths: fixed,
                                             pillsWidth: ideals[1],
                                             gaps: gaps(subviews), cap: searchCap)
            : 0
        let pills = max(0, rowWidth - fixed - gaps(subviews) - search)
        return ideals.count == 4
            ? [ideals[0], pills, search, ideals[3]]
            : [ideals[0], pills, ideals[2]]
    }
}

// The four date pills (today / tomorrow / 7 days / anytime). When the row is
// wide enough to hold four copies of the widest pill plus the gaps, all
// four render at one uniform width — `.frame(maxWidth: .infinity)` splits the
// row equally. When it isn't, they drop to their intrinsic widths via
// `.fixedSize` so every label still fits — fitting all the text beats a uniform
// row. The choice is `TopBarLayout.datePillsEqualWidth`, fed the offered row
// width (measured below) and the pills' intrinsic widths.
//
// Inter-pill spacing is fixed at `6 * scale` to match the brand → pills and
// pills → Filtry gaps in `TopBar` — every gap on the bar reads as the same
// width.
struct DatePillsRow: View {
    @Binding var dateFilter: DateFilter
    let scale: CGFloat
    @Environment(\.locale) private var locale

    /// Width offered to the row, measured from the full-width frame below so it
    /// reflects the space available regardless of how the pills size within it.
    @State private var availableWidth: CGFloat = 0

    private var spacing: CGFloat { 6 * scale }
    private var fontSize: CGFloat { 14 * scale }
    private var horizontalPadding: CGFloat { 12 * scale }

    /// Each pill's natural width: its label's rendered text width plus the
    /// horizontal padding on both sides.
    private var intrinsicWidths: [CGFloat] {
        let font = UIFont.systemFont(ofSize: fontSize, weight: .medium)
        return DateFilter.presets.map { preset in
            let textWidth = (preset.label(locale: locale) as NSString)
                .size(withAttributes: [.font: font]).width
            return textWidth.rounded(.up) + 2 * horizontalPadding
        }
    }

    private var equalWidth: Bool {
        TopBarLayout.datePillsEqualWidth(
            available: availableWidth,
            intrinsicWidths: intrinsicWidths,
            spacing: spacing
        )
    }

    var body: some View {
        // Equal-width mode shares the row between four uniform pills through a
        // fixed gap. The fallback keeps each pill at its text-fitting width but
        // justifies them across the full row (space-between): `spacing: 0` plus
        // an expanding `Spacer` between pills distributes the slack as the
        // inter-pill gaps, so the row always fills its width — never centred or
        // leading-aligned with empty margins.
        HStack(spacing: equalWidth ? spacing : 0) {
            ForEach(Array(DateFilter.presets.enumerated()), id: \.element) { index, f in
                pill(f)
                if !equalWidth, index < DateFilter.presets.count - 1 {
                    Spacer(minLength: spacing)
                }
            }
        }
        // Fill the offered width so the measurement reflects the space the row
        // is given, not the width the pills happen to occupy.
        .frame(maxWidth: .infinity)
        .onGeometryChange(for: CGFloat.self, of: { $0.size.width }) { availableWidth = $0 }
    }

    private func pill(_ f: DateFilter) -> some View {
        let isHighlighted = dateFilter == f
        return Button {
            dateFilter = f
        } label: {
            Text(f.label(locale: locale))
                .font(.system(size: fontSize, weight: .medium))
                .lineLimit(1)
                .frame(maxWidth: equalWidth ? .infinity : nil)
                .padding(.horizontal, horizontalPadding)
                .padding(.vertical, 7 * scale)
                .background(
                    isHighlighted
                        ? Color.accentColor.opacity(0.85)
                        : Color.clear,
                    in: Capsule()
                )
                .foregroundColor(isHighlighted ? .white : .primary)
        }
        .buttonStyle(.plain)
        .fixedSize(horizontal: !equalWidth, vertical: false)
        .accessibilityIdentifier(Self.accessibilityId(for: f))
        .accessibilityAddTraits(isHighlighted ? .isSelected : [])
    }

    private static func accessibilityId(for f: DateFilter) -> String {
        switch f {
        case .today:    return A11y.TopBar.datePillToday
        case .tomorrow: return A11y.TopBar.datePillTomorrow
        case .week:     return A11y.TopBar.datePillWeek
        case .anytime:  return A11y.TopBar.datePillAnytime
        case .specific: return A11y.TopBar.datePillAnytime
        }
    }
}

// Tap feedback: spring-scale the label down on press and back on
// release. Used by the Filtry icon (and any future TopBar button) so
// taps feel like they registered without needing to wait for the
// sheet to slide in. (`.symbolEffect(.bounce)` animates only the symbol,
// on a value change, not the whole label while it is held down.)
struct BounceButtonStyle: ButtonStyle {
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .scaleEffect(configuration.isPressed ? 0.85 : 1.0)
            .animation(
                .spring(response: 0.25, dampingFraction: 0.55),
                value: configuration.isPressed
            )
    }
}
