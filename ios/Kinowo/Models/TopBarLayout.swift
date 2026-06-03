#if canImport(CoreGraphics)
import CoreGraphics
#else
// `CoreGraphics` isn't on Linux, but Foundation re-exports `CGFloat`
// there — enough for this pure-logic type to build under `swift test` CI.
import Foundation
#endif

/// Decides, from the viewport width, where the search field lives.
///
/// On wide screens — iPads and landscape phones — there is room to sit
/// search on the top bar, after the "Wszystkie" date pill and before the
/// Filtry button. On narrow screens (portrait phones) there isn't, so it
/// stays as the floating pill at the bottom of the grid.
///
/// The threshold is shared verbatim with the Android app's `TopBarLayout`.
enum TopBarLayout {
    /// Viewport width (points) at or above which search moves inline. 600
    /// cleanly separates portrait phones (≤ ~440pt) from everything
    /// wider — landscape phones and iPads in either orientation.
    static let wideThreshold: CGFloat = 600

    static func searchInline(width: CGFloat) -> Bool { width >= wideThreshold }

    /// Whether all date pills should render at one uniform width.
    ///
    /// Equal width forces every pill to at least the widest label's intrinsic
    /// width (anything narrower clips that label), so a uniform row is only
    /// safe when the available width can hold `count` copies of the widest
    /// pill plus the `count - 1` gaps between them. When it can't, the pills
    /// fall back to their own intrinsic widths so every label still fits —
    /// fitting all the text takes priority over a uniform row.
    ///
    /// `intrinsicWidths` are the pills' natural (text + horizontal padding)
    /// widths; the caller measures them. Returns `false` for an empty set.
    static func datePillsEqualWidth(
        available: CGFloat,
        intrinsicWidths: [CGFloat],
        spacing: CGFloat
    ) -> Bool {
        guard let widest = intrinsicWidths.max() else { return false }
        let count = CGFloat(intrinsicWidths.count)
        return available >= count * widest + (count - 1) * spacing
    }
}
