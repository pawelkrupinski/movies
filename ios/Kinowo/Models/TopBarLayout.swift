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

    /// Whether a date-filter pill expands to share the leftover row width.
    /// The three short pills (Dziś / Jutro / 7 dni) spread to fill the row;
    /// Wszystkie (`.anytime`) always keeps its intrinsic 9-character width so
    /// its label is never clipped — including on iPad portrait, where the
    /// inline search field also competes for the row.
    static func datePillExpands(_ filter: DateFilter) -> Bool { filter != .anytime }
}
