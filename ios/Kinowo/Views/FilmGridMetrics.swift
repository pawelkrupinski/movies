#if canImport(CoreGraphics)
import CoreGraphics
import Foundation

/// Column math for the repertoire `LazyVGrid`, shared by both grids
/// (`FilmGridView` and `CinemaSectionedGridView`).
///
/// The grid used to hand the layout to `GridItem(.adaptive(minimum: 160,
/// maximum: 220))`, which picks the column count from the *enclosing scroll
/// view's* width. Inside a paged `TabView` (a `UIPageViewController`) that
/// width can lag a portrait⇄landscape rotation: the user rotates to landscape
/// and back, and the grid stays stuck at the landscape column count —
/// "zoomed-in", more than two columns in portrait, no way to undo short of
/// relaunching. (Confirmed a known SwiftUI paged-TabView rotation issue.)
///
/// Deriving the count here, from the authoritative *window* width (measured
/// outside the TabView), makes portrait reliably two columns regardless of
/// what the stale scroll view reports. Pure arithmetic in `KinowoCore` —
/// CoreGraphics, no UIKit — so it's unit-testable via `swift test`
/// (`FilmGridMetricsTests`).
enum FilmGridMetrics {
    /// Minimum / maximum width of a single poster column — the bounds the old
    /// `.adaptive(minimum: 160, maximum: 220)` used, kept so the rendered
    /// landscape layout is byte-for-byte what it was before the fix.
    static let minColumnWidth: CGFloat = 160
    static let maxColumnWidth: CGFloat = 220
    /// Spacing between columns (the `LazyVGrid` / `GridItem` spacing).
    static let columnSpacing: CGFloat = 12
    /// Horizontal padding on each side of the grid (`FilmGridView`'s
    /// `.padding(.horizontal, 12)`).
    static let horizontalPadding: CGFloat = 12

    /// How many poster columns fit at the given container width — the same
    /// count `.adaptive(minimum: minColumnWidth)` would choose, computed up
    /// front instead of read back from a possibly-stale scroll view. Never
    /// fewer than one (a column-less grid renders nothing).
    static func columnCount(forWidth width: CGFloat) -> Int {
        let available = width - 2 * horizontalPadding
        let perColumn = minColumnWidth + columnSpacing
        let fit = (available + columnSpacing) / perColumn
        return max(1, Int(fit.rounded(.down)))
    }
}
#endif
