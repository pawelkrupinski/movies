#if canImport(CoreGraphics)
import CoreGraphics
#else
// `CoreGraphics` isn't on Linux, but Foundation re-exports `CGFloat`
// there — enough for this pure-logic type to build under `swift test` CI.
import Foundation
#endif

/// Whether the film grid runs under the window's horizontal safe-area insets,
/// and so how wide the listing is. The top bar never does: it stays inside the
/// safe area, clear of a landscape phone's Dynamic Island.
///
/// A landscape iPhone has equal leading and trailing insets — the sensor
/// housing on one side, rounded corners on both — and the grid deliberately
/// runs full-width under them. The iPhone Duo instead keeps its status bar in
/// a column down the right edge: a trailing-only inset (84 pt, folded in
/// portrait and unfolded in landscape) holding live system UI, so the grid
/// stays out of it as it stays out from under the top status bar. So the grid
/// bleeds into the horizontal insets only when they are symmetric.
///
/// A one-sided inset can't be bled into cleanly anyway. Ignoring it made
/// SwiftUI lay the bar-and-grid stack out wider than the window, which dragged
/// the grid 57 pt off the folded Duo's left edge. And however the paged grid is
/// widened over the unfolded Duo's landscape column (ignoring it on the
/// TabView, its pages or a container; negative padding), its UIKit pager puts
/// every page half the inset off the leading edge: x = -42...909 in a 951 pt
/// window (2026-11-07).
///
/// `safeWidth` is the width inside the safe area; `leading` / `trailing` are
/// the window's horizontal safe-area insets.
struct ListingSafeArea: Equatable {
    var safeWidth: CGFloat
    var leading: CGFloat
    var trailing: CGFloat

    /// Equal insets (none at all, in portrait on a regular iPhone, included).
    var bleedsIntoHorizontalInsets: Bool { abs(leading - trailing) < 1 }

    /// The listing's laid-out width: the whole window when it bleeds into the
    /// insets, the safe area when it doesn't.
    var listingWidth: CGFloat {
        bleedsIntoHorizontalInsets ? safeWidth + leading + trailing : safeWidth
    }
}
