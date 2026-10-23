#if canImport(CoreGraphics)
import CoreGraphics
#else
// `CoreGraphics` isn't on Linux, but Foundation re-exports `CGFloat`
// there — enough for this pure-logic type to build under `swift test` CI.
import Foundation
#endif

/// Whether the film listing runs under the window's horizontal safe-area
/// insets, and so how wide the listing is.
///
/// A landscape iPhone has equal leading and trailing insets — the sensor
/// housing on one side, rounded corners on both — and the grid deliberately
/// runs full-width under them. The iPhone Duo instead keeps its status bar in
/// a column down the right edge: a trailing-only inset, in portrait, holding
/// live system UI, so the listing stays out of it as it stays out from under
/// the top status bar. So the listing bleeds into the horizontal insets only
/// when they are symmetric.
///
/// It must not instead ignore them and pad the one-sided part back: SwiftUI
/// lays a view that ignores an asymmetric inset out wider than the window,
/// which dragged the grid 57 pt off the Duo's left edge.
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
