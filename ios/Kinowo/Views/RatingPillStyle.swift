import SwiftUI

/// Live-tunable rendering parameters for the rating pills, read by
/// `RatingBadgesView` from the environment.
///
/// Production never injects one, so the defaults below — which mirror
/// `RatingBadgeMetrics` plus the literals baked into `RatingBadgesView` — render
/// the shipping pill unchanged. The non-prod `ShowtimeTuningScreen` swaps in an
/// edited copy to preview the rating row. Unlike Android, iOS rating sizes do
/// NOT scale with viewport width — they're flat on every phone. Mirrors the
/// showtime `ShowtimePillStyle`.
struct RatingPillStyle: Equatable {
    /// The colored label tab ("IMDb"/"FW"/"RT").
    var labelFontSize: CGFloat = RatingBadgeMetrics.labelFontSize
    var labelWeight: Font.Weight = .heavy
    /// The score value (and Metacritic's label-less solid pill).
    var valueFontSize: CGFloat = RatingBadgeMetrics.valueFontSize
    var valueWeight: Font.Weight = .semibold
    /// Metacritic's solid pill weight (no label tab; heavier than the value tab).
    var solidWeight: Font.Weight = .heavy
    /// Per-side horizontal padding — the value tab gets one more point than the
    /// label tab (the number earns a touch more room).
    var labelHInset: CGFloat = 4
    var valueHInset: CGFloat = 5
    /// Per-side vertical padding, shared by both tabs.
    var vInset: CGFloat = 2
    /// Outer corner radius of the pill.
    var cornerRadius: CGFloat = 3
    /// Gap between adjacent pills in the flow row.
    var interPillGap: CGFloat = 4
}

private struct RatingPillStyleKey: EnvironmentKey {
    static let defaultValue = RatingPillStyle()
}

extension EnvironmentValues {
    var ratingPillStyle: RatingPillStyle {
        get { self[RatingPillStyleKey.self] }
        set { self[RatingPillStyleKey.self] = newValue }
    }
}
