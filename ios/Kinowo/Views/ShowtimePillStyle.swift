import SwiftUI

/// Live-tunable rendering parameters for a showtime pill, read by
/// `ShowtimeBadge` / `ShowingsView` from the environment.
///
/// Production never injects one, so the defaults below — which mirror
/// `ShowtimePillMetrics` (and the badge's hard-coded `.regular` / `.medium`
/// weights) exactly — reproduce the shipping pill byte for byte. The non-prod
/// `ShowtimeTuningScreen` swaps in an edited copy to preview size / weight /
/// padding / spacing changes against the real `FilmCardView` before we bake new
/// values into the constants. Keep these defaults in lockstep with
/// `ShowtimePillMetrics` and the weights in `ShowtimeBadge`.
struct ShowtimePillStyle: Equatable {
    var timeFontSize: CGFloat = ShowtimePillMetrics.timeFontSize
    var timeWeight: Font.Weight = .medium
    var formatFontSize: CGFloat = ShowtimePillMetrics.formatFontSize
    var formatWeight: Font.Weight = .medium
    /// Per-side horizontal padding inside the pill.
    var horizontalInset: CGFloat = ShowtimePillMetrics.horizontalInset
    /// Per-side vertical padding inside the pill.
    var verticalInset: CGFloat = ShowtimePillMetrics.verticalInset
    /// Gap between the time and the format tag.
    var internalGap: CGFloat = ShowtimePillMetrics.internalGap
    /// Gap between adjacent pills in the flow row.
    var interPillGap: CGFloat = ShowtimePillMetrics.interPillGap
}

private struct ShowtimePillStyleKey: EnvironmentKey {
    static let defaultValue = ShowtimePillStyle()
}

extension EnvironmentValues {
    var showtimePillStyle: ShowtimePillStyle {
        get { self[ShowtimePillStyleKey.self] }
        set { self[ShowtimePillStyleKey.self] = newValue }
    }
}
