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

extension EnvironmentValues {
    @Entry var showtimePillStyle: ShowtimePillStyle = ShowtimePillStyle()
    /// While a search shows every day, the days it brought in, drawn lighter (`SearchDay`);
    /// nil everywhere else.
    @Entry var outsideDays: OutsideDays? = nil
}
