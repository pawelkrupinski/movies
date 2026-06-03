import Foundation

/// Font sizes for the rating pills, factored out of the SwiftUI
/// `RatingBadgesView` so they live in `KinowoCore` (Foundation-only, no
/// SwiftUI) and stay unit-testable via `swift test` — the view itself is
/// excluded from that module. `RatingBadgesView` renders at exactly these
/// sizes; the on-screen result is pixel-verified by Android's twin test,
/// so here we just pin the shared source the view reads from.
///
/// The pills mirror the web's two-tone shape: a small colored label tab
/// ("IMDb", "FW", "RT") beside a larger value tab with the score. The
/// value reads at 11 pt — matching the showtime chip's time
/// (`ShowtimePillMetrics.timeFontSize`) and Android's 11sp pill — with
/// the label one point smaller so the tab stays subordinate to the score.
/// Metacritic's single solid pill is value-sized.
enum RatingBadgeMetrics {
    /// The colored label tab ("IMDb"/"FW"/"RT") — one point below the
    /// value so it reads as a subordinate tag.
    static let labelFontSize: CGFloat = 10
    /// The score value, and Metacritic's label-less solid pill.
    static let valueFontSize: CGFloat = 11
}
