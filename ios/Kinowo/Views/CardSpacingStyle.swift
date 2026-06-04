import SwiftUI

/// Live-tunable vertical gaps inside a `FilmCardView`, read by the card and
/// `ShowingsView` from the environment.
///
/// Production never injects one, so the defaults below — which mirror the
/// hard-coded spacings the card used before this struct existed — reproduce the
/// shipping layout point for point. The non-prod `ShowtimeTuningScreen` swaps in
/// an edited copy to preview gap changes against the real `FilmCardView` before
/// we bake new values into the layout. Keep these defaults in lockstep with the
/// literals the card would otherwise hard-code.
///
/// We expose only the gaps that already exist as *distinct* spacing properties.
/// `sectionSpacing` is deliberately one lever for the whole title→meta→ratings
/// →showings stack (the card uses a single `VStack(spacing:)` there), matching
/// that shared spacing rather than splitting it.
struct CardSpacingStyle: Equatable {
    /// `FilmCardView`'s inner `VStack(spacing:)` — the shared gap between
    /// title, meta pills, ratings, and the showings block.
    var sectionSpacing: CGFloat = 8
    /// Extra clearance below the ratings flow (`RatingBadgesView.padding(.bottom)`),
    /// on top of `sectionSpacing`, so a wrapped last rating row can't crowd the
    /// showings block.
    var ratingsBottom: CGFloat = 14
    /// `ShowingsView`'s outer `VStack(spacing:)` — between day blocks, and
    /// between cinema blocks within a day.
    var showingsBlock: CGFloat = 6
    /// Gap below a day label, above its first cinema name / pill row.
    /// Independent of `showingsBlock` so the day↔cinema distance can be dialled
    /// separately (the Android twin is `CardSpacingStyle.dayToCinema`). Default
    /// equals the old shared `showingsBlock` gap, so production is unchanged.
    var dayToCinema: CGFloat = 6
    /// Padding above each day label.
    var dayLabelTop: CGFloat = 4
    /// `ShowingsView`'s per-cinema inner `VStack(spacing:)` — cinema label to
    /// its pill row.
    var cinemaToPills: CGFloat = 4
    /// `FlowLayout(lineSpacing:)` between wrapped rows of showtime pills.
    var pillRowSpacing: CGFloat = 4
}

private struct CardSpacingStyleKey: EnvironmentKey {
    static let defaultValue = CardSpacingStyle()
}

extension EnvironmentValues {
    var cardSpacingStyle: CardSpacingStyle {
        get { self[CardSpacingStyleKey.self] }
        set { self[CardSpacingStyleKey.self] = newValue }
    }
}
