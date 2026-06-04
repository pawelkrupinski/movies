import SwiftUI

/// Live-tunable fonts and vertical gaps of the per-film detail screen
/// (`FilmDetailView`), read from the environment.
///
/// Production never injects one, so the defaults below — which mirror the
/// literals `FilmDetailView` used before this struct existed — reproduce the
/// shipping detail layout point for point. The non-prod tuning pager (Film
/// page) swaps in an edited copy to preview typography / spacing changes
/// against the real detail screen. Keep these defaults in lockstep with the
/// literals the view would otherwise hard-code.
struct FilmDetailStyle: Equatable {
    /// Outer `VStack(spacing:)` between header / meta blocks / trailers /
    /// showings.
    var sectionSpacing: CGFloat = 16
    /// Header's right-column `VStack(spacing:)` — title to meta to ratings.
    var headerColumnSpacing: CGFloat = 8
    var titleFontSize: CGFloat = 22
    var titleWeight: Font.Weight = .bold
    /// Italic original (production-language) title under the main title.
    var originalTitleFontSize: CGFloat = 15
    /// `VStack(spacing:)` between the Opis / Reżyseria / Obsada blocks.
    var metaBlockSpacing: CGFloat = 12
    /// Uppercased meta-block label (e.g. "OPIS").
    var metaLabelFontSize: CGFloat = 11
    /// Meta-block body text (synopsis, director list, …).
    var metaValueFontSize: CGFloat = 14
    /// Gap between a meta-block label and its value.
    var metaLabelToValue: CGFloat = 4
    /// "Seanse" section header.
    var showingsHeaderFontSize: CGFloat = 18
}

private struct FilmDetailStyleKey: EnvironmentKey {
    static let defaultValue = FilmDetailStyle()
}

extension EnvironmentValues {
    var filmDetailStyle: FilmDetailStyle {
        get { self[FilmDetailStyleKey.self] }
        set { self[FilmDetailStyleKey.self] = newValue }
    }
}
