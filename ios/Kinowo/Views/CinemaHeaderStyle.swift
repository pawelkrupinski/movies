import SwiftUI

/// Live-tunable look of the Kina tab's per-cinema section header and the gaps
/// around it, read by `CinemaSectionedGridView` from the environment.
///
/// Production never injects one, so the defaults below — which mirror the
/// literals `CinemaSectionedGridView` used before this struct existed —
/// reproduce the shipping Kina layout point for point. The non-prod tuning
/// pager (Kina page) swaps in an edited copy to preview header / spacing
/// changes against the real cinema-sectioned grid. Keep these defaults in
/// lockstep with the literals the grid would otherwise hard-code.
struct CinemaHeaderStyle: Equatable {
    /// Cinema section title font size (web `.cinema-section-title`).
    var fontSize: CGFloat = 15
    var fontWeight: Font.Weight = .semibold
    /// Thickness of the #3a3a6e underline beneath the title.
    var underlineThickness: CGFloat = 1
    /// Gap between the title text and its underline.
    var titleBottomPadding: CGFloat = 6
    /// `LazyVStack(spacing:)` between adjacent cinema sections.
    var sectionSpacing: CGFloat = 20
    /// Inner `VStack(spacing:)` between a section header and its film grid.
    var headerToGrid: CGFloat = 12
}

private struct CinemaHeaderStyleKey: EnvironmentKey {
    static let defaultValue = CinemaHeaderStyle()
}

extension EnvironmentValues {
    var cinemaHeaderStyle: CinemaHeaderStyle {
        get { self[CinemaHeaderStyleKey.self] }
        set { self[CinemaHeaderStyleKey.self] = newValue }
    }
}
