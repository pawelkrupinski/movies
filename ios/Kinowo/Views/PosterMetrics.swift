import Foundation

/// Layout constants shared by the poster/card surfaces, factored out of the
/// SwiftUI views so a single definition is the source of truth (and the
/// values stay unit-testable via `swift test`, where the views themselves
/// are excluded). Each value is byte-identical to the inline literal it
/// replaced — `FilmCardView` and `FilmDetailView` render at exactly these.
enum PosterMetrics {
    /// Movie-poster aspect ratio (2:3). Drives the listing-card poster box
    /// and the detail screen's poster / no-poster placeholder.
    static let aspectRatio: CGFloat = 2.0 / 3.0
    /// Rounded-corner radius for the rounded media surfaces — the listing
    /// card, the detail poster, and the trailer embed.
    static let cornerRadius: CGFloat = 12
}
