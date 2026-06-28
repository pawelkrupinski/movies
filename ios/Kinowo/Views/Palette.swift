import SwiftUI

/// Shared accent + surface colours for the listing / detail views, factored
/// out of the individual `View`s so a single definition is the source of
/// truth. Each value is byte-identical to the inline `Color(red:green:blue:)`
/// it replaced; the hex in the trailing comment mirrors the web's CSS.
extension Color {
    /// Dark panel fill (#2a2a3e) — the rating value-tab background and the
    /// detail screen's "Brak plakatu" / loading poster placeholder.
    static let kinowoPanel = Color(red: 0.165, green: 0.165, blue: 0.243)

    /// Cinema-link / section-header / trailer-button accent (#aad4ff).
    static let kinowoLinkAccent = Color(red: 0.667, green: 0.831, blue: 1.0)

    /// Filled-button / section-underline background (#3a3a6e).
    static let kinowoButtonFill = Color(red: 0.227, green: 0.227, blue: 0.431)

    /// Cinema label + "więcej seansów" pill text on the listing card.
    static let kinowoCinemaLabel = Color(red: 0.40, green: 0.67, blue: 0.87)
}
