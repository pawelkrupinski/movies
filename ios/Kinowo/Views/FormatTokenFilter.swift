import Foundation

/// Which of a card's showtime-format tokens a pill may drop.
///
/// A pill repeating what every showtime on the card shares reads as noise and
/// costs width the two-per-row layout does not have: six pills all saying `2D`
/// tell you nothing, and neither do six all saying `NAP` when the film screens
/// no other way. What survives is what actually separates one slot from the
/// next.
///
/// The comparison spans the WHOLE card — every cinema, every day — because that
/// is the span a reader compares across. A film Multikino screens dubbed and
/// Helios subtitled is mixed even though neither cinema is, and both keep their
/// tag; a film that is dubbed everywhere shows bare times. On the Kina tab each
/// card already holds one cinema, so the same rule reads as per-cinema there.
/// Same rule as the web's `FilmFormat` and Android's `FormatTokenFilter`.
///
/// Lives here rather than in `ShowingsView` because SwiftUI files are excluded
/// from the `swift test` target — the rule is testable here and would not be
/// there.
enum FormatTokenFilter {

    /// The tokens that say a slot's dimension. IMAX, 4DX, ScreenX and the rest
    /// are projection systems the vocabulary pairs with either (`IMAX 3D`,
    /// `4DE 3D`), not dimensions of their own.
    static let dimensionTokens: Set<String> = ["2D", "3D"]

    /// The dimension of a slot that names none.
    static let impliedDimension = "2D"

    /// What a pill on this card may drop: the tokens EVERY showtime across
    /// `days` carries, whatever they name. A showtime naming no dimension
    /// counts as `impliedDimension` (Kino Rialto writes `DUB` where Helios
    /// writes `2D DUB`); a format-less one sits out. Held to the vectors the
    /// web and Android read too, `test/resources/format-token-strip.json`.
    static func tokensToStrip(_ days: [DayShowings]) -> Set<String> {
        let tokenSets = days
            .flatMap(\.cinemas)
            .flatMap(\.showtimes)
            .map { Set($0.format.split(separator: " ").map(String.init)) }
            .filter { !$0.isEmpty }
            .map { $0.isDisjoint(with: dimensionTokens) ? $0.union([impliedDimension]) : $0 }
        guard let first = tokenSets.first else { return [] }
        return tokenSets.dropFirst().reduce(first) { $0.intersection($1) }
    }

    static func filter(_ format: String, removing common: Set<String>) -> String {
        guard !common.isEmpty else { return format }
        return format.split(separator: " ")
            .filter { !common.contains(String($0)) }
            .joined(separator: " ")
    }
}
