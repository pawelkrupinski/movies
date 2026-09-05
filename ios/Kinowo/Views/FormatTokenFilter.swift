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

    /// What a pill on this card may drop: the tokens EVERY showtime across
    /// `days` carries, whatever they name.
    static func tokensToStrip(_ days: [DayShowings]) -> Set<String> {
        let tokenSets = days
            .flatMap(\.cinemas)
            .flatMap(\.showtimes)
            .map { Set($0.format.split(separator: " ").map(String.init)) }
            .filter { !$0.isEmpty }
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
