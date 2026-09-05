import Foundation

/// Which of a cinema's showtime-format tokens a pill may drop.
///
/// A pill repeating what every showtime at that cinema shares reads as noise and
/// costs width the two-per-row layout does not have, so `ShowingsView` strips
/// `tokensToStrip` from each badge: six pills all saying `2D` tell you nothing,
/// and neither do six all saying `NAP` when the cinema screens the film no other
/// way. What survives is what actually distinguishes one slot from the next.
/// Same rule as the web's `CinemaFormat` and Android's `FormatTokenFilter`.
///
/// Lives here rather than in `ShowingsView` because SwiftUI files are excluded
/// from the `swift test` target — the rule is testable here and would not be
/// there.
enum FormatTokenFilter {

    /// What a pill at `cinema` may drop: the tokens EVERY showtime there
    /// carries, whatever they name. One that is on every pill separates no slot
    /// from any other, so a language version goes the same way a screen format
    /// does — the pills that keep a tag are the ones a visitor is choosing
    /// between.
    static func tokensToStrip(_ cinema: CinemaShowings) -> Set<String> {
        let tokenSets = cinema.showtimes
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
