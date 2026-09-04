import Foundation

/// Which of a cinema's showtime-format tokens a pill may drop, and which the
/// cinema's own label has to say instead.
///
/// A pill repeating what every showtime at that cinema shares reads as noise and
/// costs width the two-per-row layout does not have, so `ShowingsView` strips
/// `commonTokens` from each badge. That is right for a screen format — six pills
/// all saying `2D` tell you nothing — but it was silently swallowing the LANGUAGE
/// VERSION too: a film a cinema screens only dubbed has every slot tagged
/// `2D DUB`, the intersection ate `DUB`, and the card could no longer tell napisy
/// from dubbing. Same defect, and same fix, as the web's `CinemaFormat`.
enum FormatTokenFilter {

    /// The tokens EVERY showtime at `cinema` carries.
    static func commonTokens(_ cinema: CinemaShowings) -> Set<String> {
        let tokenSets = cinema.showtimes
            .map { Set($0.format.split(separator: " ").map(String.init)) }
            .filter { !$0.isEmpty }
        guard let first = tokenSets.first else { return [] }
        return tokenSets.dropFirst().reduce(first) { $0.intersection($1) }
    }

    /// The part of `commonTokens` that names a language version, in the source's
    /// own order — what the cinema label says once instead of every pill saying
    /// it. Empty when the version differs between showtimes, which is exactly
    /// where a per-pill badge is the only thing that can carry it.
    static func commonVersion(_ cinema: CinemaShowings) -> [String] {
        let common = commonTokens(cinema)
        guard let first = cinema.showtimes.first(where: { !$0.format.isEmpty }) else { return [] }
        return first.format.split(separator: " ").map(String.init)
            .filter { common.contains($0) && isLanguageVersion($0) }
    }

    /// What a pill at `cinema` may drop: everything common to the cinema when a
    /// cinema LABEL is on screen to carry the version, and everything common
    /// EXCEPT the version when it is not.
    ///
    /// `hasLabel` is `ShowingsView.showCinemaHeaders`. The Kina tab's per-cinema
    /// section names the cinema but is shared by films with different versions,
    /// so there the pill is the only place the version can live. A pill that
    /// keeps it is the `"2D DUB"` shape `ShowtimePillMetricsTests` holds
    /// two-per-row against, so this never widens past the guarantee.
    ///
    /// Lives here rather than in `ShowingsView` because SwiftUI files are
    /// excluded from the `swift test` target — the rule is testable here and
    /// would not be there.
    static func tokensToStrip(_ cinema: CinemaShowings, hasLabel: Bool) -> Set<String> {
        let common = commonTokens(cinema)
        guard !hasLabel else { return common }
        return common.subtracting(commonVersion(cinema))
    }

    static func filter(_ format: String, removing common: Set<String>) -> String {
        guard !common.isEmpty else { return format }
        return format.split(separator: " ")
            .filter { !common.contains(String($0)) }
            .joined(separator: " ")
    }

    /// Does `token` name a language version — what you will hear and read —
    /// rather than a screen format or an accessibility feature?
    ///
    /// Decided by EXCLUSION, because the version half of the vocabulary is the
    /// open one: it carries every market's own spelling (`NAP`/`DUB`/`LEK`/`ORG`,
    /// `SUB`/`LEC`, `VO`/`VOSE`/`VOSI`/`DOB`/`CAT`, `OV`/`OmU`/`OmeU`/`DF`) AND
    /// the audio language itself wherever a source names one — at a UK multiplex
    /// `HINDI` is the whole difference between two screenings of the same film,
    /// and no fixed list keeps up with that. The screen-format and accessibility
    /// halves are closed, so naming those and taking the complement stays in step
    /// with the server's `ScreeningTokens`, whose three categories partition the
    /// same vocabulary.
    static func isLanguageVersion(_ token: String) -> Bool {
        !nonVersionTokens.contains(token.uppercased())
    }

    /// Screen format + per-screening accessibility — `ScreeningTokens`'s first
    /// and third categories, which are the closed ones.
    private static let nonVersionTokens: Set<String> = [
        "2D", "3D", "IMAX", "4DX", "4DE", "SCREENX", "ISENSE", "PLF", "EPIC",
        "INFINITY", "DBOX", "LASER", "HDR", "ATMOS", "DOLBY", "4K",
        "70MM", "35MM", "16MM", "VIP", "PREMIUM",
        "AD", "OC",
    ]
}
