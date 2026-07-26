import Foundation

/// A per-cinema external link shown on the detail screen ("Helios ↗",
/// "Multikino ↗"). Previously scraped from the `/film` page's
/// `<a class="cinema-link">` anchors; now derived from the listing
/// `Film`'s showings, which already carry each cinema's `cinemaURL`.
///
/// `label` is the cinema's name when the URL is that cinema's alone, and
/// the name the sharers have in common when several cinemas point at one
/// URL — see `cinemaLinks()` and `label(for:sharing:)`.
struct CinemaLink: Hashable {
    let label: String
    let url: URL

    /// What to call one link, given every cinema behind it.
    ///
    /// A cinema with a URL of its own keeps its own name. Several cinemas
    /// behind one URL are a chain linking to its film page, so they're
    /// named by what they share: `Multikino Wola Park` + `Multikino Złote
    /// Tarasy` → `Multikino`. Where the sharers have no name in common —
    /// the UK aggregator, which hands all 72 London cinemas the same
    /// flicks.co.uk page — the site is the only honest name left.
    static func label(for url: URL, sharing cinemas: [String]) -> String {
        if cinemas.count == 1 { return cinemas[0] }
        let common = commonWordPrefix(of: cinemas)
        return common.isEmpty ? siteName(of: url) : common
    }

    /// The longest run of leading whole words every name starts with, e.g.
    /// `["Cinema City Arkadia", "Cinema City Sadyba"]` → `Cinema City`.
    /// Whole words only: matching mid-word would coin names no cinema uses.
    private static func commonWordPrefix(of names: [String]) -> String {
        guard let first = names.first?.split(separator: " ") else { return "" }
        var shared = Array(first)
        for name in names.dropFirst() {
            let words = name.split(separator: " ")
            shared = Array(zip(shared, words).prefix { $0 == $1 }.map(\.0))
            if shared.isEmpty { break }
        }
        return shared.joined(separator: " ")
    }

    /// The site's name, read off the host: `www.flicks.co.uk` → `Flicks`.
    /// The last resort for a URL several unrelated cinemas share.
    static func siteName(of url: URL) -> String {
        guard let host = url.host else { return url.absoluteString }
        let name = host
            .replacingOccurrences(of: "^www\\.", with: "", options: .regularExpression)
            .split(separator: ".")
            .first
            .map(String.init) ?? host
        return name.prefix(1).uppercased() + name.dropFirst()
    }
}

extension Array where Element == DayShowings {
    /// One link per distinct URL across every day, sorted alphabetically by
    /// label. Replaces the scraped `FilmDetail.cinemaLinks`: the showings
    /// tree already names every cinema the film plays at and carries its
    /// `cinemaURL`, so no second fetch is needed.
    ///
    /// Deduping by URL, not by cinema name, is what keeps the row honest:
    /// a link is worth its own pill only if it goes somewhere of its own.
    /// A PL chain points all five of its venues at one film page, so those
    /// five pills were five ways to open the same page — they now collapse
    /// to "Multikino ↗". The UK aggregator takes it further and hands ALL
    /// 72 London cinemas the same flicks.co.uk page: 72 pills to one page,
    /// and — keyed by that shared URL — 72 SwiftUI rows with one identity,
    /// which drew the alphabetically-first cinema's name over and over.
    /// Every cinema stays individually linked in the showings list below.
    func cinemaLinks() -> [CinemaLink] {
        var cinemasByURL: [URL: [String]] = [:]
        var seenCinemas: Set<String> = []
        for day in self {
            for c in day.cinemas {
                guard let url = c.cinemaURL, seenCinemas.insert(c.cinema).inserted else { continue }
                cinemasByURL[url, default: []].append(c.cinema)
            }
        }
        return cinemasByURL
            .map { url, cinemas in
                CinemaLink(label: CinemaLink.label(for: url, sharing: cinemas), url: url)
            }
            // URL breaks ties: the grouping is a Dictionary, so two links that
            // share a label would otherwise swap places between runs.
            .sorted {
                let byLabel = $0.label.localizedCaseInsensitiveCompare($1.label)
                return byLabel == .orderedSame
                    ? $0.url.absoluteString < $1.url.absoluteString
                    : byLabel == .orderedAscending
            }
    }
}
