import Foundation

/// A per-cinema external link shown on the detail screen ("Helios ↗",
/// "Multikino ↗"). Previously scraped from the `/film` page's
/// `<a class="cinema-link">` anchors; now derived from the listing
/// `Film`'s showings, which already carry each cinema's `cinemaURL`.
struct CinemaLink: Hashable {
    let cinema: String
    let url: URL
}

extension Array where Element == DayShowings {
    /// Distinct cinema + URL pairs across every day, deduped by cinema
    /// name (first URL seen wins) and sorted alphabetically by cinema
    /// name. Replaces the scraped `FilmDetail.cinemaLinks`: the showings
    /// tree already names every cinema the film plays at and carries its
    /// `cinemaURL`, so no second fetch is needed.
    func cinemaLinks() -> [CinemaLink] {
        var byName: [String: URL] = [:]
        for day in self {
            for c in day.cinemas {
                guard let url = c.cinemaURL else { continue }
                if byName[c.cinema] == nil { byName[c.cinema] = url }
            }
        }
        return byName
            .map { CinemaLink(cinema: $0.key, url: $0.value) }
            .sorted { $0.cinema.localizedCaseInsensitiveCompare($1.cinema) == .orderedAscending }
    }
}
