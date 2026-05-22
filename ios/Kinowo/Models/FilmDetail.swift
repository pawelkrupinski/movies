import Foundation

/// Per-film detail data scraped from `/film?title=...`.
///
/// Mirrors the right-hand column of the web's `film.scala.html`:
/// cinema-link buttons, Opis / Reżyseria / Obsada blocks, Zwiastuny
/// (trailer embed URLs), and the full Seanse section. The listing
/// `Film` model carries only what the `/` page renders per card
/// (title, poster, ratings, runtime, showings); detail-only fields
/// live here so the listing parser doesn't grow obligations the
/// listing HTML can't satisfy.
struct FilmDetail: Hashable {
    let title: String
    let posterURL: URL?
    let fallbackPosterURLs: [URL]
    let ratings: Film.Ratings
    /// Per-cinema "Helios ↗", "Multikino ↗" buttons — external links to
    /// each cinema's film page. Order preserved as the server emitted
    /// (alphabetical-by-cinema).
    let cinemaLinks: [CinemaLink]
    let synopsis: String?
    let director: String?
    let cast: String?
    /// YouTube `/embed/<id>` URLs, deduped server-side. May be empty.
    let trailerURLs: [URL]
    /// Same shape as the listing's showings tree — re-parsed off the
    /// `/film` page's `showtimes-section` because it can show more dates
    /// than the listing card's `_filmShowings` slice.
    let showings: [DayShowings]

    struct CinemaLink: Hashable {
        let cinema: String
        let url: URL
    }
}
