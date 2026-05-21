import Foundation

struct Film: Identifiable, Hashable {
    var id: String { title }
    let title: String
    let posterURL: URL?
    /// TMDB-derived poster. Server-side `_movieCard` emits this as
    /// `data-fallback` on the `<img>` whenever it differs from the
    /// primary, because cinema-side URLs occasionally 404 (e.g. Cinema
    /// City listing a film with a posterLink to an image they haven't
    /// uploaded yet). The web swaps via `onerror`; we do it in
    /// `FilmCardView.PosterView`.
    let fallbackPosterURL: URL?
    let runtimeMinutes: Int?
    let ratings: Ratings
    let showings: [DayShowings]

    struct Ratings: Hashable {
        let imdb: Double?
        let imdbURL: URL?
        let metascore: Int?
        let metacriticURL: URL?
        let rottenTomatoes: Int?
        let rottenTomatoesURL: URL?
        let filmweb: Double?
        let filmwebURL: URL?

        static let empty = Ratings(
            imdb: nil, imdbURL: nil,
            metascore: nil, metacriticURL: nil,
            rottenTomatoes: nil, rottenTomatoesURL: nil,
            filmweb: nil, filmwebURL: nil
        )

        var isEmpty: Bool {
            imdb == nil && metascore == nil && rottenTomatoes == nil && filmweb == nil
        }
    }
}

struct DayShowings: Hashable {
    /// `YYYY-MM-DD` — comparable as a string thanks to ISO layout.
    let date: String
    /// Polish day-and-date label exactly as the web app renders it
    /// (e.g. "Czwartek 21 maja"). Kept verbatim so the iOS app doesn't
    /// have to re-format locale-aware Polish dates.
    let label: String
    let cinemas: [CinemaShowings]
}

struct CinemaShowings: Hashable {
    let cinema: String
    let cinemaURL: URL?
    let showtimes: [Showtime]
}

struct Showtime: Hashable, Identifiable {
    var id: String { "\(time)|\(format)|\(bookingURL?.absoluteString ?? "")" }
    /// `HH:MM`.
    let time: String
    /// Space-separated format tokens ("2D NAP", "IMAX 3D", …).
    let format: String
    let room: String?
    let bookingURL: URL?
}
