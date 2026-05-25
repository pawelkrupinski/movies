import Foundation

struct Film: Identifiable, Hashable {
    var id: String { title }
    let title: String
    let posterURL: URL?
    /// Chain of alternative poster URLs to try when `posterURL` fails.
    /// Server-side `_movieCard` ships them in source-priority order
    /// (Cinema City after Multikino, then other cinemas, then TMDB,
    /// then IMDb) as a pipe-separated `data-fallbacks` attribute —
    /// cinema CDNs intermittently 403/404 on the bytes themselves
    /// (Cinema City has shipped posterLinks to images they hadn't
    /// uploaded; some cinemas rotate slugs without redirects), so we'd
    /// rather walk through every cinema poster we know before falling
    /// back to TMDB. The web swaps via `onerror`; we do it in
    /// `FilmCardView.PosterImage`.
    ///
    /// Multikino specifically is fine from residential IPs — phones,
    /// browsers, this app's `AsyncImage` on a user's network all hit
    /// 200. The server-side `PosterProxy.SkipHosts` block exists
    /// because weserv's datacenter egress (and Fly's) lands on
    /// Cloudflare's ASN blocklist; client fetches don't go through
    /// either, so end-users see the poster fine.
    let fallbackPosterURLs: [URL]
    let runtimeMinutes: Int?
    let ratings: Ratings
    let countries: [String]
    let directors: [String]
    let cast: [String]
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
