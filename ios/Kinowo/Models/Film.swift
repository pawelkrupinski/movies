import Foundation

struct Film: Identifiable, Hashable, Codable {
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
    /// Release year — shown as a `.pill.year` next to runtime, mirroring
    /// the web `_movieCard` / `/film` title block. Optional because not
    /// every cinema/enrichment source resolves a year.
    let releaseYear: Int?
    /// Genre labels (already in source-priority order from the server).
    /// The web card shows the first three; the `/film` page shows all.
    let genres: [String]
    let ratings: Ratings
    let countries: [String]
    let directors: [String]
    let cast: [String]
    let showings: [DayShowings]

    struct Ratings: Hashable, Codable {
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

        /// Equal-weight average of every rating we hold, each normalised to
        /// a 0–10 scale (Metacritic and Rotten Tomatoes are 0–100, so they
        /// divide by 10). Missing sources are skipped — present sources share
        /// the weight equally — and a film with no ratings scores 0. Mirrors
        /// the server's `MovieRecord.weightedRating`; it's the sort key
        /// behind the "Ocena" sort order.
        var weightedRating: Double {
            let normalised: [Double] = [
                imdb,
                filmweb,
                metascore.map { Double($0) / 10.0 },
                rottenTomatoes.map { Double($0) / 10.0 }
            ].compactMap { $0 }
            return normalised.isEmpty ? 0 : normalised.reduce(0, +) / Double(normalised.count)
        }

        /// One-decimal score string for the IMDb / Filmweb pills. Always shows
        /// the tenths place — a whole-number score like 7.0 renders "7.0", not
        /// "7". `String(format:)` is not locale-aware for `%f`, so the separator
        /// is always a dot, matching the web's `f"$r%.1f"` and Android's
        /// `oneDecimal`.
        static func scoreText(_ value: Double) -> String {
            String(format: "%.1f", value)
        }
    }
}

struct DayShowings: Hashable, Codable {
    /// `YYYY-MM-DD` — comparable as a string thanks to ISO layout.
    let date: String
    /// Polish day-and-date label exactly as the web app renders it
    /// (e.g. "Czwartek 21 maja"). Kept verbatim so the iOS app doesn't
    /// have to re-format locale-aware Polish dates.
    let label: String
    let cinemas: [CinemaShowings]
}

struct CinemaShowings: Hashable, Codable {
    let cinema: String
    let cinemaURL: URL?
    let showtimes: [Showtime]
}

struct Showtime: Hashable, Identifiable, Codable {
    var id: String { "\(time)|\(format)|\(bookingURL?.absoluteString ?? "")" }
    /// `HH:MM`.
    let time: String
    /// Space-separated format tokens ("2D NAP", "IMAX 3D", …).
    let format: String
    let room: String?
    let bookingURL: URL?

    /// Room/hall name to surface on a long-press, or `nil` when the
    /// scraper left it blank (e.g. Apollo, Rialto) or it's only
    /// whitespace — so a blank value never pops an empty tooltip.
    var displayRoom: String? {
        guard let room = room?.trimmingCharacters(in: .whitespacesAndNewlines),
              !room.isEmpty else { return nil }
        return room
    }
}

/// Canonical public URL for a film's `/film?title=…` page — the iOS
/// counterpart of the server's `controllers.FilmHref`. Backs the Share
/// button on `FilmDetailView` and the long-press "Skopiuj link" on
/// `FilmCardView`. Mirrors `FilmHref`'s RFC 3986 encoding (spaces → `%20`,
/// not the form `+`) so a link shared from the app is byte-identical to one
/// copied off the website and round-trips through link-preview scrapers and
/// the web router alike.
///
/// The host is hardcoded rather than reusing `kinowoBaseURL` because that
/// constant lives in the Combine-only `Auth` layer, which `KinowoCore`
/// (this file's SPM target) excludes — the networking layer hardcodes the
/// same host for the same reason.
enum FilmShareLink {
    /// Allowed set for the `title` query value: the unreserved chars plus
    /// the `*-._` that Java's `URLEncoder.encode` also leaves literal.
    /// Everything else — space, `:`, `&`, `?`, diacritics — percent-encodes,
    /// matching `URLEncoder.encode(title).replace("+", "%20")` byte for byte.
    private static let titleAllowed: CharacterSet = {
        var set = CharacterSet()
        set.insert(charactersIn: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*-._")
        return set
    }()

    static func url(forTitle title: String) -> URL {
        let encoded = title.addingPercentEncoding(withAllowedCharacters: titleAllowed) ?? title
        // Safe to force-unwrap: `encoded` contains only URL-safe characters
        // and the surrounding string is a fixed, valid absolute URL.
        return URL(string: "https://kinowo.fly.dev/film?title=\(encoded)")!
    }
}
