import SwiftUI

/// The pages the tuning pager swipes between, in display order.
enum TuningPage: Int, CaseIterable, Identifiable {
    case card, kina, film
    var id: Int { rawValue }
    var title: String {
        switch self {
        case .card: return "Karta"
        case .kina: return "Kina"
        case .film: return "Film"
        }
    }
}

/// The `Font.Weight` choices the tuning picker offers, with a reverse map so the
/// segmented control can show the current style's weight. The `rawValue` is the
/// token used in the copied-values text.
enum WeightOption: String, CaseIterable, Identifiable {
    case regular, medium, semibold, bold, heavy
    var id: String { rawValue }
    var short: String {
        switch self {
        case .regular: return "Reg"
        case .medium: return "Med"
        case .semibold: return "Semi"
        case .bold: return "Bold"
        case .heavy: return "Heavy"
        }
    }
    var weight: Font.Weight {
        switch self {
        case .regular: return .regular
        case .medium: return .medium
        case .semibold: return .semibold
        case .bold: return .bold
        case .heavy: return .heavy
        }
    }
    init(_ w: Font.Weight) {
        switch w {
        case .medium: self = .medium
        case .semibold: self = .semibold
        case .bold: self = .bold
        case .heavy: self = .heavy
        default: self = .regular
        }
    }
}

/// Deterministic sample repertoire — identical every launch. The first two
/// films exercise every showtime-rendering case; films 3–6 are ordinary so the
/// grid looks like a real screen. `sections` regroups them by cinema for the
/// Kina page, and `detailFilm` (+ `details`) feeds the Film page.
enum TuningSampleData {
    static let films: [Film] = [edgeCases, manyShowtimes, drama, kids, latNight, festival]

    /// Cinema-grouped view of the sample films for the Kina page. Uses real
    /// cinema display names (so `CinemaSection.pillName` shortens them the way
    /// the live Kina tab does).
    static let sections: [CinemaSection] = [
        CinemaSection(cinema: "Cinema City Kinepolis", films: [edgeCases, drama, festival]),
        CinemaSection(cinema: "Multikino Stary Browar", films: [manyShowtimes, kids]),
        CinemaSection(cinema: "Helios Posnania", films: [latNight]),
    ]

    /// One rich film for the Film-detail page — genres / directors / cast /
    /// countries populated (the listing samples leave them empty) so every
    /// meta block renders and its fonts are tunable.
    static let detailFilm = Film(
        title: "Wszystkie przypadki", posterURL: nil, fallbackPosterURLs: [],
        runtimeMinutes: 128, releaseYear: 2025,
        genres: ["Dramat", "Thriller"], ratings: ratings(7.8, 81, 92, 7.4),
        countries: ["Polska", "Francja"], directors: ["Jan Kowalski"],
        cast: ["Anna Nowak", "Piotr Wiśniewski", "Maria Lewandowska"],
        showings: edgeCases.showings
    )

    /// Synopsis + trailers for `detailFilm` (matched by title), so the Opis
    /// block and Zwiastuny section show real content to tune.
    static let details: [FilmDetails] = [
        FilmDetails(
            title: "Wszystkie przypadki",
            originalTitle: "All The Cases",
            synopsis: "Wciągający dramat o splątanych losach kilku rodzin w powojennej "
                + "Europie. Reżyser prowadzi widza przez kolejne odsłony tajemnicy, "
                + "stopniowo odsłaniając, jak jeden wybór potrafi zaważyć na życiu "
                + "wszystkich bohaterów. Tekst celowo długi, by dało się dostroić "
                + "rozmiar i odstępy opisu na ekranie szczegółów.",
            trailerURLs: [URL(string: "https://www.youtube.com/embed/aqz-KE-bpKQ")!]
        )
    ]

    private static func ratings(_ imdb: Double?, _ mc: Int?, _ rt: Int?, _ fw: Double?) -> Film.Ratings {
        Film.Ratings(imdb: imdb, imdbURL: nil, metascore: mc, metacriticURL: nil,
                     rottenTomatoes: rt, rottenTomatoesURL: nil, filmweb: fw, filmwebURL: nil)
    }

    private static func film(_ title: String, _ runtime: Int?, _ year: Int?, _ ratings: Film.Ratings, _ days: [DayShowings]) -> Film {
        Film(title: title, posterURL: nil, fallbackPosterURLs: [], runtimeMinutes: runtime,
             releaseYear: year, genres: [], ratings: ratings, countries: [], directors: [],
             cast: [], showings: days)
    }

    private static func st(_ time: String, _ format: String = "", room: String? = nil) -> Showtime {
        Showtime(time: time, format: format, room: room, bookingURL: nil)
    }

    // 1 — every case: empty/short/long/multi-token formats, a room, common-token
    // stripping (Multikino's showtimes all share "2D NAP" → rendered bare), two
    // days, two cinemas.
    private static let edgeCases = film("1 — Wszystkie przypadki", 128, 2025, ratings(7.8, 81, 92, 7.4), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: URL(string: "https://example.com"), showtimes: [
                st("10:00"),
                st("12:30", "2D"),
                st("14:45", "2D DUB", room: "Sala 1"),
                st("18:00", "IMAX 3D NAP", room: "IMAX"),
                st("20:15", "3D NAP"),
                st("22:30", "2D NAP"),
                st("23:59", "4DX 3D DUB"),
            ]),
            CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
                st("11:00", "2D NAP"), st("13:00", "2D NAP"), st("15:00", "2D NAP"),
            ]),
        ]),
        DayShowings(date: "2026-06-09", label: "Wtorek 9 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: nil, showtimes: [
                st("16:00", "2D"), st("19:30", "VOSE"),
            ]),
        ]),
    ])

    // 2 — many showtimes → wraps across several rows.
    private static let manyShowtimes = film("2 — Dużo seansów", 95, 2026, ratings(6.2, nil, 55, nil), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Cinema City", cinemaURL: nil, showtimes: [
                st("09:15", "2D"), st("10:30", "2D DUB"), st("11:45", "3D"), st("13:00", "2D NAP"),
                st("14:15", "2D"), st("15:30", "3D DUB"), st("16:45", "2D"), st("18:00", "IMAX 2D"),
                st("19:15", "2D NAP"), st("20:30", "3D NAP"), st("21:45", "2D"), st("23:00", "2D DUB"),
            ]),
        ]),
    ])

    private static let drama = film("3 — Dramat", 142, 2024, ratings(8.1, 88, nil, 7.9), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: nil, showtimes: [
                st("17:00", "2D NAP"), st("20:00", "2D NAP"),
            ]),
        ]),
    ])

    private static let kids = film("4 — Dla dzieci", 88, 2026, ratings(nil, nil, 78, 6.8), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Multikino", cinemaURL: nil, showtimes: [
                st("10:00", "2D DUB"), st("12:00", "2D DUB"), st("14:00", "3D DUB"),
            ]),
        ]),
    ])

    private static let latNight = film("5 — Nocny seans", 116, 2025, ratings(7.0, 64, 71, nil), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Cinema City", cinemaURL: nil, showtimes: [
                st("22:00", "2D NAP"), st("23:30", "2D NAP", room: "Sala 7"),
            ]),
        ]),
    ])

    private static let festival = film("6 — Festiwal", 101, 2023, ratings(7.5, nil, nil, 8.2), [
        DayShowings(date: "2026-06-08", label: "Poniedziałek 8 czerwca", cinemas: [
            CinemaShowings(cinema: "Kino Pod Baranami", cinemaURL: nil, showtimes: [
                st("15:00", "VOSE"), st("18:30", "OV"), st("21:00", "VOSE"),
            ]),
        ]),
    ])
}

#Preview {
    ShowtimeTuningScreen()
        .environment(UserPreferences())
}
