import Foundation

enum DateFilter: Hashable {
    case anytime
    case today
    case tomorrow
    case week
    case specific(String) // YYYY-MM-DD

    // Dated options first (Dziś / Jutro / Tydzień), with the catch-all
    // `Kiedykolwiek` pushed to the rightmost slot so the bar reads as
    // "narrow → broad" left-to-right.
    static let presets: [DateFilter] = [.today, .tomorrow, .week, .anytime]

    var label: String {
        switch self {
        case .anytime:        return "Wszystkie"
        case .today:          return "Dziś"
        case .tomorrow:       return "Jutro"
        case .week:           return "7 dni"
        case .specific(let d): return d
        }
    }

    func matches(date dateString: String, now: Date = Date()) -> Bool {
        switch self {
        case .anytime:
            return true
        case .today:
            return dateString == DateFilter.iso(now)
        case .tomorrow:
            return dateString == DateFilter.iso(now.addingTimeInterval(86_400))
        case .week:
            let today = DateFilter.iso(now)
            let in7   = DateFilter.iso(now.addingTimeInterval(7 * 86_400))
            return dateString >= today && dateString <= in7
        case .specific(let d):
            return dateString == d
        }
    }

    private static let warsawCalendar: Calendar = {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw") ?? .current
        return cal
    }()

    private static let isoFormatter: DateFormatter = {
        let formatter = DateFormatter()
        formatter.calendar = warsawCalendar
        formatter.timeZone = warsawCalendar.timeZone
        formatter.dateFormat = "yyyy-MM-dd"
        formatter.locale = Locale(identifier: "en_US_POSIX")
        return formatter
    }()

    static func iso(_ date: Date) -> String { isoFormatter.string(from: date) }
}

/// Mirrors the web's Filtry dropdown: per-axis format tokens (Wymiar /
/// Wersja / IMAX) combined with a from-hour lower bound. Each axis is
/// independent — empty strings on the radio axes (`dimension`, `language`)
/// mean "no constraint", and `fromHour < 0` means "Dowolna". A showtime
/// passes when EVERY non-empty constraint matches, matching
/// `applyFilters()` on the web side.
struct FormatFilter: Equatable {
    var dimension: String = ""    // "" | "2D" | "3D"
    var language: String = ""     // "" | "NAP" | "DUB"
    var imax: Bool = false
    var fromHour: Int = -1        // -1 = Dowolna
    var fromMinute: Int = 0

    static let empty = FormatFilter()

    var isEmpty: Bool {
        dimension.isEmpty && language.isEmpty && !imax && fromHour < 0
    }

    /// Tokens that the badge's `data-format` set must contain. Empty
    /// list = no constraint from the format axes (the time axis can
    /// still narrow things).
    private var requiredTokens: [String] {
        var tokens: [String] = []
        if !dimension.isEmpty { tokens.append(dimension) }
        if !language.isEmpty  { tokens.append(language)  }
        if imax               { tokens.append("IMAX")    }
        return tokens
    }

    /// `nil` when the user picked "Dowolna" (any time).
    var fromMinutes: Int? {
        fromHour >= 0 ? fromHour * 60 + fromMinute : nil
    }

    func matches(showtime: Showtime) -> Bool {
        let tokens = requiredTokens
        if !tokens.isEmpty {
            let badge = Set(showtime.format.split(separator: " ").map(String.init))
            for t in tokens where !badge.contains(t) { return false }
        }
        if let from = fromMinutes {
            let parts = showtime.time.split(separator: ":").compactMap { Int($0) }
            // Unparseable time → never filtered out (matches the web's
            // `timeMin < 0 || timeMin >= fromMin` guard).
            if parts.count == 2 && (parts[0] * 60 + parts[1]) < from {
                return false
            }
        }
        return true
    }
}

/// Whether any filter that "Wyczyść" clears is currently set — the single
/// source of truth for the Filtry button's active tint. Deliberately ignores
/// cinema selection and hidden films: those are persistent preferences that
/// Wyczyść leaves alone, so they must not light the bar (or it would read as
/// "filtering" for a state Wyczyść can't clear). Mirrors exactly the reset in
/// the Filtry sheet's Wyczyść button.
enum ActiveFilters {
    static func any(
        format: FormatFilter,
        excludedCountries: Set<String>,
        excludedGenres: Set<String>,
        excludedDirectors: Set<String>,
        excludedCast: Set<String>
    ) -> Bool {
        !format.isEmpty
            || !excludedCountries.isEmpty
            || !excludedGenres.isEmpty
            || !excludedDirectors.isEmpty
            || !excludedCast.isEmpty
    }
}

/// One cinema's slice of a filtered film list: every film that plays at
/// this cinema, with each film's `showings` restricted to this cinema's
/// dates and slots. Drives the Kina tab's cinema-grouped layout —
/// mirrors the web's `_cinemaCards` (one `.cinema-section` per cinema,
/// holding film cards whose `_filmShowings` see only that cinema's
/// slots).
struct CinemaSection: Identifiable, Hashable {
    var id: String { cinema }
    let cinema: String
    let films: [Film]

    /// Full cinema-name → short pill label map, one entry per cinema across
    /// every city — mirrors the web's `Cinema.pillMap` (and grouped in the same
    /// city order) so a pill reads identically on every platform. A name absent
    /// here falls back to itself via `pillName(for:)`.
    static let pillNames: [String: String] = [
        // Poznań
        "Kino Apollo": "Apollo",
        "Kino Bułgarska 19": "Bułgarska 19",
        "Kino Malta Charlie Monroe": "Malta Charlie Monroe",
        "Helios Posnania": "Helios",
        "Cinema City Kinepolis": "Kinepolis",
        "Kino Muza": "Muza",
        "Multikino Stary Browar": "Multikino",
        "Kino Pałacowe": "Pałacowe",
        "Cinema City Poznań Plaza": "Poznań Plaza",
        "Kino Rialto": "Rialto",
        // Wrocław
        "Cinema City Wroclavia": "Wroclavia",
        "Cinema City Korona": "Korona",
        "Multikino Pasaż Grunwaldzki": "Pasaż Grunwaldzki",
        "Helios Magnolia Park": "Magnolia",
        "Helios Aleja Bielany": "Aleja Bielany",
        "Kino Nowe Horyzonty": "Nowe Horyzonty",
        "Dolnośląskie Centrum Filmowe": "DCF",
        // Warszawa
        "Cinema City Arkadia": "Arkadia",
        "Cinema City Bemowo": "Bemowo",
        "Cinema City Galeria Północna": "Galeria Północna",
        "Cinema City Janki": "Janki",
        "Cinema City Mokotów": "Mokotów",
        "Cinema City Promenada": "Promenada",
        "Cinema City Sadyba": "Sadyba",
        "Multikino Złote Tarasy": "Złote Tarasy",
        "Multikino Młociny": "Młociny",
        "Multikino Reduta": "Reduta",
        "Multikino Targówek": "Targówek",
        "Multikino Wola Park": "Wola Park",
        "Helios Blue City": "Blue City",
        "Kino Muranów": "Muranów",
        "Kino Luna": "Luna",
        "Kino Elektronik": "Elektronik",
        "Kino Iluzjon": "Iluzjon",
        "KinoGram": "KinoGram",
        "Kino Kultura": "Kultura",
        "Kino Amondo": "Amondo",
        "Kino na Boku": "na Boku",
        "Kino Głębocka 66": "Głębocka 66",
        "KINOMUZEUM": "Kinomuzeum",
        "Kino Świt": "Świt",
        "Kino Kępa": "Kępa",
        "KINOkawiarnia Stacja Falenica": "Stacja Falenica",
        "Służewski Dom Kultury": "SDK",
        "Kino Atlantic": "Atlantic",
        "Kinoteka": "Kinoteka",
        "Kino U-jazdowski": "U-jazdowski",
        "Kino Cytadela": "Cytadela",
        // Kraków
        "Cinema City Bonarka": "Bonarka",
        "Cinema City Kazimierz": "Kazimierz",
        "Cinema City Zakopianka": "Zakopianka",
        "Multikino Kraków": "Multikino",
        "Kino Mikro": "Mikro",
        "Mikro Bronowice": "Mikro Bronowice",
        "Kino Sfinks": "Sfinks",
        // Łódź
        "Cinema City Manufaktura": "Manufaktura",
        "Multikino Łódź": "Multikino",
        "Helios Łódź": "Helios",
        "Kino Charlie": "Charlie",
        // Katowice
        "Cinema City Punkt 44": "Punkt 44",
        "Cinema City Silesia": "Silesia",
        "Multikino Katowice": "Multikino",
        "Helios Katowice": "Helios",
        "Kino Kosmos": "Kosmos",
        "Kino Światowid": "Światowid",
        // Szczecin
        "Helios Kupiec": "Helios",
        "Multikino Szczecin": "Multikino",
        "Kino Pionier 1907": "Pionier",
        // Białystok
        "Helios Alfa": "Alfa",
        "Helios Biała": "Biała",
        "Helios Jurowiecka": "Jurowiecka",
        "Kino Forum": "Forum",
        // Trójmiasto (Gdańsk · Gdynia · Sopot)
        "Multikino Gdańsk": "Multikino",
        "Helios Metropolia": "Metropolia",
        "Helios Forum": "Forum",
        "Helios Riviera": "Riviera",
        "Kino Spektrum": "Spektrum",
        "Kino Kameralne Cafe": "Kameralne",
        "Kino IKM": "IKM",
        "Kino Muzeum": "Muzeum",
        "Kino Żak": "Żak",
        "KinoPort": "KinoPort",
        // Bydgoszcz
        "Cinema City Bydgoszcz": "Cinema City",
        "Multikino Bydgoszcz": "Multikino",
        "Helios Bydgoszcz": "Helios",
        "Kino Orzeł": "Orzeł",
        // Lublin
        "Cinema City Felicity": "Felicity",
        "Cinema City Lublin Plaza": "Lublin Plaza",
        "Multikino Lublin": "Multikino",
        "Kino Bajka": "Bajka",
        // Częstochowa
        "Cinema City Jurajska": "Jurajska",
        "Cinema City Wolność": "Wolność",
        // Radom
        "Helios Radom": "Helios",
        "Multikino Radom": "Multikino",
        // Sosnowiec
        "Helios Sosnowiec": "Helios",
        "Cinema City Sosnowiec": "Cinema City",
        // Toruń
        "Cinema City Czerwona Droga": "Czerwona Droga",
        "Cinema City Toruń Plaza": "Plaza",
        // Kielce
        "Helios Kielce": "Helios",
        "Multikino Kielce": "Multikino",
        // Rzeszów
        "Helios Rzeszów": "Helios",
        "Multikino Rzeszów": "Multikino",
        "Kino Zorza": "Zorza",
        // Gliwice
        "Cinema City Gliwice": "Cinema City",
        // Zabrze
        "Multikino Zabrze": "Multikino",
    ]

    static func pillName(for cinema: String) -> String {
        pillNames[cinema] ?? cinema
    }
}

/// Mirrors the web's `#sort-by` dropdown (shared.js `compareCards`).
/// `earliest` keeps the server's earliest-showtime order (the default);
/// `rating` orders by weighted rating, descending. Both are offered in the
/// iOS Filtry sheet.
enum SortOption: String, CaseIterable, Hashable {
    case earliest
    case rating

    var label: String {
        switch self {
        case .earliest: return "Najbliższy seans"
        case .rating:   return "Ocena"
        }
    }
}

extension Sequence where Element == Film {
    /// Apply the chosen sort. `.earliest` preserves the server's
    /// earliest-showtime order (the list already arrives sorted that way);
    /// `.rating` orders by weighted rating descending, falling back to the
    /// earliest order for ties — matching the web's `compareCards`.
    func sorted(by option: SortOption) -> [Film] {
        switch option {
        case .earliest:
            return Array(self)
        case .rating:
            return enumerated()
                .sorted { lhs, rhs in
                    let a = lhs.element.ratings.weightedRating
                    let b = rhs.element.ratings.weightedRating
                    if a != b { return a > b }
                    return lhs.offset < rhs.offset // tie-break: earliest order
                }
                .map(\.element)
        }
    }

    /// Drop screenings already in the past and re-sort cinemas by the
    /// earliest remaining slot of the day. Mirrors the web's
    /// `MovieController.toSchedules`:
    /// - showtimes with `dateTime <= now - 30min` are dropped (a
    ///   screening that started 25 min ago still counts as future);
    /// - within each day, cinemas are ordered by their earliest
    ///   remaining showtime that day;
    /// - cinema-groups / days / films that empty out are removed.
    ///
    /// The server applies the same logic at request time, so a freshly
    /// fetched payload is already pruned. Calling this locally on app
    /// foreground keeps the cached data fresh as wall-clock advances
    /// without a round-trip.
    func prunedPastShowings(now: Date = Date()) -> [Film] {
        return self.compactMap { film in
            let days: [DayShowings] = film.showings.compactMap { day in
                let kept: [CinemaShowings] = day.cinemas.compactMap { cg in
                    let future = cg.showtimes.filter {
                        ShowtimeClock.isFuture($0, on: day.date, now: now)
                    }
                    guard !future.isEmpty else { return nil }
                    return CinemaShowings(cinema: cg.cinema, cinemaURL: cg.cinemaURL, showtimes: future)
                }
                guard !kept.isEmpty else { return nil }
                let sorted = kept.sorted {
                    ShowtimeClock.earliestMinutes($0) < ShowtimeClock.earliestMinutes($1)
                }
                return DayShowings(date: day.date, label: day.label, cinemas: sorted)
            }
            if days.isEmpty { return nil }
            return Film(
                title: film.title,
                posterURL: film.posterURL,
                fallbackPosterURLs: film.fallbackPosterURLs,
                runtimeMinutes: film.runtimeMinutes,
                releaseYear: film.releaseYear,
                genres: film.genres,
                ratings: film.ratings,
                countries: film.countries,
                directors: film.directors,
                cast: film.cast,
                showings: days
            )
        }
    }

    /// Apply the cross-screen filter stack — date / format / search /
    /// hidden / single-cinema — to a list of films.
    ///
    /// `selectedCinema` is the single-select cinema pill (nil = Wszystkie,
    /// no cinema constraint). When set, every cinema-group whose name isn't
    /// the selected one is dropped, so the listing shows only that cinema's
    /// screenings. A `selectedCinema` that matches none of the films' cinemas
    /// — e.g. a value persisted while browsing another city — is treated as
    /// Wszystkie, so a stale cross-city selection can't empty the whole screen.
    ///
    /// Semantics otherwise mirror the web's `applyFilters()`: drop a showtime
    /// whose format/time-from doesn't pass, drop a cinema-group whose
    /// every showtime fell out, drop a day whose every cinema-group
    /// fell out, drop a film whose every day fell out. A film stays
    /// visible as long as one badge somewhere still passes.
    func filteredFor(
        date: DateFilter,
        format: FormatFilter,
        query: String,
        hidden: Set<String>,
        selectedCinema: String? = nil,
        excludedCountries: Set<String> = [],
        excludedGenres: Set<String> = [],
        excludedDirectors: Set<String> = [],
        excludedCast: Set<String> = [],
        now: Date = Date()
    ) -> [Film] {
        let films = Array(self)
        // Guard cross-city (feature point 5): a selection made in another city
        // won't name any cinema present here; fall back to "all" rather than
        // filtering every film away into an empty screen.
        let effectiveCinema: String? = selectedCinema.flatMap { name in
            films.contains { film in
                film.showings.contains { $0.cinemas.contains { $0.cinema == name } }
            } ? name : nil
        }
        let trimmedQuery = query.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        return films.compactMap { film in
            if hidden.contains(film.title) { return nil }
            if !trimmedQuery.isEmpty && !film.title.lowercased().contains(trimmedQuery) { return nil }
            if !excludedCountries.isEmpty && Set(film.countries).isSubset(of: excludedCountries) { return nil }
            if !excludedGenres.isEmpty && !film.genres.isEmpty && Set(film.genres).isSubset(of: excludedGenres) { return nil }
            if !excludedDirectors.isEmpty && !film.directors.isEmpty && Set(film.directors).isSubset(of: excludedDirectors) { return nil }
            if !excludedCast.isEmpty && !film.cast.isEmpty && Set(film.cast).isSubset(of: excludedCast) { return nil }
            let days: [DayShowings] = film.showings.compactMap { day in
                if !date.matches(date: day.date, now: now) { return nil }
                let cinemas: [CinemaShowings] = day.cinemas.compactMap { cg in
                    if let sel = effectiveCinema, cg.cinema != sel { return nil }
                    let times = format.isEmpty
                        ? cg.showtimes
                        : cg.showtimes.filter { format.matches(showtime: $0) }
                    guard !times.isEmpty else { return nil }
                    return CinemaShowings(cinema: cg.cinema, cinemaURL: cg.cinemaURL, showtimes: times)
                }
                guard !cinemas.isEmpty else { return nil }
                return DayShowings(date: day.date, label: day.label, cinemas: cinemas)
            }
            if days.isEmpty { return nil }
            return Film(
                title: film.title,
                posterURL: film.posterURL,
                fallbackPosterURLs: film.fallbackPosterURLs,
                runtimeMinutes: film.runtimeMinutes,
                releaseYear: film.releaseYear,
                genres: film.genres,
                ratings: film.ratings,
                countries: film.countries,
                directors: film.directors,
                cast: film.cast,
                showings: days
            )
        }
    }

    /// Resolve the complete, all-days film for a listing film handed to the
    /// detail screen. The day-paged grids navigate with a `filteredFor`
    /// copy whose `showings` were pruned to the tapped day-page; the detail
    /// screen is a full-schedule view, so it re-resolves the whole film by
    /// title from this (unfiltered) `store.films` list — the same lookup the
    /// deep-link push and Android's `films.firstOrNull { it.title == title }`
    /// already do. Falls back to `film` itself when the title is no longer in
    /// the listing (it just left the repertoire).
    func fullFilm(for film: Film) -> Film {
        first { $0.title == film.title } ?? film
    }

    /// Pivot the (cross-cinema) film list into per-cinema sections. The
    /// web's `/kina` controller emits this shape directly from
    /// `CinemaSchedule`; iOS only sees the per-film grouping from `/`,
    /// so the same view shape is rebuilt here. Each output `Film`
    /// carries only the showings that happen at the section's cinema —
    /// so dropping the per-card cinema label (`showCinemaHeaders=false`)
    /// is non-lossy.
    ///
    /// Sections are ordered alphabetically by cinema name to match the
    /// Kina-tab pill row; films inside a section keep the input order
    /// (which is the global "earliest showtime" ordering coming from
    /// the server's `/` HTML).
    func groupedByCinema() -> [CinemaSection] {
        var perCinema: [String: [Film]] = [:]
        for film in self {
            var seen = Set<String>()
            for day in film.showings {
                for cg in day.cinemas {
                    seen.insert(cg.cinema)
                }
            }
            for cinema in seen {
                let days: [DayShowings] = film.showings.compactMap { day in
                    let kept = day.cinemas.filter { $0.cinema == cinema }
                    guard !kept.isEmpty else { return nil }
                    return DayShowings(date: day.date, label: day.label, cinemas: kept)
                }
                guard !days.isEmpty else { continue }
                perCinema[cinema, default: []].append(Film(
                    title: film.title,
                    posterURL: film.posterURL,
                    fallbackPosterURLs: film.fallbackPosterURLs,
                    runtimeMinutes: film.runtimeMinutes,
                    releaseYear: film.releaseYear,
                    genres: film.genres,
                    ratings: film.ratings,
                    countries: film.countries,
                    directors: film.directors,
                    cast: film.cast,
                    showings: days
                ))
            }
        }
        return perCinema.keys.sorted { CinemaSection.pillName(for: $0) < CinemaSection.pillName(for: $1) }.map { name in
            CinemaSection(cinema: name, films: perCinema[name]!)
        }
    }
}

/// Combines a screening's `YYYY-MM-DD` date and `HH:MM` time into a
/// wall-clock moment in Europe/Warsaw — the timezone the web emits
/// against and the only one the rest of the app reasons about. Pulled
/// out so `prunedPastShowings` and any future caller (detail screen,
/// notifications) share one implementation of "is this slot still in
/// the future" and "what minute does this cinema's earliest slot start".
enum ShowtimeClock {

    /// Match the web's `isAfter(now.minusMinutes(30))` — a slot is
    /// "future" if its wall-clock dateTime is strictly after
    /// `now - 30min`. A screening that started 25 min ago is still
    /// considered live; one that started 31 min ago is dropped. Slots
    /// whose `time` doesn't parse are kept (don't silently drop badges
    /// we can't reason about — same defensive stance as
    /// `FormatFilter.matches`).
    static func isFuture(_ slot: Showtime, on date: String, now: Date = Date()) -> Bool {
        guard let dt = warsawDate(date: date, time: slot.time) else { return true }
        return dt > now.addingTimeInterval(-30 * 60)
    }

    /// Minute-of-day of the earliest slot in a cinema-group. Sentinel
    /// `Int.max` for an empty list so an unsortable input lands at the
    /// end rather than crashing — empty cinema-groups should have been
    /// dropped before calling this, so the sentinel exists purely to
    /// keep the comparator total.
    static func earliestMinutes(_ cg: CinemaShowings) -> Int {
        cg.showtimes
            .compactMap(minutesOfDay)
            .min() ?? Int.max
    }

    private static func minutesOfDay(_ slot: Showtime) -> Int? {
        let parts = slot.time.split(separator: ":").compactMap { Int($0) }
        guard parts.count == 2 else { return nil }
        return parts[0] * 60 + parts[1]
    }

    private static let warsawCalendar: Calendar = {
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(identifier: "Europe/Warsaw") ?? .current
        return cal
    }()

    private static let warsawFormatter: DateFormatter = {
        let formatter = DateFormatter()
        formatter.calendar = warsawCalendar
        formatter.timeZone = warsawCalendar.timeZone
        formatter.dateFormat = "yyyy-MM-dd HH:mm"
        formatter.locale = Locale(identifier: "en_US_POSIX")
        return formatter
    }()

    private static func warsawDate(date: String, time: String) -> Date? {
        warsawFormatter.date(from: "\(date) \(time)")
    }
}
