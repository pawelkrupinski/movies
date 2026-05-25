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
        let f = DateFormatter()
        f.calendar = warsawCalendar
        f.timeZone = warsawCalendar.timeZone
        f.dateFormat = "yyyy-MM-dd"
        f.locale = Locale(identifier: "en_US_POSIX")
        return f
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
        var t: [String] = []
        if !dimension.isEmpty { t.append(dimension) }
        if !language.isEmpty  { t.append(language)  }
        if imax               { t.append("IMAX")    }
        return t
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
}

extension Sequence where Element == Film {
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
                ratings: film.ratings,
                countries: film.countries,
                showings: days
            )
        }
    }

    /// Apply the cross-screen filter stack — date / format / search /
    /// hidden / per-cinema — to a list of films. Both `/` (Filmy) and
    /// `/kina` go through this; they only differ in which set of
    /// cinemas is passed as `disabledCinemas` (Filtry's persistent set
    /// on Filmy, every-cinema-except-pinned on Kina).
    ///
    /// Semantics mirror the web's `applyFilters()`: drop a showtime
    /// whose format/time-from doesn't pass, drop a cinema-group whose
    /// every showtime fell out, drop a day whose every cinema-group
    /// fell out, drop a film whose every day fell out. A film stays
    /// visible as long as one badge somewhere still passes.
    func filteredFor(
        date: DateFilter,
        format: FormatFilter,
        query: String,
        hidden: Set<String>,
        disabledCinemas: Set<String>,
        excludedCountries: Set<String> = [],
        now: Date = Date()
    ) -> [Film] {
        let q = query.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        return self.compactMap { film in
            if hidden.contains(film.title) { return nil }
            if !q.isEmpty && !film.title.lowercased().contains(q) { return nil }
            if !excludedCountries.isEmpty && Set(film.countries).isSubset(of: excludedCountries) { return nil }
            let days: [DayShowings] = film.showings.compactMap { day in
                if !date.matches(date: day.date, now: now) { return nil }
                let cinemas: [CinemaShowings] = day.cinemas.compactMap { cg in
                    if disabledCinemas.contains(cg.cinema) { return nil }
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
                ratings: film.ratings,
                countries: film.countries,
                showings: days
            )
        }
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
                    ratings: film.ratings,
                    countries: film.countries,
                    showings: days
                ))
            }
        }
        return perCinema.keys.sorted().map { name in
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
        let f = DateFormatter()
        f.calendar = warsawCalendar
        f.timeZone = warsawCalendar.timeZone
        f.dateFormat = "yyyy-MM-dd HH:mm"
        f.locale = Locale(identifier: "en_US_POSIX")
        return f
    }()

    private static func warsawDate(date: String, time: String) -> Date? {
        warsawFormatter.date(from: "\(date) \(time)")
    }
}
