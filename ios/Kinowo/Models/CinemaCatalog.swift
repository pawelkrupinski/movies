import Foundation

/// A city's cinema universe plus its optional area grouping, as served by
/// `GET /:city/api/cinemas` (mirrors the web `ApiCityCinemas`). A **flat** city
/// returns an empty `areas`; a **split** city (e.g. London) returns one entry
/// per area, together partitioning `cinemas`. iOS shows the multi-select area
/// panel only when `isSplit` — flat cities keep the single-select pill bar.
///
/// Pure Foundation → lives in `KinowoCore` and decodes cross-platform under
/// `swift test`.
struct CinemaCatalog: Codable, Hashable {
    /// Every venue in the city, in city order (the full universe — includes
    /// venues with no showings today, so the picker is stable).
    let cinemas: [String]
    /// The area grouping, or empty for a flat city.
    let areas: [CinemaArea]
    /// The city's seasonal venues (open-air, drive-in) — absent from the payload
    /// when it has none, and from servers older than the field.
    var seasons: [CinemaSeason]?

    static let empty = CinemaCatalog(cinemas: [], areas: [])

    /// Whether the city is split into areas (vs. a flat cinema list).
    var isSplit: Bool { !areas.isEmpty }

    /// The season of `cinema`, when it is a seasonal venue.
    func season(of cinema: String) -> CinemaSeason? {
        seasons?.first { $0.cinema == cinema }
    }

    /// Whether `cinema` is out of season today — the SERVER's verdict, on its own
    /// clock, so the app never disagrees with the web about it.
    func isOffSeason(_ cinema: String) -> Bool {
        season(of: cinema)?.offSeason ?? false
    }

    /// `cinemas` with the out-of-season ones moved to the bottom, order otherwise kept —
    /// how the filter lists a flat city, and each area of a split one.
    func seasonOrdered(_ cinemas: [String]) -> [String] {
        cinemas.filter { !isOffSeason($0) } + cinemas.filter { isOffSeason($0) }
    }

    /// The cinemas to EXCLUDE when only the areas in [keepingAreas] (slugs) are
    /// kept — every cinema in an unchecked area. Drives the first-visit area
    /// picker: the areas the user leaves checked stay shown, the rest are added
    /// to `disabledCinemas`. Areas are a partition, so this is unambiguous.
    func cinemasToDisable(keepingAreas: Set<String>) -> [String] {
        areas.filter { !keepingAreas.contains($0.slug) }.flatMap(\.cinemas)
    }
}

/// One area group in a split city: display name, stable slug, and the full
/// display names of the venues it holds. Mirrors web `ApiCinemaArea`.
struct CinemaArea: Codable, Hashable, Identifiable {
    let name: String
    let slug: String
    let cinemas: [String]

    var id: String { slug }
}

/// A seasonal venue's typical window, months 1...12 both inclusive, and whether
/// today is outside it (decided by the server). Mirrors web `ApiCinemaSeason`.
struct CinemaSeason: Codable, Hashable {
    let cinema: String
    let opens: Int
    let closes: Int
    let offSeason: Bool

    /// The filter row's muted second line, "back in season · May–Sept", in the
    /// app's UI `language` ("pl", "en", "de", "es"; anything else reads English).
    /// A month outside 1...12 drops the window, leaving the bare "back in season".
    func backInSeasonLine(language: String) -> String {
        let words = SeasonWords.of(language)
        guard let from = words.month(opens), let to = words.month(closes) else { return words.back }
        return "\(words.back) · \(from)–\(to)"
    }
}

/// The words a season is spelled in, per UI language — kept identical to the web's
/// `season.back` / `season.month.N` messages and Android's `SeasonWords`.
struct SeasonWords {
    let back: String
    let months: [String]

    /// The short name of `month` (1...12), or nil for any other number.
    func month(_ month: Int) -> String? {
        months.indices.contains(month - 1) ? months[month - 1] : nil
    }

    static func of(_ language: String) -> SeasonWords {
        switch language {
        case "pl": return SeasonWords(back: "wraca w sezonie",
                                      months: ["sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paź", "lis", "gru"])
        case "de": return SeasonWords(back: "öffnet wieder",
                                      months: ["Jan.", "Feb.", "März", "Apr.", "Mai", "Juni", "Juli", "Aug.", "Sept.", "Okt.", "Nov.", "Dez."])
        case "es": return SeasonWords(back: "vuelve en temporada",
                                      months: ["ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sept", "oct", "nov", "dic"])
        default:   return SeasonWords(back: "back in season",
                                      months: ["Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"])
        }
    }
}

/// The area picker's checkbox state: which of a split city's areas are kept.
/// Seeded to ALL of them (the pre-selected default), so tapping straight
/// through the sheet shows everything.
///
/// Lives beside the catalog rather than as `@State` inside `AreaPickerSheet` so
/// the select-all / deselect-all rules are testable under `swift test` — the
/// sheet is SwiftUI and out of `KinowoCore`. Mirrors the web picker's master
/// checkbox (`#area-picker-all` in `shared.js`) and Android's `AreaPickerDialog`.
struct AreaSelection: Hashable {
    /// The slugs currently checked.
    private(set) var kept: Set<String>
    /// Every area's slug, in the catalog's order — the universe `kept` is a
    /// subset of, and what "select all" restores.
    let all: [String]

    init(areas: [CinemaArea]) {
        all = areas.map(\.slug)
        kept = Set(all)
    }

    /// Every area is kept — the master row's checked state.
    var allKept: Bool { kept.count == all.count }
    /// Some but not all — the master row's indeterminate/mixed state.
    var partial: Bool { !kept.isEmpty && !allKept }

    func isKept(_ slug: String) -> Bool { kept.contains(slug) }

    mutating func toggle(_ slug: String) {
        if kept.contains(slug) { kept.remove(slug) } else { kept.insert(slug) }
    }

    /// Select all / deselect all. A tap on a mixed master row selects
    /// everything first — what a user reaching for it while some are ticked
    /// means — which is what `!allKept` gives.
    mutating func setAll(_ on: Bool) { kept = on ? Set(all) : [] }
}
