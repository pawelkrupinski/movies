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

    static let empty = CinemaCatalog(cinemas: [], areas: [])

    /// Whether the city is split into areas (vs. a flat cinema list).
    var isSplit: Bool { !areas.isEmpty }
}

/// One area group in a split city: display name, stable slug, and the full
/// display names of the venues it holds. Mirrors web `ApiCinemaArea`.
struct CinemaArea: Codable, Hashable, Identifiable {
    let name: String
    let slug: String
    let cinemas: [String]

    var id: String { slug }
}
