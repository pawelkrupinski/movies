import Foundation

/// A name with how many loaded films carry it — one row of a Filtry name list
/// (country / genre / director / cast). Android's `NameCount`.
struct NameCount: Equatable {
    let name: String
    let count: Int
}

/// The value universe of the loaded repertoire's four name axes, each sorted by
/// descending frequency then name — the shape every Filtry name list expects,
/// and what a deep link's inclusion lists are inverted against. Android's
/// `FilterFacets`.
struct FilterFacets: Equatable {
    let countries: [NameCount]
    let genres: [NameCount]
    let directors: [NameCount]
    let cast: [NameCount]

    init(films: [Film]) {
        countries = Self.nameCounts(films) { $0.countries }
        genres = Self.nameCounts(films) { $0.genres }
        directors = Self.nameCounts(films) { $0.directors }
        cast = Self.nameCounts(films) { $0.cast }
    }

    /// How often each value appears across `films`, most frequent first, ties
    /// in case-insensitive name order.
    static func nameCounts(_ films: [Film], _ values: (Film) -> [String]) -> [NameCount] {
        var counts: [String: Int] = [:]
        for film in films {
            for value in values(film) {
                counts[value, default: 0] += 1
            }
        }
        return counts.map { NameCount(name: $0.key, count: $0.value) }
            .sorted {
                if $0.count != $1.count { return $0.count > $1.count }
                return $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending
            }
    }
}

/// The Filtry sheet's session axes — sort, format and the four name exclusions:
/// exactly what its Wyczyść button resets. Ephemeral, owned by `ContentView`;
/// the cinema picks and hidden films are persistent preferences and live in
/// `UserPreferences` instead.
struct ListingFilters: Equatable {
    var sort: SortOption = .earliest
    var format: FormatFilter = .empty
    var excludedCountries: Set<String> = []
    var excludedGenres: Set<String> = []
    var excludedDirectors: Set<String> = []
    var excludedCast: Set<String> = []

    static let empty = ListingFilters()

    /// `format` minus an IMAX pick `films` can't satisfy — see
    /// `FormatFilter.applicable(to:)`.
    func applicableFormat(to films: [Film]) -> FormatFilter { format.applicable(to: films) }

    /// Whether any axis Wyczyść clears narrows the listing — lights the Filtry
    /// button (see `ActiveFilters`). Sort orders rather than narrows, so it
    /// doesn't count.
    func isActive(over films: [Film]) -> Bool {
        ActiveFilters.any(
            format: applicableFormat(to: films),
            excludedCountries: excludedCountries,
            excludedGenres: excludedGenres,
            excludedDirectors: excludedDirectors,
            excludedCast: excludedCast
        )
    }

    /// The filtered, sorted listing of one day page — every `TabView` page
    /// from the same rule.
    func films(
        from all: [Film], date: DateFilter, query: String,
        hidden: Set<String>, disabledCinemas: Set<String>, zone: TimeZone
    ) -> [Film] {
        all.filteredFor(
            date: date,
            format: applicableFormat(to: all),
            query: query,
            hidden: hidden,
            disabledCinemas: disabledCinemas,
            excludedCountries: excludedCountries,
            excludedGenres: excludedGenres,
            excludedDirectors: excludedDirectors,
            excludedCast: excludedCast,
            zone: zone
        )
        .sorted(by: sort)
    }

    /// A deep link's sort and format axes, which need no repertoire. Absent
    /// axes leave the current choice alone.
    mutating func applyScalars(of link: DeepLinkFilters) {
        format = link.formatFilter(base: format)
        if let linkedSort = link.sort { sort = linkedSort }
    }

    /// A deep link's inclusion lists, inverted into exclusions against the
    /// loaded repertoire's `facets`. An axis the link doesn't name is left alone.
    mutating func applyInclusions(of link: DeepLinkFilters, facets: FilterFacets) {
        func excluded(_ included: [String], _ universe: [NameCount]) -> Set<String> {
            link.excluded(included, universe: Set(universe.map(\.name)))
        }
        if !link.includedCountries.isEmpty { excludedCountries = excluded(link.includedCountries, facets.countries) }
        if !link.includedGenres.isEmpty { excludedGenres = excluded(link.includedGenres, facets.genres) }
        if !link.includedDirectors.isEmpty { excludedDirectors = excluded(link.includedDirectors, facets.directors) }
        if !link.includedCast.isEmpty { excludedCast = excluded(link.includedCast, facets.cast) }
    }
}

extension Array where Element == Film {
    /// The film a deep link opens. A slug link resolves exactly (the server
    /// minted both sides from the same fold); a legacy `?title=` link matches
    /// the way the web's film lookup does — by normalized title (Arabic→Roman
    /// fold), so "…Prady 2" finds the stored "…Prady II". `nil` when the film
    /// has left the listing.
    func deepLinkTarget(_ link: DeepLink) -> Film? {
        link.filmSlug.flatMap { slug in first(where: { $0.slug == slug }) }
            ?? link.filmTitle.flatMap { title in first(where: { DeepLinkTitle.matches($0.title, title) }) }
    }
}
