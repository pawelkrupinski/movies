import Foundation

extension Array where Element == City {

    /// Every region, subregion and city in `countryCode` matching `query`,
    /// merged into ONE alphabetical list — a query for "kent" must surface the
    /// Kent COUNTY heading even from the UK's top level, not only once
    /// England is already open. Empty for a blank query: the per-level
    /// `topLevelRows`/`secondLevelRows` above handle that case, and a country
    /// with no grouping (Poland, Spain) has nothing to flatten in the first
    /// place — its own `matching(_:inCountry:)` already reaches every city.
    func searchRows(matching query: String, inCountry countryCode: String) -> [City.SearchRow] {
        let q = City.searchFold(query.trimmingCharacters(in: .whitespaces))
        guard !q.isEmpty else { return [] }
        var seenRegion = Set<String>()
        var seenSubregion = Set<String>()
        var rows: [City.SearchRow] = []
        for city in inCountry(countryCode) {
            if let region = city.region {
                rows += headingRows(of: city, region: region, matching: q, seenRegion: &seenRegion, seenSubregion: &seenSubregion)
            }
            if city.matches(folded: q) { rows.append(.city(city)) }
        }
        let locale = City.collationLocale(for: countryCode)
        return rows.sorted { $0.label.compare($1.label, options: .caseInsensitive, range: nil, locale: locale) == .orderedAscending }
    }

    /// The region and subregion headings of `city` that match `q`, not yet listed.
    private func headingRows(of city: City, region: String, matching q: String,
                             seenRegion: inout Set<String>, seenSubregion: inout Set<String>) -> [City.SearchRow] {
        var rows: [City.SearchRow] = []
        if seenRegion.insert(region).inserted, City.searchFold(region).contains(q) { rows.append(.region(region)) }
        if let subregion = city.subregion, seenSubregion.insert("\(region)\u{0}\(subregion)").inserted,
           City.searchFold(subregion).contains(q) { rows.append(.subregion(subregion, region: region)) }
        return rows
    }
}
