import Foundation

enum FormatTokenFilter {

    static func commonTokens(_ cinema: CinemaShowings) -> Set<String> {
        let tokenSets = cinema.showtimes
            .map { Set($0.format.split(separator: " ").map(String.init)) }
            .filter { !$0.isEmpty }
        guard let first = tokenSets.first else { return [] }
        return tokenSets.dropFirst().reduce(first) { $0.intersection($1) }
    }

    static func filter(_ format: String, removing common: Set<String>) -> String {
        guard !common.isEmpty else { return format }
        return format.split(separator: " ")
            .filter { !common.contains(String($0)) }
            .joined(separator: " ")
    }
}
