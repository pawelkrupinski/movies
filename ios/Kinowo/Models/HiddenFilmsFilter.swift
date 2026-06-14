import Foundation

/// Sort + search filtering for the "Ukryte filmy" screen, sharing its shape
/// with the Android HiddenFilmsCard so both list hidden titles the same way:
/// case-insensitive alphabetical, narrowed to titles containing the (trimmed)
/// query. An empty / whitespace-only query keeps every title.
enum HiddenFilmsFilter {
    static func visibleTitles(in hidden: Set<String>, query: String) -> [String] {
        let sorted = hidden.sorted {
            $0.localizedCaseInsensitiveCompare($1) == .orderedAscending
        }
        let q = query.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !q.isEmpty else { return sorted }
        return sorted.filter { $0.localizedCaseInsensitiveContains(q) }
    }
}
