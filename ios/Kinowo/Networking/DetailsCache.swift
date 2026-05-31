import Foundation

/// On-disk cache for `/api/details`, parallel to `RepertoireCache`.
/// Separate files (`details.json` / `details-lastmodified.txt`) so the
/// two endpoints' conditional-GET state never collides.
enum DetailsCache {
    private static var cacheURL: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("details.json")
    }

    static func save(_ details: [FilmDetails]) {
        guard let data = try? JSONEncoder().encode(details) else { return }
        try? data.write(to: cacheURL, options: .atomic)
    }

    static func load() -> [FilmDetails]? {
        guard let data = try? Data(contentsOf: cacheURL) else { return nil }
        return try? JSONDecoder().decode([FilmDetails].self, from: data)
    }

    private static var lastModifiedURL: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("details-lastmodified.txt")
    }

    static func saveLastModified(_ value: String) {
        try? value.write(to: lastModifiedURL, atomically: true, encoding: .utf8)
    }

    static func loadLastModified() -> String? {
        try? String(contentsOf: lastModifiedURL, encoding: .utf8)
    }
}
