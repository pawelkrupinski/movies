import Foundation

/// On-disk cache of the repertoire payload plus the city + `Last-Modified` it
/// was fetched for, so a cold start paints instantly off disk and a warm reload
/// of the *same* city issues a conditional GET.
///
/// The cached `Last-Modified` is bound to the city it came from: the server
/// stamps a single *global* timestamp regardless of city, so replaying one
/// city's value as `If-Modified-Since` for another city would draw a 304 and
/// strand the grid on the old city's films. `lastModified(forCity:)` therefore
/// only hands the timestamp back for the city that produced it — a switch sends
/// no conditional header and always gets a fresh 200.
enum RepertoireCache {
    private static var cacheDir: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    }
    private static var cacheURL: URL { cacheDir.appendingPathComponent("repertoire.json") }
    // line 1: city slug the body was fetched for; line 2: its Last-Modified.
    private static var metaURL: URL { cacheDir.appendingPathComponent("repertoire-meta.txt") }

    /// Persist the freshly-fetched `films` for `city` together with its
    /// `lastModified` header, so a later same-city reload can revalidate.
    static func save(_ films: [Film], city: String, lastModified: String?) {
        if let data = try? JSONEncoder().encode(films) {
            try? data.write(to: cacheURL, options: .atomic)
        }
        let meta = city + "\n" + (lastModified ?? "")
        try? meta.write(to: metaURL, atomically: true, encoding: .utf8)
    }

    static func load() -> [Film]? {
        guard let data = try? Data(contentsOf: cacheURL) else { return nil }
        return try? JSONDecoder().decode([Film].self, from: data)
    }

    /// The `Last-Modified` to replay as `If-Modified-Since`, but only when the
    /// cached body belongs to `city`; nil for any other city (forcing a full
    /// fetch on a city switch — see the type note on the global timestamp).
    static func lastModified(forCity city: String) -> String? {
        guard let meta = try? String(contentsOf: metaURL, encoding: .utf8) else { return nil }
        let lines = meta.components(separatedBy: "\n")
        guard let cachedCity = lines.first, cachedCity == city else { return nil }
        let value = lines.count > 1 ? lines[1] : ""
        return value.isEmpty ? nil : value
    }
}
