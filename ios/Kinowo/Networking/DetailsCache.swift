import Foundation

/// On-disk cache for `/api/details`, parallel to `RepertoireCache` — same
/// city-bound `Last-Modified` rule (see `RepertoireCache` for why the global
/// server timestamp must not be replayed across a city switch). Separate files
/// (`details.json` / `details-meta.txt`) so the two endpoints' conditional-GET
/// state never collides.
enum DetailsCache {
    private static var cacheDir: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
    }
    private static var cacheURL: URL { cacheDir.appendingPathComponent("details.json") }
    // line 1: city slug the body was fetched for; line 2: its Last-Modified.
    private static var metaURL: URL { cacheDir.appendingPathComponent("details-meta.txt") }

    /// Persist the freshly-fetched `details` for `city` together with its
    /// `lastModified` header, so a later same-city reload can revalidate.
    static func save(_ details: [FilmDetails], city: String, lastModified: String?) {
        if let data = try? JSONEncoder().encode(details) {
            try? data.write(to: cacheURL, options: .atomic)
        }
        let meta = city + "\n" + (lastModified ?? "")
        try? meta.write(to: metaURL, atomically: true, encoding: .utf8)
    }

    static func load() -> [FilmDetails]? {
        guard let data = try? Data(contentsOf: cacheURL) else { return nil }
        return try? JSONDecoder().decode([FilmDetails].self, from: data)
    }

    /// The `Last-Modified` to replay as `If-Modified-Since`, but only when the
    /// cached body belongs to `city`; nil for any other city.
    static func lastModified(forCity city: String) -> String? {
        guard let meta = try? String(contentsOf: metaURL, encoding: .utf8) else { return nil }
        let lines = meta.components(separatedBy: "\n")
        guard let cachedCity = lines.first, cachedCity == city else { return nil }
        let value = lines.count > 1 ? lines[1] : ""
        return value.isEmpty ? nil : value
    }
}
