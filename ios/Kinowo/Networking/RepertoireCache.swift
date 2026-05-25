import Foundation

enum RepertoireCache {
    private static var cacheURL: URL {
        FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("repertoire.json")
    }

    static func save(_ films: [Film]) {
        guard let data = try? JSONEncoder().encode(films) else { return }
        try? data.write(to: cacheURL, options: .atomic)
    }

    static func load() -> [Film]? {
        guard let data = try? Data(contentsOf: cacheURL) else { return nil }
        return try? JSONDecoder().decode([Film].self, from: data)
    }
}
