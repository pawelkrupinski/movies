import Foundation

/// On-disk cache for `/api/details`, parallel to `RepertoireCache` — same
/// deployment+city-bound rule (see `ConditionalPayloadCache`). Separate files
/// (`details.json` / `details-meta.txt`) so the two endpoints' conditional-GET
/// state never collides.
enum DetailsCache {
    private static let store = ConditionalPayloadCache<FilmDetails>(
        bodyFile: "details.json", metaFile: "details-meta.txt"
    )

    static func save(_ details: [FilmDetails], deployment: URL, city: String, lastModified: String?) {
        store.save(details, deployment: deployment, city: city, lastModified: lastModified)
    }

    static func load(deployment: URL, city: String) -> [FilmDetails]? {
        store.load(deployment: deployment, city: city)
    }

    static func bodyForNotModified(callerIsEmpty: Bool, deployment: URL, city: String) -> [FilmDetails]? {
        store.bodyForNotModified(callerIsEmpty: callerIsEmpty, deployment: deployment, city: city)
    }

    static func lastModified(deployment: URL, city: String) -> String? {
        store.lastModified(deployment: deployment, city: city)
    }
}
