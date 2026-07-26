import Foundation

/// On-disk cache of the repertoire payload — see `ConditionalPayloadCache` for
/// why an entry is bound to the deployment AND the city that produced it.
enum RepertoireCache {
    private static let store = ConditionalPayloadCache<Film>(
        bodyFile: "repertoire.json", metaFile: "repertoire-meta.txt"
    )

    static func save(_ films: [Film], deployment: URL, city: String, lastModified: String?) {
        store.save(films, deployment: deployment, city: city, lastModified: lastModified)
    }

    static func load(deployment: URL, city: String) -> [Film]? {
        store.load(deployment: deployment, city: city)
    }

    static func bodyForNotModified(callerIsEmpty: Bool, deployment: URL, city: String) -> [Film]? {
        store.bodyForNotModified(callerIsEmpty: callerIsEmpty, deployment: deployment, city: city)
    }

    static func lastModified(deployment: URL, city: String) -> String? {
        store.lastModified(deployment: deployment, city: city)
    }
}
