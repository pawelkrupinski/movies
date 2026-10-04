import Foundation
@testable import KinowoCore

/// Test-only helpers: a scratch directory per test, and seeding an entry.
///
/// Seed a cache entry from a model value, through the production save: encoded
/// here, on the test's thread, as the server would have sent it, and on disk
/// when this returns — so a store built next, over its own cache instance of
/// the same file (the queue orders only one instance's accesses), reads it. Production has
/// no such call — `ConditionalListEndpoint` saves the response's own bytes — so
/// it lives with the tests. (KinowoNetworkingTests carries the same helper: the
/// two targets share no support module on Linux, where this one also builds.)
extension ConditionalPayloadCache {
    func save(_ payload: [Payload], deployment: URL, city: String, lastModified: String?) {
        guard let body = try? JSONEncoder().encode(payload) else { return }
        saveInBackground(body: body, deployment: deployment, city: city, lastModified: lastModified)
        queue.sync {}
    }
}

extension ConditionalPayloadCache {
    /// A fresh, empty directory of one test's own for its caches, under the
    /// temp directory — never the user's caches (`defaultDirectory`), which a
    /// concurrent `swift test` and the developer's own app share by file name.
    /// Remove it with `discardScratchDirectory(_:)` in `tearDown`.
    static func scratchDirectory() -> URL {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("ConditionalPayloadCache-\(UUID().uuidString)", isDirectory: true)
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        return directory
    }

    static func discardScratchDirectory(_ directory: URL) {
        try? FileManager.default.removeItem(at: directory)
    }
}
