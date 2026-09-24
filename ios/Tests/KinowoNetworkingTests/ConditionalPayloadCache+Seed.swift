import Foundation
@testable import KinowoCore

/// Seed a cache entry from a model value, through the production save: encoded
/// here, on the test's thread, as the server would have sent it. Production has
/// no such call — `ConditionalListEndpoint` saves the response's own bytes — so
/// it lives with the tests. (KinowoCoreTests carries the same helper: the
/// two targets share no support module on Linux, where this one also builds.)
extension ConditionalPayloadCache {
    func save(_ payload: [Payload], deployment: URL, city: String, lastModified: String?) {
        guard let body = try? JSONEncoder().encode(payload) else { return }
        saveInBackground(body: body, deployment: deployment, city: city, lastModified: lastModified)
    }
}
