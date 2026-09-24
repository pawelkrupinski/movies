import Foundation
import XCTest
@testable import KinowoCore

/// The cache's serial queue is what a main-thread read (`lastModified`, `load`)
/// waits on, so it must hold only file I/O — never a whole-listing JSON encode.
/// The production save (`saveInBackground(body:)`, called by
/// `ConditionalListEndpoint` with the response it just received) guarantees that
/// by encoding nothing at all: the bytes on disk are the server's, as sent.
final class ConditionalPayloadCacheQueueTests: XCTestCase {

    private struct Item: Codable, Equatable { let title: String }

    private static let file = "conditional-payload-queue-test.json"
    private let cache = ConditionalPayloadCache<Item>(file: file)
    private let deployment = URL(string: "https://kinowo.net")!

    override func tearDown() {
        cache.remove()
        super.tearDown()
    }

    /// A field the model does not carry, and the server's own spacing: a save that
    /// decoded and re-encoded the listing — on the queue or anywhere — would drop
    /// the one and normalise the other.
    func testTheSaveStoresTheServersBytesWithoutEncodingThem() throws {
        let body = Data(#"[ {"title": "Diuna",  "notInTheModel": 1} ]"#.utf8)
        cache.saveInBackground(body: body, deployment: deployment, city: "poznan", lastModified: "x")

        // A read is ordered after the save, so the entry is on disk once it answers.
        XCTAssertEqual(cache.load(deployment: deployment, city: "poznan"), [Item(title: "Diuna")])
        let onDisk = try Data(contentsOf: FileManager.default
            .urls(for: .cachesDirectory, in: .userDomainMask)[0].appendingPathComponent(Self.file))
        XCTAssertEqual(onDisk.suffix(body.count), body)
    }
}
