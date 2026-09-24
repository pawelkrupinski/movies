import Foundation
import XCTest
@testable import KinowoCore

/// The cache's serial queue is what a main-thread read (`lastModified`, `load`)
/// waits on, so it must hold only file I/O — never the whole-listing JSON
/// encode a save pays for, or a launch-time read stalls behind a save of the
/// previous listing.
final class ConditionalPayloadCacheQueueTests: XCTestCase {

    /// A payload whose encode blocks until the test lets it go.
    private struct BlockingPayload: Codable {
        static let encodeStarted = DispatchSemaphore(value: 0)
        static let releaseEncode = DispatchSemaphore(value: 0)

        init() {}
        init(from decoder: Decoder) throws {}
        func encode(to encoder: Encoder) throws {
            Self.encodeStarted.signal()
            Self.releaseEncode.wait()
        }
    }

    private let cache = ConditionalPayloadCache<BlockingPayload>(file: "conditional-payload-queue-test.json")
    private let deployment = URL(string: "https://kinowo.net")!

    override func tearDown() {
        cache.remove()
        super.tearDown()
    }

    func testAReadDoesNotWaitOnAnotherSavesEncode() {
        let saved = expectation(description: "the save finished")
        DispatchQueue.global().async {
            self.cache.save([BlockingPayload()], deployment: self.deployment, city: "poznan", lastModified: "x")
            saved.fulfill()
        }
        XCTAssertEqual(BlockingPayload.encodeStarted.wait(timeout: .now() + 5), .success)

        let read = expectation(description: "the read returned while the encode was still running")
        DispatchQueue.global().async {
            _ = self.cache.lastModified(deployment: self.deployment, city: "poznan")
            read.fulfill()
        }
        wait(for: [read], timeout: 2)

        BlockingPayload.releaseEncode.signal()
        wait(for: [saved], timeout: 5)
    }
}
