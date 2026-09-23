import XCTest
import KinowoTestSupport
@testable import KinowoAuth

/// `HttpLanguageClient.push` must report a rejected PUT as a failure — a
/// 401/500 treated as success would drop the pick for good instead of
/// leaving it for `StateSyncService` to retry.
final class HttpLanguageClientTests: XCTestCase {

    override func tearDown() {
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    private func client(answering statusCode: Int) -> HttpLanguageClient {
        URLProtocolStub.handler = { _ in .init(statusCode: statusCode, headers: [:], body: Data()) }
        return HttpLanguageClient(session: URLProtocolStub.session())
    }

    func testPushThrowsWhenTheServerRejectsIt() async {
        do {
            try await client(answering: 401).push("de")
            XCTFail("a 401 must not read as a successful push")
        } catch {}
    }

    func testPushSucceedsOnA2xx() async throws {
        try await client(answering: 204).push("de")
    }
}
