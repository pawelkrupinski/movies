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

    /// A 400 (a language this server does not know) is refused for good — a
    /// different error from one worth retrying, so the caller can stop.
    func testPushReportsAPermanentRefusalAsSuch() async {
        do {
            try await client(answering: 400).push("it")
            XCTFail("a 400 must not read as a successful push")
        } catch {
            XCTAssertEqual((error as? LanguagePushRefused)?.statusCode, 400)
        }
    }

    func testPushReportsARetryableFailureAsRetryable() async {
        // 403 included: a Cloudflare challenge in front of the app is a 403 too.
        for status in [401, 403, 404, 408, 422, 429, 503] {
            do {
                try await client(answering: status).push("de")
                XCTFail("a \(status) must not read as a successful push")
            } catch {
                XCTAssertFalse(error is LanguagePushRefused, "\(status) is worth retrying")
            }
        }
    }

    func testPushSucceedsOnA2xx() async throws {
        try await client(answering: 204).push("de")
    }
}
