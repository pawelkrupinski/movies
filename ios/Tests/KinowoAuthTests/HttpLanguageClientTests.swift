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

    /// Only a 400 (a language this server does not know) is refused for good —
    /// a different error from one worth retrying, so the caller can stop. A 403
    /// in particular is as likely a Cloudflare challenge in front of the app.
    /// Every status is a row of the repo's retry-classification table, which
    /// the web and Android hold their own rule to as well.
    func testPushSettlesEveryStatusAsTheRetryClassificationTableSays() async throws {
        let table = try RetryClassificationTable.load()
        XCTAssertTrue(table.sources(consumedBy: "ios").contains("user-state:language-push"))
        let rows = try table.rows(for: "user-state:language-push")
        XCTAssertFalse(rows.isEmpty)
        for row in rows {
            let status = try XCTUnwrap(row.status, "\(row)")
            XCTAssertEqual(LanguagePushRefused.isPermanent(status), row.isPermanent, "\(row)")
            do {
                try await client(answering: status).push("de")
                XCTFail("a \(status) must not read as a successful push")
            } catch {
                XCTAssertEqual(error is LanguagePushRefused, row.isPermanent, "\(row)")
                if row.isPermanent { XCTAssertEqual((error as? LanguagePushRefused)?.statusCode, status) }
            }
        }
    }

    func testPushSucceedsOnA2xx() async throws {
        try await client(answering: 204).push("de")
    }
}
