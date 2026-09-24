import XCTest
import KinowoTestSupport
@testable import KinowoAuth

/// `HttpHiddenFilmsClient`'s writes tell an edit the server refuses for good
/// from a failure worth resending, so `StateSyncService` can stop owing the
/// one and keep the other queued. Mirrors Android `HttpHiddenFilmsClientTest`.
final class HttpHiddenFilmsClientTests: XCTestCase {

    override func tearDown() {
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    private func client(answering statusCode: Int) -> HttpHiddenFilmsClient {
        URLProtocolStub.handler = { _ in .init(statusCode: statusCode, headers: [:], body: Data()) }
        return HttpHiddenFilmsClient(session: URLProtocolStub.session())
    }

    /// Only 400 (over-long title, unknown country) and 413 (full bucket) are
    /// refused for good; a 403 in particular is as likely a Cloudflare
    /// challenge in front of the app. Every status is a row of the repo's
    /// retry-classification table, which the web and Android hold their own
    /// rule to as well — for a hide, an unhide and a clear alike.
    func testEveryWriteSettlesEveryStatusAsTheRetryClassificationTableSays() async throws {
        let table = try RetryClassificationTable.load()
        XCTAssertTrue(table.sources(consumedBy: "ios").contains("user-state:hidden-films-write"))
        let rows = try table.rows(for: "user-state:hidden-films-write")
        XCTAssertFalse(rows.isEmpty)
        let writes: [(String, (HttpHiddenFilmsClient) async throws -> HiddenFilmsResult)] = [
            ("hide", { try await $0.hide(country: "pl", title: "Film") }),
            ("unhide", { try await $0.unhide(country: "pl", title: "Film") }),
            ("clear", { try await $0.clear(country: "pl") }),
        ]
        for row in rows {
            let status = try XCTUnwrap(row.status, "\(row)")
            XCTAssertEqual(HiddenFilmsWriteRefused.isPermanent(status), row.isPermanent, "\(row)")
            for (name, write) in writes {
                do {
                    _ = try await write(client(answering: status))
                    XCTFail("a \(status) must not read as a successful \(name)")
                } catch {
                    XCTAssertEqual(error is HiddenFilmsWriteRefused, row.isPermanent, "\(name): \(row)")
                    if row.isPermanent { XCTAssertEqual((error as? HiddenFilmsWriteRefused)?.statusCode, status) }
                }
            }
        }
    }
}
