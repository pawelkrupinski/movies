import XCTest
import KinowoTestSupport
@testable import KinowoAuth

/// `HttpHiddenFilmsClient`'s writes tell an edit the server refuses for good
/// from a failure worth resending, so `StateSyncService` can stop owing the
/// one and keep the other queued. Mirrors Android `HttpHiddenFilmsClientTest`.
final class HttpHiddenFilmsClientTests: XCTestCase {

    /// Only 400 (over-long title, unknown country) and 413 (full bucket) are
    /// refused for good; a 403 in particular is as likely a Cloudflare
    /// challenge in front of the app. Every status is a row of the repo's
    /// retry-classification table, which the web and Android hold their own
    /// rule to as well — for a hide, an unhide and a clear alike.
    func testEveryWriteSettlesEveryStatusAsTheRetryClassificationTableSays() async throws {
        try await assertEveryCallSettlesAsTheTableSays(
            source: "user-state:hidden-films-write",
            isPermanent: HiddenFilmsWriteRefused.isPermanent,
            refusedStatus: { ($0 as? HiddenFilmsWriteRefused)?.statusCode },
            calls: [
                ("hide", { _ = try await HttpHiddenFilmsClient(session: $0).hide(country: "pl", title: "Film") }),
                ("unhide", { _ = try await HttpHiddenFilmsClient(session: $0).unhide(country: "pl", title: "Film") }),
                ("clear", { _ = try await HttpHiddenFilmsClient(session: $0).clear(country: "pl") }),
            ])
    }
}
