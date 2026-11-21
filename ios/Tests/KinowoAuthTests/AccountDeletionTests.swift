import Foundation
import XCTest
import KinowoTestSupport
@testable import KinowoCore
@testable import KinowoAuth

/// `DELETE /api/me` answers 503 when the user store is unreachable and the account was NOT deleted
/// (the session is kept). Only a 2xx may read as deleted: anything else must leave `AuthService`
/// signed in, with its local data, so the user sees the failure and can try again.
final class AccountDeletionTests: XCTestCase {
    private let base = URL(string: "https://kinowo.test")!

    override func tearDown() {
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    private func deleted(answering statusCode: Int) async -> Bool {
        URLProtocolStub.handler = { _ in .init(statusCode: statusCode, headers: [:], body: Data()) }
        return await AccountDeletion.delete(session: URLProtocolStub.session(), base: base)
    }

    func testA2xxIsADeletedAccount() async {
        let result = await deleted(answering: 204)
        XCTAssertTrue(result)
    }

    func testA503IsNotADeletedAccount() async {
        let result = await deleted(answering: 503)
        XCTAssertFalse(result, "the server kept the account; the app must not show it as deleted")
    }

    func testAnyOtherStatusIsNotADeletedAccount() async {
        for status in [401, 403, 500] {
            let result = await deleted(answering: status)
            XCTAssertFalse(result, "\(status)")
        }
    }

    func testANetworkErrorIsNotADeletedAccount() async {
        URLProtocolStub.handler = nil // the stub fails every request it has no answer for
        let result = await AccountDeletion.delete(session: URLProtocolStub.session(), base: base)
        XCTAssertFalse(result)
    }

    func testItSendsTheDeleteRequest() async {
        let methods = LockedValue<[String?]>([])
        URLProtocolStub.handler = { request in
            methods.withLock { $0.append(request.httpMethod) }
            return .init(statusCode: 204, headers: [:], body: Data())
        }
        _ = await AccountDeletion.delete(session: URLProtocolStub.session(), base: base)
        XCTAssertEqual(URLProtocolStub.requestedURLs.map(\.path), ["/api/me"])
        XCTAssertEqual(methods.withLock { $0 }, ["DELETE"])
    }
}
