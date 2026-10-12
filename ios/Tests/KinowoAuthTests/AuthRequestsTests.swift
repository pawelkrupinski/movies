import Foundation
import XCTest
@testable import KinowoAuth

/// Every auth request carries the app's User-Agent: without it the server's route-usage stats file
/// the app's sign-in, token, logout and account-deletion calls under "other", and an endpoint only
/// the app calls can look retirable.
final class AuthRequestsTests: XCTestCase {
    private let base = URL(string: "https://kinowo.test")!

    func testEveryAuthRequestCarriesTheAppUserAgent() {
        let requests: [(String, URLRequest)] = [
            ("exchange", AuthRequests.exchange(base: base, body: Data("{}".utf8))),
            ("token", AuthRequests.token(base: base, body: Data("{}".utf8))),
            ("me", AuthRequests.me(base: base)),
            ("logout", AuthRequests.logout(base: base)),
            ("deleteAccount", AuthRequests.deleteAccount(base: base)),
        ]
        for (name, request) in requests {
            XCTAssertEqual(request.value(forHTTPHeaderField: "User-Agent"), "KinowoIOS/1.0", name)
        }
    }

    func testEachRequestHitsItsRouteWithItsMethod() {
        XCTAssertEqual(AuthRequests.exchange(base: base, body: Data()).url?.path, "/auth/exchange")
        XCTAssertEqual(AuthRequests.token(base: base, body: Data()).httpMethod, "POST")
        XCTAssertEqual(AuthRequests.me(base: base).httpMethod, "GET")
        XCTAssertEqual(AuthRequests.logout(base: base).url?.path, "/auth/logout")
        XCTAssertEqual(AuthRequests.deleteAccount(base: base).httpMethod, "DELETE")
        XCTAssertEqual(AuthRequests.deleteAccount(base: base).url?.path, "/api/me")
    }

    func testABodyIsSentAsJson() {
        let request = AuthRequests.token(base: base, body: Data("{\"a\":1}".utf8))
        XCTAssertEqual(request.value(forHTTPHeaderField: "Content-Type"), "application/json")
        XCTAssertEqual(request.httpBody, Data("{\"a\":1}".utf8))
    }
}
