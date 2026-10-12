import Foundation
#if canImport(KinowoCore)
@testable import KinowoCore
#endif

/// The requests `AuthService` sends, built here — outside the app-only service, which needs
/// AuthenticationServices — so a test can see every one of them carries the app's User-Agent.
enum AuthRequests {
    static func exchange(base: URL, body: Data) -> URLRequest {
        json(request(base, "auth/exchange", method: "POST"), body)
    }

    static func token(base: URL, body: Data) -> URLRequest {
        json(request(base, "auth/token", method: "POST"), body)
    }

    static func me(base: URL) -> URLRequest { request(base, "api/me") }

    static func logout(base: URL) -> URLRequest { request(base, "auth/logout", method: "POST") }

    static func deleteAccount(base: URL) -> URLRequest { request(base, "api/me", method: "DELETE") }

    private static func request(_ base: URL, _ path: String, method: String = "GET") -> URLRequest {
        var request = URLRequest(url: base.appendingPathComponent(path))
        request.httpMethod = method
        request.setValue(KinowoUserAgent.value, forHTTPHeaderField: "User-Agent")
        return request
    }

    private static func json(_ request: URLRequest, _ body: Data) -> URLRequest {
        var request = request
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = body
        return request
    }
}
