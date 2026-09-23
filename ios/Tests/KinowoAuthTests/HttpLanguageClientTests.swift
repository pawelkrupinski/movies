import XCTest
@testable import KinowoAuth

/// `HttpLanguageClient.push` must report a rejected PUT as a failure — a
/// 401/500 treated as success would drop the pick for good instead of
/// leaving it for `StateSyncService` to retry.
final class HttpLanguageClientTests: XCTestCase {

    override func tearDown() {
        StatusOnlyURLProtocol.statusCode = 200
        super.tearDown()
    }

    private func client() -> HttpLanguageClient {
        let config = URLSessionConfiguration.ephemeral
        config.protocolClasses = [StatusOnlyURLProtocol.self]
        return HttpLanguageClient(session: URLSession(configuration: config))
    }

    func testPushThrowsWhenTheServerRejectsIt() async {
        StatusOnlyURLProtocol.statusCode = 401
        do {
            try await client().push("de")
            XCTFail("a 401 must not read as a successful push")
        } catch {}
    }

    func testPushSucceedsOnA2xx() async throws {
        StatusOnlyURLProtocol.statusCode = 204
        try await client().push("de")
    }
}

/// Answers every request with an empty body and `statusCode`.
final class StatusOnlyURLProtocol: URLProtocol {
    static var statusCode = 200

    override class func canInit(with request: URLRequest) -> Bool { true }
    override class func canonicalRequest(for request: URLRequest) -> URLRequest { request }

    override func startLoading() {
        let response = HTTPURLResponse(url: request.url!, statusCode: Self.statusCode, httpVersion: "HTTP/1.1", headerFields: [:])!
        client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
        client?.urlProtocol(self, didLoad: Data())
        client?.urlProtocolDidFinishLoading(self)
    }

    override func stopLoading() {}
}
