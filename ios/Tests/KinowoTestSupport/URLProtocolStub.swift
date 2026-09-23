import Foundation

/// Intercepts every request on a session configured with it and answers from
/// `handler`, so a store or HTTP client can be driven against canned HTTP
/// responses without touching the network. Shared by every test target
/// through `KinowoTestSupport`.
public final class URLProtocolStub: URLProtocol {
    public struct Response {
        public let statusCode: Int
        public let headers: [String: String]
        public let body: Data
        /// Answer this long after the request starts, off the loading
        /// thread, so other requests on the same session aren't held up.
        public var delay: TimeInterval = 0

        public init(statusCode: Int, headers: [String: String], body: Data, delay: TimeInterval = 0) {
            self.statusCode = statusCode
            self.headers = headers
            self.body = body
            self.delay = delay
        }
    }

    public static var handler: ((URLRequest) -> Response)?

    /// An ephemeral session whose every request this stub answers.
    public static func session() -> URLSession {
        let config = URLSessionConfiguration.ephemeral
        config.protocolClasses = [URLProtocolStub.self]
        return URLSession(configuration: config)
    }

    override public class func canInit(with request: URLRequest) -> Bool { true }
    override public class func canonicalRequest(for request: URLRequest) -> URLRequest { request }

    override public func startLoading() {
        guard let handler = URLProtocolStub.handler, let url = request.url else {
            client?.urlProtocol(self, didFailWithError: URLError(.badServerResponse))
            return
        }
        let response = handler(request)
        let httpResponse = HTTPURLResponse(
            url: url, statusCode: response.statusCode,
            httpVersion: "HTTP/1.1", headerFields: response.headers)!
        let deliver = {
            self.client?.urlProtocol(self, didReceive: httpResponse, cacheStoragePolicy: .notAllowed)
            self.client?.urlProtocol(self, didLoad: response.body)
            self.client?.urlProtocolDidFinishLoading(self)
        }
        if response.delay > 0 {
            DispatchQueue.global().asyncAfter(deadline: .now() + response.delay, execute: deliver)
        } else {
            deliver()
        }
    }

    override public func stopLoading() {}
}
