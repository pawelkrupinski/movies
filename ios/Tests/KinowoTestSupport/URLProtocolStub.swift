import Foundation
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

/// Intercepts every request on a session configured with it and answers from
/// `handler`, so a store or HTTP client can be driven against canned HTTP
/// responses without touching the network. Shared by every test target
/// through `KinowoTestSupport`.
public final class URLProtocolStub: URLProtocol {
    public struct Response: Sendable {
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

    /// Set by a test, read on URLSession's loading threads — so `@Sendable`,
    /// and kept behind a lock.
    public static var handler: (@Sendable (URLRequest) -> Response)? {
        get { installedHandler.withLock { $0 } }
        set {
            installedHandler.withLock { $0 = newValue }
            requestLog.withLock { $0 = [] }
        }
    }

    private static let installedHandler = LockedValue<(@Sendable (URLRequest) -> Response)?>(nil)
    private static let requestLog = LockedValue<[URL]>([])

    /// Every URL requested since `handler` was last set — lets a test wait
    /// for a request to be on the wire instead of sleeping and hoping.
    public static var requestedURLs: [URL] { requestLog.withLock { $0 } }

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
        Self.requestLog.withLock { $0.append(url) }
        let response = handler(request)
        let delivery = Delivery(stub: self, url: url, response: response)
        if response.delay > 0 {
            DispatchQueue.global().asyncAfter(deadline: .now() + response.delay) { delivery.run() }
        } else {
            delivery.run()
        }
    }

    override public func stopLoading() {}
}

/// One answer, carried to the thread that delivers it (a delayed answer leaves
/// the loading thread). Unchecked because swift-corelibs-foundation marks
/// `URLProtocol` non-Sendable. Safe: the stub keeps no state of its own (the
/// shared handler and log sit behind locks), and the `request` / `client` it
/// reads are fixed when URLSession creates it.
private struct Delivery: @unchecked Sendable {
    let stub: URLProtocolStub
    let url: URL
    let response: URLProtocolStub.Response

    func run() {
        let httpResponse = HTTPURLResponse(
            url: url, statusCode: response.statusCode,
            httpVersion: "HTTP/1.1", headerFields: response.headers)!
        stub.client?.urlProtocol(stub, didReceive: httpResponse, cacheStoragePolicy: .notAllowed)
        stub.client?.urlProtocol(stub, didLoad: response.body)
        stub.client?.urlProtocolDidFinishLoading(stub)
    }
}
