import Foundation

/// Intercepts every request on a session configured with it and answers from
/// `handler`, so `RepertoireStore.reload()` can be driven against canned
/// HTTP responses without touching the network.
final class URLProtocolStub: URLProtocol {
    struct Response {
        let statusCode: Int
        let headers: [String: String]
        let body: Data
        /// Answer this long after the request starts, off the loading
        /// thread, so other requests on the same session aren't held up.
        var delay: TimeInterval = 0
    }

    static var handler: ((URLRequest) -> Response)?

    override class func canInit(with request: URLRequest) -> Bool { true }
    override class func canonicalRequest(for request: URLRequest) -> URLRequest { request }

    override func startLoading() {
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

    override func stopLoading() {}
}
