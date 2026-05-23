import XCTest
@testable import KinowoCore

#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

// Base class for tests that hit a live Play server boot of
// `FixtureServerMain`. The server is started by the
// `ios-local-server` GH Actions job (or by hand for local dev — see
// the LocalServer README) and writes its random port to a file the
// shell reads back into `KINOWO_LOCAL_URL`.
//
// Why a real server instead of bundled HTML fixtures: the bundled
// snapshots only catch parser drift; they don't catch server-side
// HTML changes until the snapshot is regenerated. Running against
// a live render closes that loop — a template change that breaks
// `HTMLParser.parse` fails CI on the same PR that introduced it.
//
// Skipping when `KINOWO_LOCAL_URL` is unset is the correct local-dev
// behaviour: `swift test` from `ios/` shouldn't require sbt running.
// CI sets the env var; local devs opt in by booting the server and
// exporting the URL.
class LocalServerTestCase: XCTestCase {

    /// Resolved from `KINOWO_LOCAL_URL`. Force-unwrapped because
    /// `setUpWithError` throws `XCTSkip` when the env var is missing,
    /// so test bodies only run when this is set.
    var baseURL: URL!

    override func setUpWithError() throws {
        guard let raw = ProcessInfo.processInfo.environment["KINOWO_LOCAL_URL"],
              let url = URL(string: raw) else {
            throw XCTSkip("set KINOWO_LOCAL_URL to run LocalServer tests (boot FixtureServerMain)")
        }
        self.baseURL = url
    }

    /// Synchronous fetch helper. Uses `URLSession.dataTask` +
    /// `DispatchSemaphore` rather than the async API because Swift
    /// 5.10's FoundationNetworking on Linux doesn't ship
    /// `URLSession.shared.data(for:)`. The completion-handler
    /// dataTask is present on both Darwin and Linux, so this one
    /// helper works in both CI containers.
    func fetchHTML(path: String) throws -> String {
        guard let url = URL(string: path, relativeTo: baseURL) else {
            throw URLError(.badURL)
        }
        var req = URLRequest(url: url)
        req.setValue("KinowoLocalServerTests/1.0", forHTTPHeaderField: "User-Agent")
        req.cachePolicy = .reloadIgnoringLocalCacheData

        let semaphore = DispatchSemaphore(value: 0)
        var captured: Result<String, Error> = .failure(URLError(.badServerResponse))
        URLSession.shared.dataTask(with: req) { data, response, error in
            defer { semaphore.signal() }
            if let error = error {
                captured = .failure(error)
                return
            }
            if let http = response as? HTTPURLResponse, !(200..<300).contains(http.statusCode) {
                captured = .failure(URLError(.badServerResponse))
                return
            }
            guard let data = data else {
                captured = .failure(URLError(.badServerResponse))
                return
            }
            captured = .success(String(decoding: data, as: UTF8.self))
        }.resume()
        // Localhost over loopback; 30s is generous for any real fail mode.
        _ = semaphore.wait(timeout: .now() + .seconds(30))
        return try captured.get()
    }
}
