import XCTest
@testable import KinowoCore

#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

// Base class for tests that hit a live Play server boot of
// `FixtureServerMain`. The server is started by the
// `mobile-local-server` GH Actions job (or by hand for local dev — see
// the "LocalServer" lane in `ios/README.md`) and writes its random port
// to a file the shell reads back into `KINOWO_LOCAL_URL`.
//
// Why a real server instead of bundled JSON fixtures: the bundled
// snapshots only catch client-side decoder drift; they don't catch a
// server-side JSON shape change until the snapshot is regenerated.
// Running against a live render closes that loop — a `MovieController`
// change that breaks `JSONDecoder().decode([Film].self, …)` fails CI on
// the same PR that introduced it.
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

    /// One fetched API response: the body and the headers the app's
    /// conditional-request path reads back (`Last-Modified`).
    struct Fetched {
        let data: Data
        let response: HTTPURLResponse

        var lastModified: String? { response.value(forHTTPHeaderField: "Last-Modified") }
    }

    /// Synchronous GET of `url`, sent the way `RepertoireStore` /
    /// `DetailsStore` send theirs (same User-Agent, cache bypass). Uses
    /// `URLSession.dataTask` + `DispatchSemaphore` rather than the async
    /// API because Swift 5.10's FoundationNetworking on Linux doesn't ship
    /// `URLSession.shared.data(for:)`. The completion-handler dataTask is
    /// present on both Darwin and Linux, so this one helper works in both
    /// CI containers.
    func fetch(_ url: URL) throws -> Fetched {
        var request = URLRequest(url: url)
        request.setValue("KinowoIOS/1.0", forHTTPHeaderField: "User-Agent")
        request.cachePolicy = .reloadIgnoringLocalCacheData

        let semaphore = DispatchSemaphore(value: 0)
        var captured: Result<Fetched, Error> = .failure(URLError(.badServerResponse))
        URLSession.shared.dataTask(with: request) { data, response, error in
            defer { semaphore.signal() }
            if let error = error {
                captured = .failure(error)
                return
            }
            guard let http = response as? HTTPURLResponse, let data = data,
                  (200..<300).contains(http.statusCode) else {
                captured = .failure(URLError(.badServerResponse))
                return
            }
            captured = .success(Fetched(data: data, response: http))
        }.resume()
        // Localhost over loopback; 30s is generous for any real fail mode.
        _ = semaphore.wait(timeout: .now() + .seconds(30))
        return try captured.get()
    }

    /// Fetch `/{city}/api/{endpoint}` — the exact URL `City.apiURL` builds
    /// for the production stores — and decode it with the same bare
    /// `JSONDecoder()` they use. Anything the stores would fail on, this
    /// fails on.
    func decodeCityAPI<T: Decodable>(_ type: T.Type, city: String, endpoint: String) throws -> (value: T, fetched: Fetched) {
        let fetched = try fetch(City.apiURL(base: baseURL, slug: city, endpoint: endpoint))
        return (try JSONDecoder().decode(type, from: fetched.data), fetched)
    }
}
