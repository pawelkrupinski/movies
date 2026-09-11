import XCTest
@testable import KinowoCore
@testable import KinowoNetworking

/// Regression coverage for `RepertoireStore.reload()`: every path that
/// updates `films` must re-prune against the CALLER's own clock, not trust
/// whatever the network handed back as already-current. A foreground
/// transition runs `pruneStaleShowings()` first (see `ContentView`'s
/// `scenePhase` handler); if `reload()` then clobbers `films` with a payload
/// that wasn't re-pruned, a screening more than 30 minutes in the past can
/// ride straight back onto the grid.
@MainActor
final class RepertoireStoreReloadPruningTests: XCTestCase {

    private let deployment = URL(string: "https://reload-pruning-test.invalid")!
    private let city = "testcity"

    override func tearDown() {
        // Reset the bound disk cache so cases don't leak into one another.
        RepertoireCache.save([], deployment: deployment, city: city, lastModified: nil)
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    private func stubbedSession() -> URLSession {
        let config = URLSessionConfiguration.ephemeral
        config.protocolClasses = [URLProtocolStub.self]
        return URLSession(configuration: config)
    }

    // MARK: - A freshly-decoded (200) response

    /// Even though the server is expected to have already dropped past
    /// screenings at request time, a slow round trip (or a response generated
    /// a little earlier than it's applied) can still land one more than 30
    /// minutes stale. `reload()` must not trust that — this is the exact
    /// gap that let a foreground `pruneStaleShowings()` get clobbered by the
    /// `reloadIfStale()` it's immediately followed by.
    func testReloadPrunesAScreeningTheServerSentBackAsPast() async throws {
        let now = Date()
        let (date, time) = warsawDateAndTime(now.addingTimeInterval(-45 * 60))
        let stalePayload = [film(date: date, time: time)]

        URLProtocolStub.handler = { request in
            request.url!.path.hasSuffix("/cinemas")
                ? .init(statusCode: 404, headers: [:], body: Data())
                : jsonResponse(stalePayload)
        }

        let store = RepertoireStore(base: deployment, citySlug: city, session: stubbedSession())
        await store.reload(now: now)

        XCTAssertTrue(store.films.isEmpty,
                       "a screening 45 minutes in the past should have been pruned from a freshly-fetched payload, got \(store.films)")
    }

    /// A screening still inside the 30-minute grace survives the same path —
    /// the fix must prune, not blank the listing outright.
    func testReloadKeepsAScreeningStillInsideTheGraceWindow() async throws {
        let now = Date()
        let (date, time) = warsawDateAndTime(now.addingTimeInterval(-10 * 60))
        let payload = [film(date: date, time: time)]

        URLProtocolStub.handler = { request in
            request.url!.path.hasSuffix("/cinemas")
                ? .init(statusCode: 404, headers: [:], body: Data())
                : jsonResponse(payload)
        }

        let store = RepertoireStore(base: deployment, citySlug: city, session: stubbedSession())
        await store.reload(now: now)

        XCTAssertEqual(store.films.count, 1,
                        "a screening only 10 minutes in the past is still inside the 30-minute grace and must survive")
    }

    // MARK: - A cached body replayed via 304

    /// A 304 response vouches only for the BODY being unchanged since it was
    /// cached, not for it still being current — the cached body can carry a
    /// screening time that has since crossed the 30-minute cutoff.
    /// `RepertoireCache.bodyForNotModified` only hands back the cached entry
    /// when the caller is holding nothing, so a fresh store (never
    /// `loadCachedData()`-primed) is what exercises that path.
    func testReloadPrunesAStaleScreeningReplayedByA304() async throws {
        let now = Date()
        let (date, time) = warsawDateAndTime(now.addingTimeInterval(-45 * 60))
        let staleCachedPayload = [film(date: date, time: time)]
        RepertoireCache.save(staleCachedPayload, deployment: deployment, city: city, lastModified: "cached-tag")

        URLProtocolStub.handler = { request in
            request.url!.path.hasSuffix("/cinemas")
                ? .init(statusCode: 404, headers: [:], body: Data())
                : .init(statusCode: 304, headers: [:], body: Data())
        }

        let store = RepertoireStore(base: deployment, citySlug: city, session: stubbedSession())
        XCTAssertTrue(store.films.isEmpty, "precondition: a fresh store starts with no in-memory films")
        await store.reload(now: now)

        XCTAssertTrue(store.films.isEmpty,
                       "a 304-replayed cache entry with a screening 45 minutes in the past should have been pruned, got \(store.films)")
    }
}

// MARK: - Fixtures

/// A film with one showtime at `time` (`HH:mm`, `.warsaw`) on `date`
/// (`yyyy-MM-dd`, `.warsaw`) — everything else is filler.
private func film(date: String, time: String) -> Film {
    Film(
        title: "Test Film",
        posterURL: nil,
        fallbackPosterURLs: [],
        runtimeMinutes: 100,
        releaseYear: 2026,
        genres: [],
        ageRating: nil,
        ratings: .empty,
        countries: [],
        directors: [],
        cast: [],
        showings: [
            DayShowings(date: date, label: "Dziś", cinemas: [
                CinemaShowings(cinema: "Kino Test", cinemaURL: nil, showtimes: [
                    Showtime(time: time, format: "2D", room: nil, bookingURL: nil)
                ])
            ])
        ]
    )
}

/// `moment`'s `yyyy-MM-dd`/`HH:mm` in `.warsaw`, the pair `prunedPastShowings`
/// parses back via `ShowtimeClock.wallClockDate`.
private func warsawDateAndTime(_ moment: Date) -> (date: String, time: String) {
    var calendar = Calendar(identifier: .gregorian)
    calendar.timeZone = .warsaw
    let c = calendar.dateComponents([.year, .month, .day, .hour, .minute], from: moment)
    let date = String(format: "%04d-%02d-%02d", c.year!, c.month!, c.day!)
    let time = String(format: "%02d:%02d", c.hour!, c.minute!)
    return (date, time)
}

private func jsonResponse(_ films: [Film]) -> URLProtocolStub.Response {
    .init(statusCode: 200, headers: [:], body: try! JSONEncoder().encode(films))
}

// MARK: - URLProtocol stub

/// Intercepts every request on a session configured with it and answers from
/// `handler`, so `RepertoireStore.reload()` can be driven against canned
/// HTTP responses without touching the network.
final class URLProtocolStub: URLProtocol {
    struct Response {
        let statusCode: Int
        let headers: [String: String]
        let body: Data
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
        client?.urlProtocol(self, didReceive: httpResponse, cacheStoragePolicy: .notAllowed)
        client?.urlProtocol(self, didLoad: response.body)
        client?.urlProtocolDidFinishLoading(self)
    }

    override func stopLoading() {}
}
