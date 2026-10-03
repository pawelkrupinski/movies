import XCTest
import KinowoTestSupport
@testable import KinowoCore
@testable import KinowoNetworking

/// The `If-Modified-Since` stamp is read before every fetch, by an endpoint
/// living on the main actor. Read with a `queue.sync` there, it held the main
/// thread for as long as the cache's queue was busy — a save of the previous
/// response still being written — and read the whole listing to decode one line.
@MainActor
final class ConditionalListEndpointStampTests: XCTestCase {

    private let deployment = URL(string: "https://stamp-test.invalid")!
    private let city = "stampcity"
    private let cache = ConditionalPayloadCache<FilmDetails>(file: "conditional-list-endpoint-stamp-test.json")

    override func tearDown() {
        cache.remove()
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    func testAFetchDoesNotHoldTheMainActorWhileTheCacheQueueIsBusy() async throws {
        cache.save([], deployment: deployment, city: city, lastModified: "Sun, 25 May 2026 10:00:00 GMT")
        URLProtocolStub.handler = { _ in .init(statusCode: 200, headers: [:], body: Data("[]".utf8)) }
        let endpoint = ConditionalListEndpoint<FilmDetails>(
            base: deployment, citySlug: city, endpoint: "details", cache: cache, session: URLProtocolStub.session())
        let released = DispatchSemaphore(value: 0)
        cache.queue.async { _ = released.wait(timeout: .now() + 3) }

        let started = Date()
        let fetch = Task { try await endpoint.fetch(now: Date(), callerIsEmpty: { true }) }
        try await Task.sleep(nanoseconds: 50_000_000)
        let mainActorFreeAfter = Date().timeIntervalSince(started)
        released.signal()
        _ = try await fetch.value

        XCTAssertLessThan(mainActorFreeAfter, 1, "the stamp read held the main actor while the cache queue was busy")
        XCTAssertFalse(URLProtocolStub.requestedURLs.isEmpty, "the fetch never reached the network")
    }
}
