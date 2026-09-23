import XCTest
@testable import KinowoCore
@testable import KinowoNetworking

/// `DetailsStore.reload()` racing a city switch — the same late-landing
/// hazard `RepertoireStoreReloadPruningTests` covers for the repertoire.
@MainActor
final class DetailsStoreCitySwitchTests: XCTestCase {

    private let deployment = URL(string: "https://details-switch-test.invalid")!
    private let city = "detailscity"
    private let otherCity = "otherdetailscity"

    override func tearDown() {
        DetailsCache.save([], deployment: deployment, city: city, lastModified: nil)
        DetailsCache.save([], deployment: deployment, city: otherCity, lastModified: nil)
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    func testSlowResponseForThePreviousCityIsDroppedAfterASwitch() async throws {
        let oldCity = [FilmDetails(title: "Old City Film", originalTitle: nil, synopsis: "old", trailerURLs: [])]
        let newCity = [FilmDetails(title: "New City Film", originalTitle: nil, synopsis: "new", trailerURLs: [])]
        let slowCity = city
        URLProtocolStub.handler = { request in
            let isOld = request.url!.path.contains("/\(slowCity)/")
            var response = URLProtocolStub.Response(
                statusCode: 200, headers: [:], body: try! JSONEncoder().encode(isOld ? oldCity : newCity))
            if isOld { response.delay = 0.4 }
            return response
        }
        let config = URLSessionConfiguration.ephemeral
        config.protocolClasses = [URLProtocolStub.self]

        let store = DetailsStore(base: deployment, citySlug: city, session: URLSession(configuration: config))
        let slowReload = Task { await store.reload() }
        try await Task.sleep(for: .milliseconds(100))
        store.use(citySlug: otherCity)
        await slowReload.value
        try await Task.sleep(for: .milliseconds(300))

        XCTAssertNil(store.details(for: "Old City Film"))
        XCTAssertNotNil(store.details(for: "New City Film"))
        XCTAssertEqual(DetailsCache.load(deployment: deployment, city: otherCity)?.map(\.title), ["New City Film"])
    }
}
