import XCTest
import KinowoTestSupport
@testable import KinowoCore
@testable import KinowoAuth
@testable import KinowoNetworking

/// `switchCountry` must re-point the stores, not just persist the pick: the
/// stores capture their deployment at init, so a country change that only
/// wrote the preference (the deep-link path used to) kept fetching the new
/// country's city from the old deployment.
@MainActor
final class CountrySwitchTests: XCTestCase {

    private var defaults: UserDefaults!
    private let directory = ConditionalPayloadCache<Film>.scratchDirectory()

    override func setUp() async throws {
        try await super.setUp()
        defaults = .scratch(suiteName: "CountrySwitchTests")
    }

    override func tearDown() async throws {
        defaults.discardScratch(suiteName: "CountrySwitchTests")
        ConditionalPayloadCache<Film>.discardScratchDirectory(directory)
        URLProtocolStub.handler = nil
        try await super.tearDown()
    }

    func testCitySelectedAfterACountrySwitchIsFetchedFromTheNewDeployment() async throws {
        let poland = Country.all.first { $0.code == "pl" }!
        let unitedKingdom = Country.all.first { $0.code == "uk" }!
        let requestedHosts = LockedValue<[String]>([])
        URLProtocolStub.handler = { request in
            requestedHosts.withLock { $0.append(request.url!.host!) }
            return .init(statusCode: 404, headers: [:], body: Data())
        }
        let session = URLProtocolStub.session()
        let prefs = UserPreferences(store: defaults)
        let store = RepertoireStore(base: poland.baseURL, citySlug: "poznan", session: session,
                                    cache: .repertoire(in: directory), posters: .throwaway())
        let details = DetailsStore(base: poland.baseURL, citySlug: "poznan", session: session, cache: .details(in: directory))

        switchCountry(to: unitedKingdom, prefs: prefs, store: store, details: details)
        store.use(citySlug: "london")
        details.use(citySlug: "london")
        // Each store asks the new city for its details and its cinemas.
        let fetched = await pollUntil { URLProtocolStub.requestedURLs.count >= 4 }
        XCTAssertTrue(fetched)

        XCTAssertEqual(prefs.selectedCountry, unitedKingdom)
        XCTAssertFalse(requestedHosts.withLock { $0 }.isEmpty)
        XCTAssertEqual(Set(requestedHosts.withLock { $0 }), [unitedKingdom.baseURL.host!])
    }
}
