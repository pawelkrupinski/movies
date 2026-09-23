import XCTest
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

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: "CountrySwitchTests")!
        defaults.removePersistentDomain(forName: "CountrySwitchTests")
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: "CountrySwitchTests")
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    func testCitySelectedAfterACountrySwitchIsFetchedFromTheNewDeployment() async throws {
        let poland = Country.all.first { $0.code == "pl" }!
        let unitedKingdom = Country.all.first { $0.code == "uk" }!
        let requestedHosts = LockedHosts()
        URLProtocolStub.handler = { request in
            requestedHosts.append(request.url!.host!)
            return .init(statusCode: 404, headers: [:], body: Data())
        }
        let config = URLSessionConfiguration.ephemeral
        config.protocolClasses = [URLProtocolStub.self]
        let session = URLSession(configuration: config)
        let prefs = UserPreferences(store: defaults)
        let store = RepertoireStore(base: poland.baseURL, citySlug: "poznan", session: session)
        let details = DetailsStore(base: poland.baseURL, citySlug: "poznan", session: session)

        switchCountry(to: unitedKingdom, prefs: prefs, store: store, details: details)
        store.use(citySlug: "london")
        details.use(citySlug: "london")
        try await Task.sleep(for: .milliseconds(300))

        XCTAssertEqual(prefs.selectedCountry, unitedKingdom)
        XCTAssertFalse(requestedHosts.all.isEmpty)
        XCTAssertEqual(Set(requestedHosts.all), [unitedKingdom.baseURL.host!])
    }
}

private final class LockedHosts: @unchecked Sendable {
    private let lock = NSLock()
    private var hosts: [String] = []
    func append(_ host: String) { lock.lock(); hosts.append(host); lock.unlock() }
    var all: [String] { lock.lock(); defer { lock.unlock() }; return hosts }
}
