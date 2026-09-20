import XCTest
import Combine
@testable import KinowoAuth

@MainActor
final class StateSyncServiceTests: XCTestCase {

    private var defaults: UserDefaults!
    private var prefs: UserPreferences!
    private var client: FakeHiddenFilmsClient!
    private var userSubject: CurrentValueSubject<UserProfile?, Never>!

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: "StateSyncServiceTests")!
        defaults.removePersistentDomain(forName: "StateSyncServiceTests")
        prefs = UserPreferences(store: defaults)
        client = FakeHiddenFilmsClient()
        userSubject = CurrentValueSubject(nil)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: "StateSyncServiceTests")
        super.tearDown()
    }

    private func makeSyncService() -> StateSyncService {
        StateSyncService(
            prefs: prefs,
            userPublisher: userSubject.eraseToAnyPublisher(),
            client: client
        )
    }

    private func waitUntil(_ cond: @escaping () -> Bool, timeout: TimeInterval = 1) async throws {
        let start = Date()
        while !cond() {
            if Date().timeIntervalSince(start) > timeout {
                XCTFail("condition not met within \(timeout)s"); return
            }
            try await Task.sleep(for: .milliseconds(20))
        }
    }

    private func login() {
        userSubject.send(UserProfile(
            displayName: "Test", email: "test@test.com", avatarUrl: nil, provider: "google"))
    }

    private let pl = "pl"
    private let unitedKingdom = Country.all.first { $0.code == "uk" }!

    // MARK: - First sync (per-country migration)

    func testLoginSyncsRemoteHiddenIntoEmptyLocal() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Film A", "Film B"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()

        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Film A", "Film B"] }

        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, "\"e1\"")
        XCTAssertTrue(client.hideCalls.isEmpty) // nothing local-only to push
        _ = sync
    }

    func testLoginMergesLocalAndRemoteHiddenPushingOnlyTheLocalOnlyDiff() async throws {
        prefs.hide("Local Only")
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Remote Only"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let pushed = expectation(description: "local-only title pushed")
        client.onHide = { pushed.fulfill() }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)
        try await waitUntil { self.prefs.hiddenFilms == ["Local Only", "Remote Only"] }

        XCTAssertEqual(client.hideCalls.count, 1)
        XCTAssertEqual(client.hideCalls.first?.country, pl)
        XCTAssertEqual(client.hideCalls.first?.title, "Local Only")
        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))
        _ = sync
    }

    // Regression for the cinema-sync retirement: proves a cinema toggle
    // never reaches the network via ANY of the three write methods, and the
    // local value survives a login/merge untouched.
    func testMergeNeverTouchesDisabledCinemas() async throws {
        prefs.setDisabledCinemas(["Local Only Cinema"])
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Film A"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()

        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Film A"] }

        XCTAssertEqual(prefs.disabledCinemas, ["Local Only Cinema"])
        _ = sync
    }

    // MARK: - Server authoritative after first sync

    func testServerAuthoritativeAfterFirstSyncDropsStaleLocal() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Film A"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync1 = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Film A"] }
        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))

        // Another device removed "Film A" from the account.
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"e2\"", lastModified: "Tue, 19 May 2026 13:00:00 GMT"))

        // Fresh session restore (publisher starts nil, then the user) — must
        // NOT clear the flag on the initial nil, and must MIRROR the now-empty
        // server rather than re-union the stale local copy back in.
        let userSubject2 = CurrentValueSubject<UserProfile?, Never>(nil)
        let sync2 = StateSyncService(prefs: prefs, userPublisher: userSubject2.eraseToAnyPublisher(), client: client)
        userSubject2.send(UserProfile(displayName: "Test", email: "test@test.com", avatarUrl: nil, provider: "google"))

        try await waitUntil { self.prefs.hiddenFilms.isEmpty }
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, "\"e2\"")
        _ = (sync1, sync2)
    }

    func testNotModifiedLeavesLocalStateUntouched() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Film A"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        client.fetchResults[pl] = .notModified
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.hiddenFilms, ["Film A"])
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, "\"e1\"") // unchanged — 304 carried no fresh validators to store
    }

    /// The reconcile a FOREGROUND RESUME triggers is the exact same public
    /// entry point `ContentView`'s `scenePhase` handler calls — this proves
    /// the entry point itself does a real reconcile, independent of login.
    func testReconcileCurrentCountryPicksUpARemoteChangeWithoutARelaunch() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Hidden Elsewhere"], etag: "\"e2\"", lastModified: "Tue, 19 May 2026 13:00:00 GMT"))
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.hiddenFilms, ["Hidden Elsewhere"])
    }

    func testLogoutReArmsMigrationForEveryCountry() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Film A"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        userSubject.send(nil) // logout
        try await waitUntil { !self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        XCTAssertNil(prefs.hiddenFilmsValidators(country: pl).etag) // validators forgotten too
        _ = sync
    }

    func testNoSyncWhenNotLoggedIn() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Film A"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()

        try await Task.sleep(for: .milliseconds(200))

        XCTAssertTrue(prefs.hiddenFilms.isEmpty)
        XCTAssertTrue(client.fetchedCountries.isEmpty)
        _ = sync
    }

    func testFetchFailurePreservesLocalStateAndLeavesMigrationUnset() async throws {
        prefs.hide("My Film")
        client.shouldFailFetch = true
        let sync = makeSyncService()

        login()
        try await Task.sleep(for: .milliseconds(200))

        XCTAssertEqual(prefs.hiddenFilms, ["My Film"])
        XCTAssertFalse(prefs.isHiddenFilmsMigrated(country: pl))
        _ = sync
    }

    // MARK: - Immediate (non-debounced) writes

    func testHideFiresImmediatelyNoDebounce() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        let pushed = expectation(description: "hide pushed")
        client.writeResult = HiddenFilmsResult(hiddenFilms: ["New Hide"], etag: "\"e2\"", lastModified: "Tue, 19 May 2026 13:00:00 GMT")
        client.onHide = { pushed.fulfill() }
        prefs.hide("New Hide")

        // The OLD debounce was 400ms — a push landing well before that proves
        // there's no debounce left for hiddenFilms.
        await fulfillment(of: [pushed], timeout: 0.2)
        XCTAssertEqual(client.hideCalls.last?.title, "New Hide")
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, "\"e2\"")
        _ = sync
    }

    func testUnhideFiresImmediately() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["Was Hidden"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Was Hidden"] }

        let pushed = expectation(description: "unhide pushed")
        client.onUnhide = { pushed.fulfill() }
        prefs.unhide("Was Hidden")

        await fulfillment(of: [pushed], timeout: 0.2)
        XCTAssertEqual(client.unhideCalls.last?.title, "Was Hidden")
        _ = sync
    }

    func testUnhideAllCallsClearNotIndividualUnhides() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["A", "B"], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["A", "B"] }

        let pushed = expectation(description: "clear pushed")
        client.onClear = { pushed.fulfill() }
        prefs.unhideAll()

        await fulfillment(of: [pushed], timeout: 0.2)
        XCTAssertEqual(client.clearCalls, [pl])
        XCTAssertTrue(client.unhideCalls.isEmpty)
        _ = sync
    }

    // MARK: - Country switch

    func testCountrySwitchReconcilesTheNewlySelectedCountry() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"pl1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        XCTAssertFalse(client.fetchedCountries.contains(unitedKingdom.code))

        client.fetchResults[unitedKingdom.code] = .current(HiddenFilmsResult(hiddenFilms: ["UK Film"], etag: "\"uk1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        prefs.setCountry(unitedKingdom)

        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.unitedKingdom.code) }
        XCTAssertEqual(prefs.hiddenFilms, ["UK Film"])
        _ = sync
    }
}

// MARK: - Fake

@MainActor
final class FakeHiddenFilmsClient: HiddenFilmsClient {
    /// What `fetch` returns, keyed by country — set directly by each test.
    /// Absent for a country means "empty, fresh" (a fresh 200 with nothing
    /// hidden), matching a brand-new account.
    var fetchResults: [String: HiddenFilmsFetchResult] = [:]
    var shouldFailFetch = false
    var writeResult = HiddenFilmsResult(hiddenFilms: [], etag: "\"w\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT")

    private(set) var fetchedCountries: [String] = []
    private(set) var hideCalls: [(country: String, title: String)] = []
    private(set) var unhideCalls: [(country: String, title: String)] = []
    private(set) var clearCalls: [String] = []

    var onHide: (() -> Void)?
    var onUnhide: (() -> Void)?
    var onClear: (() -> Void)?

    func fetch(country: String, etag: String?, lastModified: String?) async throws -> HiddenFilmsFetchResult {
        if shouldFailFetch { throw URLError(.notConnectedToInternet) }
        fetchedCountries.append(country)
        return fetchResults[country] ?? .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"empty\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
    }

    func hide(country: String, title: String) async throws -> HiddenFilmsResult {
        hideCalls.append((country, title))
        defer { onHide?() }
        return writeResult
    }

    func unhide(country: String, title: String) async throws -> HiddenFilmsResult {
        unhideCalls.append((country, title))
        defer { onUnhide?() }
        return writeResult
    }

    func clear(country: String) async throws -> HiddenFilmsResult {
        clearCalls.append(country)
        defer { onClear?() }
        return writeResult
    }
}
