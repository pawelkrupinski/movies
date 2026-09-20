import XCTest
import Combine
@testable import KinowoAuth

@MainActor
final class StateSyncServiceTests: XCTestCase {

    private var defaults: UserDefaults!
    private var prefs: UserPreferences!
    private var client: FakeUserStateClient!
    private var userSubject: CurrentValueSubject<UserProfile?, Never>!

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: "StateSyncServiceTests")!
        defaults.removePersistentDomain(forName: "StateSyncServiceTests")
        prefs = UserPreferences(store: defaults)
        client = FakeUserStateClient()
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

    /// Poll until `cond` holds — for the server-authoritative path, which
    /// mirrors state without a push, so there's no `onPut` to await.
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
            displayName: "Test",
            email: "test@test.com",
            avatarUrl: nil,
            provider: "google"
        ))
    }

    // MARK: - Merge on login

    func testLoginSyncsRemoteHiddenIntoEmptyLocal() async throws {
        client.remoteState = UserSyncState(hiddenFilms: ["Film A", "Film B"], disabledCinemas: [])
        let pushed = expectation(description: "state pushed to server")
        client.onPut = { _ in pushed.fulfill() }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)

        XCTAssertEqual(prefs.hiddenFilms, ["Film A", "Film B"])
        _ = sync
    }

    func testLoginMergesLocalAndRemoteHidden() async throws {
        prefs.hide("Local Only")
        client.remoteState = UserSyncState(hiddenFilms: ["Remote Only"], disabledCinemas: [])
        let pushed = expectation(description: "state pushed to server")
        client.onPut = { _ in pushed.fulfill() }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)

        XCTAssertEqual(prefs.hiddenFilms, ["Local Only", "Remote Only"])
        XCTAssertEqual(client.lastPushed?.hiddenFilms, ["Local Only", "Remote Only"])
        _ = sync
    }

    func testLoginSyncsDisabledCinemas() async throws {
        client.remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: ["Cinema X"])
        let pushed = expectation(description: "state pushed to server")
        client.onPut = { _ in pushed.fulfill() }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)

        XCTAssertEqual(prefs.disabledCinemas, ["Cinema X"])
        _ = sync
    }

    func testLoginPushesMergedStateToServer() async throws {
        prefs.hide("Already Hidden")
        prefs.setDisabledCinemas(["Local Cinema"])
        client.remoteState = UserSyncState(hiddenFilms: ["From Server"], disabledCinemas: ["Remote Cinema"])
        let pushed = expectation(description: "state pushed to server")
        client.onPut = { _ in pushed.fulfill() }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)

        XCTAssertEqual(client.lastPushed?.hiddenFilms, ["Already Hidden", "From Server"])
        XCTAssertEqual(client.lastPushed?.disabledCinemas, ["Local Cinema", "Remote Cinema"])
        _ = sync
    }

    // MARK: - Language sync — a scalar, so no migration-flag dance

    /// The account's pick is restored regardless of the `serverStateSynced`
    /// flag — this is the very first merge (flag unset), which the sets'
    /// union path shares, but language must not wait for a second login.
    func testLoginRestoresAccountLanguage() async throws {
        client.remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: [], language: "de")
        let sync = makeSyncService()
        login()

        try await waitUntil { self.prefs.selectedLanguage == "de" }
        _ = sync
    }

    /// The account has no pick yet, but this device does (set before login,
    /// e.g. while anonymous) — it gets adopted as the account's, same
    /// "migrate this device's local state up" spirit as the sets.
    func testLoginPushesLocalExplicitLanguageWhenAccountHasNone() async throws {
        prefs.setLanguage("es")
        client.remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: [], language: nil)
        let pushed = expectation(description: "language pushed to server")
        client.onPut = { state in if state.language == "es" { pushed.fulfill() } }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)
        _ = sync
    }

    /// Neither side has an explicit pick — nothing to restore or stamp onto
    /// the account. `selectedLanguage` stays on whatever `resolve()` fell
    /// back to at init (device/storefront/English), untouched by the merge —
    /// the FIRST login's own hiddenFilms/disabledCinemas migration still
    /// pushes once regardless (pre-existing, unrelated to language), so this
    /// only checks that push's `language` came along empty, not that no push
    /// happened at all.
    func testLoginLeavesResolvedDefaultAloneWhenNeitherSideHasAPick() async throws {
        let resolvedAtInit = prefs.selectedLanguage
        client.remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: [], language: nil)
        let sync = makeSyncService()

        login()
        try await Task.sleep(for: .milliseconds(200))

        XCTAssertEqual(prefs.selectedLanguage, resolvedAtInit)
        XCTAssertNil(client.lastPushed?.language)
        _ = sync
    }

    /// A pick made AFTER login (not just the merge-on-login case above)
    /// reaches the server too, through the same debounced push the sets use.
    func testLanguagePickAfterLoginIsPushed() async throws {
        client.remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: [], language: nil)
        let sync = makeSyncService()
        login()
        // Neither side has a pick, so this merge pushes nothing — no
        // expectation to await. `startObservingPrefs()` runs right after the
        // merge settles, so a short sleep (same idiom the "nothing happens"
        // tests elsewhere in this file use) is enough to let it land before
        // the pick below, which the observer must be live for.
        try await Task.sleep(for: .milliseconds(200))

        let pushed = expectation(description: "explicit pick pushed")
        client.onPut = { state in if state.language == "de" { pushed.fulfill() } }
        prefs.setLanguage("de")
        await fulfillment(of: [pushed], timeout: 1)
        _ = sync
    }

    // MARK: - Server authoritative after first sync

    /// Regression: once the one-time migration has run, a later launch must
    /// MIRROR the server, not blindly union. A film removed on another device
    /// (server now empty) must not be resurrected from this device's stale
    /// local copy. The previous union-on-every-login made removals impossible.
    func testServerAuthoritativeAfterFirstSyncDropsStaleLocal() async throws {
        // Launch 1: migrate from server = ["Film A"], flag flips on.
        client.remoteState = UserSyncState(hiddenFilms: ["Film A"], disabledCinemas: [])
        let pushed = expectation(description: "first push")
        client.onPut = { _ in pushed.fulfill() }
        let sync1 = makeSyncService()
        login()
        await fulfillment(of: [pushed], timeout: 1)
        XCTAssertEqual(prefs.hiddenFilms, ["Film A"])
        XCTAssertTrue(prefs.serverStateSynced)

        // Another device removes "Film A" from the account.
        client.remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: [])
        client.onPut = nil

        // Launch 2: same persisted prefs (flag still set), a fresh session
        // restore (publisher starts nil, then the user) — must NOT clear the
        // flag on the initial nil, and must mirror the now-empty server.
        let userSubject2 = CurrentValueSubject<UserProfile?, Never>(nil)
        let sync2 = StateSyncService(
            prefs: prefs, userPublisher: userSubject2.eraseToAnyPublisher(), client: client)
        userSubject2.send(UserProfile(
            displayName: "Test", email: "test@test.com", avatarUrl: nil, provider: "google"))

        try await waitUntil { self.prefs.hiddenFilms.isEmpty }
        XCTAssertEqual(prefs.hiddenFilms, [])
        _ = (sync1, sync2)
    }

    /// A genuine logout re-arms migration so the next sign-in carries this
    /// device's current local picks up again.
    func testLogoutReArmsMigration() async throws {
        client.remoteState = UserSyncState(hiddenFilms: ["Film A"], disabledCinemas: [])
        let pushed = expectation(description: "first push")
        client.onPut = { _ in pushed.fulfill() }
        let sync = makeSyncService()
        login()
        await fulfillment(of: [pushed], timeout: 1)
        XCTAssertTrue(prefs.serverStateSynced)

        userSubject.send(nil)  // logout
        try await waitUntil { self.prefs.serverStateSynced == false }
        XCTAssertFalse(prefs.serverStateSynced)
        _ = sync
    }

    func testNoSyncWhenNotLoggedIn() async throws {
        client.remoteState = UserSyncState(hiddenFilms: ["Film A"], disabledCinemas: [])
        let sync = makeSyncService()

        try await Task.sleep(for: .milliseconds(200))

        XCTAssertTrue(prefs.hiddenFilms.isEmpty)
        XCTAssertNil(client.lastPushed)
        _ = sync
    }

    func testFetchFailurePreservesLocalState() async throws {
        prefs.hide("My Film")
        client.shouldFailFetch = true
        let sync = makeSyncService()

        login()
        try await Task.sleep(for: .milliseconds(200))

        XCTAssertEqual(prefs.hiddenFilms, ["My Film"])
        XCTAssertNil(client.lastPushed)
        _ = sync
    }
}

// MARK: - Fake

@MainActor
final class FakeUserStateClient: UserStateClient {
    var remoteState = UserSyncState(hiddenFilms: [], disabledCinemas: [])
    var lastPushed: UserSyncState?
    var onPut: ((UserSyncState) -> Void)?
    var shouldFailFetch = false

    func fetchState() async throws -> UserSyncState {
        if shouldFailFetch { throw URLError(.notConnectedToInternet) }
        return remoteState
    }

    func putState(_ state: UserSyncState) async throws {
        lastPushed = state
        onPut?(state)
    }
}
