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
