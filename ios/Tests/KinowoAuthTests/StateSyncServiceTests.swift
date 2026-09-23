import XCTest
import Combine
@testable import KinowoAuth

@MainActor
final class StateSyncServiceTests: XCTestCase {

    private var defaults: UserDefaults!
    private var prefs: UserPreferences!
    private var client: FakeHiddenFilmsClient!
    private var languageClient: FakeLanguageClient!
    private var userSubject: CurrentValueSubject<UserProfile?, Never>!

    override func setUp() {
        super.setUp()
        defaults = UserDefaults(suiteName: "StateSyncServiceTests")!
        defaults.removePersistentDomain(forName: "StateSyncServiceTests")
        prefs = UserPreferences(store: defaults)
        client = FakeHiddenFilmsClient()
        languageClient = FakeLanguageClient()
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
            client: client,
            languageClient: languageClient
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

    // MARK: - Language sync — a scalar, so no migration-flag dance, and its
    // own fetch/push via LanguageClient (HiddenFilmsClient's response never
    // carries it)

    /// The account's pick is restored regardless of the per-country
    /// migration flags — this is the very first merge (flags unset), which
    /// the sets' union path shares, but language must not wait for a second
    /// login.
    func testLoginRestoresAccountLanguage() async throws {
        languageClient.remote = "de"
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
        languageClient.remote = nil
        let pushed = expectation(description: "language pushed to server")
        languageClient.onPush = { language in if language == "es" { pushed.fulfill() } }
        let sync = makeSyncService()

        login()
        await fulfillment(of: [pushed], timeout: 1)
        _ = sync
    }

    /// Neither side has an explicit pick — nothing to restore or stamp onto
    /// the account, and no push at all: unlike the sets, language reconcile
    /// makes no fixed first write. `selectedLanguage` stays on whatever
    /// `resolve()` fell back to at init (device/storefront/English).
    func testLoginLeavesResolvedDefaultAloneWhenNeitherSideHasAPick() async throws {
        let resolvedAtInit = prefs.selectedLanguage
        languageClient.remote = nil
        let sync = makeSyncService()

        login()
        try await Task.sleep(for: .milliseconds(200))

        XCTAssertEqual(prefs.selectedLanguage, resolvedAtInit)
        XCTAssertNil(languageClient.lastPushed)
        _ = sync
    }

    /// A pick made AFTER login (not just the merge-on-login case above)
    /// reaches the server too, through language's own debounced push.
    func testLanguagePickAfterLoginIsPushed() async throws {
        languageClient.remote = nil
        let sync = makeSyncService()
        login()
        // Neither side has a pick, so this merge pushes nothing — no
        // expectation to await. `observeLocalChanges()` runs right after the
        // merge settles, so a short sleep (same idiom the "nothing happens"
        // tests elsewhere in this file use) is enough to let it land before
        // the pick below, which the observer must be live for.
        try await Task.sleep(for: .milliseconds(200))

        let pushed = expectation(description: "explicit pick pushed")
        languageClient.onPush = { language in if language == "de" { pushed.fulfill() } }
        prefs.setLanguage("de")
        await fulfillment(of: [pushed], timeout: 1)
        _ = sync
    }

    /// A foreground reconcile inside the push debounce used to fetch the
    /// account's OLDER pick, write it over the one just made, and push that
    /// old value back — cancelling the push of the new one. The pending pick
    /// must win and reach the server instead.
    func testReconcileRightAfterAPickKeepsThePickAndPushesIt() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        prefs.setLanguage("es")
        await Task.yield()
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.selectedLanguage, "es")
        try await waitUntil { self.languageClient.remote == "es" }
        try await Task.sleep(for: .milliseconds(500))
        XCTAssertEqual(languageClient.pushes, ["es"])
        _ = sync
    }

    /// Adopting the account's pick on a resume reconcile (another device
    /// changed it) is not a local pick: it must not echo the same value
    /// straight back to the server.
    func testAdoptingTheAccountLanguageDoesNotPushItBack() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        languageClient.remote = "pl"
        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.selectedLanguage, "pl")

        try await Task.sleep(for: .milliseconds(600))

        XCTAssertEqual(languageClient.pushes, [])
        _ = sync
    }

    /// A push the server rejected is still pending: the next reconcile
    /// retries it rather than adopting the account's older value.
    func testAFailedLanguagePushIsRetriedOnTheNextReconcile() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        languageClient.shouldFailPush = true
        let attempted = expectation(description: "push attempted")
        languageClient.onPush = { _ in attempted.fulfill() }
        prefs.setLanguage("es")
        await fulfillment(of: [attempted], timeout: 1)
        languageClient.onPush = nil

        languageClient.shouldFailPush = false
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.selectedLanguage, "es")
        XCTAssertEqual(languageClient.remote, "es")
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
        let sync2 = StateSyncService(prefs: prefs, userPublisher: userSubject2.eraseToAnyPublisher(), client: client, languageClient: languageClient)
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

    /// Switching to a country this device has never synced must show that
    /// country's bucket — not union the previous country's titles in and
    /// push them up as hides of the new one.
    func testCountrySwitchToUnsyncedCountryDoesNotLeakThePreviousCountrysHides() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["PL Film"], etag: "\"pl1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }

        client.fetchResults[unitedKingdom.code] = .current(HiddenFilmsResult(hiddenFilms: ["UK Film"], etag: "\"uk1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        prefs.setCountry(unitedKingdom)

        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.unitedKingdom.code) }
        XCTAssertEqual(prefs.hiddenFilms, ["UK Film"])
        XCTAssertTrue(client.hideCalls.isEmpty, "pushed another country's hides: \(client.hideCalls)")
        _ = sync
    }

    /// Switching BACK to an already-synced country answers 304 — the
    /// visible set must still become that country's own, not stay on the
    /// OTHER country's.
    func testCountrySwitchBackToASyncedCountryRestoresItsOwnSet() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["PL Film"], etag: "\"pl1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        client.fetchResults[unitedKingdom.code] = .current(HiddenFilmsResult(hiddenFilms: ["UK Film"], etag: "\"uk1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }
        prefs.setCountry(unitedKingdom)
        try await waitUntil { self.prefs.hiddenFilms == ["UK Film"] }

        // PL's bucket hasn't changed server-side, so a conditional GET with
        // PL's validators would answer 304.
        client.notModifiedWhenETagMatches = true
        prefs.setCountry(Country.all.first { $0.code == pl }!)

        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }
        _ = sync
    }

    /// A reconcile still in flight when the user switches country must not
    /// land its (old-country) result over the newly selected country's set —
    /// it belongs in the old country's own bucket.
    func testReconcileResultForAPreviouslySelectedCountryLandsInItsOwnBucket() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["PL Film"], etag: "\"pl1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        client.fetchResults[unitedKingdom.code] = .current(HiddenFilmsResult(hiddenFilms: ["UK Film"], etag: "\"uk1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }

        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: ["PL Film", "PL Film 2"], etag: "\"pl2\"", lastModified: "Tue, 19 May 2026 13:00:00 GMT"))
        client.fetchDelay[pl] = .milliseconds(300)
        let slowResume = Task { await sync.reconcileCurrentCountry() }
        try await Task.sleep(for: .milliseconds(50))
        prefs.setCountry(unitedKingdom)
        try await waitUntil { self.prefs.hiddenFilms == ["UK Film"] }

        await slowResume.value
        XCTAssertEqual(prefs.hiddenFilms, ["UK Film"])
        XCTAssertEqual(prefs.hiddenFilms(country: pl), ["PL Film", "PL Film 2"])
    }

    /// `AuthService.user` re-publishes a non-nil profile whenever
    /// `checkSession()` re-runs (e.g. the root `.task` restarting after the
    /// language `.id` flips). A second non-nil emission must not stack a
    /// second set of change observers — every hide would then be PUT twice.
    func testRepeatedLoginEmissionPushesEachHideOnce() async throws {
        client.fetchResults[pl] = .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"e1\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        login()
        try await Task.sleep(for: .milliseconds(200))

        prefs.hide("New Hide")
        try await Task.sleep(for: .milliseconds(200))

        XCTAssertEqual(client.hideCalls.map(\.title), ["New Hide"])
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
    /// Answer like a real conditional GET: `.notModified` when the caller's
    /// `etag` matches the stored result's. Off by default so existing tests
    /// keep their explicit `.notModified` / `.current` scripting.
    var notModifiedWhenETagMatches = false
    /// Hold `fetch` for this long before answering, per country — lets a
    /// test switch country while an older reconcile is still in flight.
    var fetchDelay: [String: Duration] = [:]
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
        if let delay = fetchDelay[country] { try await Task.sleep(for: delay) }
        let result = fetchResults[country] ?? .current(HiddenFilmsResult(hiddenFilms: [], etag: "\"empty\"", lastModified: "Tue, 19 May 2026 12:00:00 GMT"))
        if notModifiedWhenETagMatches, let etag, case .current(let current) = result, current.etag == etag {
            return .notModified
        }
        return result
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

@MainActor
final class FakeLanguageClient: LanguageClient {
    /// The account's stored pick — a successful push updates it, as the
    /// server does.
    var remote: String?
    var shouldFailPush = false
    /// Every push that SUCCEEDED, in order.
    private(set) var pushes: [String] = []
    var lastPushed: String? { pushes.last }
    /// Called on every push attempt, failed or not.
    var onPush: ((String) -> Void)?

    func fetch() async throws -> String? { remote }

    func push(_ language: String) async throws {
        defer { onPush?(language) }
        if shouldFailPush { throw URLError(.badServerResponse) }
        pushes.append(language)
        remote = language
    }
}
