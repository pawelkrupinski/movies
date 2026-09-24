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

    private func waitUntil(_ cond: @escaping () -> Bool, timeout: TimeInterval = 1,
                           file: StaticString = #filePath, line: UInt = #line) async throws {
        let start = Date()
        while !cond() {
            if Date().timeIntervalSince(start) > timeout {
                XCTFail("condition not met within \(timeout)s", file: file, line: line); return
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
        client.remote[pl] = ["Film A", "Film B"]
        let sync = makeSyncService()

        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Film A", "Film B"] }

        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, client.etagOf(pl))
        XCTAssertTrue(client.hideCalls.isEmpty) // nothing local-only to push
        _ = sync
    }

    func testLoginMergesLocalAndRemoteHiddenPushingOnlyTheLocalOnlyDiff() async throws {
        prefs.hide("Local Only")
        client.remote[pl] = ["Remote Only"]
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
        client.remote[pl] = ["Film A"]
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

    /// A push the server refuses for good (a language this server does not
    /// know) can never land: kept pending, it was re-pushed on every reconcile
    /// and the account's own pick never came back. It is dropped, and the
    /// account's pick adopted, without pushing the refused one again.
    func testALanguagePushRefusedForGoodIsDroppedForTheAccountsPick() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        languageClient.refusePush = true
        let attempted = expectation(description: "push attempted")
        languageClient.onPush = { _ in attempted.fulfill() }
        prefs.setLanguage("es")
        await fulfillment(of: [attempted], timeout: 1)
        languageClient.onPush = nil

        try await waitUntil { self.prefs.selectedLanguage == "de" }
        let attempts = languageClient.pushesStarted
        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.selectedLanguage, "de")
        XCTAssertEqual(languageClient.pushesStarted, attempts, "the refused pick must not be pushed again")
        _ = sync
    }

    /// A pick whose push failed survives a relaunch: the next session's
    /// login reconcile pushes it instead of restoring the account's older
    /// value over it.
    func testAFailedLanguagePushSurvivesARelaunch() async throws {
        languageClient.remote = "de"
        let sync1 = makeSyncService()
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

        // Relaunch: fresh prefs over the same store, fresh service, session restored.
        prefs = UserPreferences(store: defaults)
        let userSubject2 = CurrentValueSubject<UserProfile?, Never>(nil)
        let sync2 = StateSyncService(prefs: prefs, userPublisher: userSubject2.eraseToAnyPublisher(), client: client, languageClient: languageClient)
        userSubject2.send(UserProfile(displayName: "Test", email: "test@test.com", avatarUrl: nil, provider: "google"))
        try await waitUntil { self.languageClient.remote == "es" }
        XCTAssertEqual(prefs.selectedLanguage, "es")
        _ = (sync1, sync2)
    }

    /// Picking back to the account's language while another pick's push is
    /// still in flight: the pick-back matches what the account held when it
    /// was made, so nothing was sent for it, and the in-flight push then
    /// left the server on the abandoned pick. The final pick must win.
    func testPickingBackWhileAPushIsInFlightEndsOnTheFinalPick() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        let gate = AsyncGate()
        languageClient.beforePush = { await gate.wait() }
        prefs.setLanguage("es")
        try await waitUntil { self.languageClient.pushesStarted == 1 }
        prefs.setLanguage("de")
        languageClient.beforePush = nil
        await gate.open()

        try await waitUntil { self.languageClient.pushes.last == "de" }
        XCTAssertEqual(languageClient.remote, "de")
        _ = sync
    }

    /// Picking back while another pick's push is in flight, when that push
    /// then fails AFTER the server applied it: the account now holds the
    /// abandoned pick, so the pick-back must stay pending and win the next
    /// reconcile rather than the account's value being adopted. Mirrors Android.
    func testAPickBackSurvivesAnInFlightPushThatFailsAfterLanding() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        let gate = AsyncGate()
        languageClient.beforePushResponse = { await gate.wait(); throw URLError(.networkConnectionLost) }
        prefs.setLanguage("es")
        try await waitUntil { self.languageClient.pushesStarted == 1 }
        prefs.setLanguage("de")
        await gate.open()
        try await waitUntil { self.languageClient.inFlight == 0 }
        languageClient.beforePushResponse = nil
        XCTAssertEqual(languageClient.remote, "es") // the lost push landed

        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.selectedLanguage, "de")
        XCTAssertEqual(languageClient.remote, "de")
    }

    /// A pick made while another pick's push is still on the wire waits for
    /// it: two PUTs in flight at once could land in either order, leaving the
    /// account on the older pick. One push at a time; the latest pick wins.
    func testLanguagePushesAreSentOneAtATimeAndTheLatestWins() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))

        let gate = AsyncGate()
        languageClient.beforePushResponse = { await gate.wait() }
        prefs.setLanguage("es")
        try await waitUntil { self.languageClient.pushesStarted == 1 }
        prefs.setLanguage("fr")
        try await Task.sleep(for: .milliseconds(600)) // past the debounce
        XCTAssertEqual(languageClient.pushesStarted, 1)
        await gate.open()

        try await waitUntil { self.languageClient.remote == "fr" && self.languageClient.inFlight == 0 }
        XCTAssertEqual(languageClient.maxPushesInFlight, 1)
        XCTAssertEqual(languageClient.pushes, ["es", "fr"])
        _ = sync
    }

    /// A pick made AND sent while a reconcile's fetch is on the wire: the
    /// fetch's answer predates it, so adopting that answer would put the
    /// account's older pick back over the one it just confirmed. Mirrors Android.
    func testAPickSentDuringAReconcileFetchSurvivesItsAnswer() async throws {
        languageClient.remote = "de"
        let debounce = ManualDebounceScheduler()
        let sync = StateSyncService(prefs: prefs, userPublisher: userSubject.eraseToAnyPublisher(),
                                    client: client, languageClient: languageClient, debounceScheduler: debounce)
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" && self.languageClient.inFlight == 0 }

        let gate = AsyncGate()
        languageClient.beforeFetchResponse = { await gate.wait() }
        let reconcile = Task { await sync.reconcileCurrentCountry() }
        try await waitUntil { self.languageClient.fetchesStarted == 2 && self.languageClient.inFlight == 1 }
        prefs.setLanguage("es")
        debounce.fireAll()
        try await waitUntil { self.languageClient.pushes == ["es"] }
        await gate.open()
        await reconcile.value

        XCTAssertEqual(prefs.selectedLanguage, "es")
        XCTAssertEqual(languageClient.remote, "es")
    }

    /// A pick made while the LOGIN reconcile's fetch is in flight: the
    /// observers used to start only after that reconcile, so the pick was
    /// never marked pending and the account's older value overwrote it.
    func testAPickDuringTheLoginFetchIsKeptAndPushed() async throws {
        languageClient.remote = "de"
        let gate = AsyncGate()
        languageClient.beforeFetch = { await gate.wait() }
        let sync = makeSyncService()
        login()
        try await waitUntil { self.languageClient.fetchesStarted == 1 }

        prefs.setLanguage("es")
        languageClient.beforeFetch = nil
        await gate.open()

        try await waitUntil { self.languageClient.remote == "es" }
        XCTAssertEqual(prefs.selectedLanguage, "es")
        _ = sync
    }

    /// A genuine logout forgets the unsent pick — the next sign-in may be a
    /// different account, which must not inherit it.
    func testLogoutForgetsAnUnsentLanguagePick() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.selectedLanguage == "de" }
        try await Task.sleep(for: .milliseconds(100))
        languageClient.shouldFailPush = true
        prefs.setLanguage("es")
        XCTAssertEqual(prefs.pendingLanguagePush, "es")

        userSubject.send(nil)
        try await waitUntil { self.prefs.pendingLanguagePush == nil }
        _ = sync
    }

    // MARK: - Server authoritative after first sync

    func testServerAuthoritativeAfterFirstSyncDropsStaleLocal() async throws {
        client.remote[pl] = ["Film A"]
        let sync1 = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Film A"] }
        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))

        // Another device removed "Film A" from the account.
        client.remote[pl] = []

        // Fresh session restore (publisher starts nil, then the user) — must
        // NOT clear the flag on the initial nil, and must MIRROR the now-empty
        // server rather than re-union the stale local copy back in.
        let userSubject2 = CurrentValueSubject<UserProfile?, Never>(nil)
        let sync2 = StateSyncService(prefs: prefs, userPublisher: userSubject2.eraseToAnyPublisher(), client: client, languageClient: languageClient)
        userSubject2.send(UserProfile(displayName: "Test", email: "test@test.com", avatarUrl: nil, provider: "google"))

        try await waitUntil { self.prefs.hiddenFilms.isEmpty }
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, client.etagOf(pl))
        _ = (sync1, sync2)
    }

    func testNotModifiedLeavesLocalStateUntouched() async throws {
        client.remote[pl] = ["Film A"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        let validators = prefs.hiddenFilmsValidators(country: pl)
        // The account is unchanged, so the conditional fetch answers 304.
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.hiddenFilms, ["Film A"])
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, validators.etag) // unchanged — 304 carried no fresh validators to store
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).lastModified, validators.lastModified)
    }

    /// The reconcile a FOREGROUND RESUME triggers is the exact same public
    /// entry point `ContentView`'s `scenePhase` handler calls — this proves
    /// the entry point itself does a real reconcile, independent of login.
    func testReconcileCurrentCountryPicksUpARemoteChangeWithoutARelaunch() async throws {
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        client.remote[pl] = ["Hidden Elsewhere"]
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.hiddenFilms, ["Hidden Elsewhere"])
    }

    func testLogoutReArmsMigrationForEveryCountry() async throws {
        client.remote[pl] = ["Film A"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        userSubject.send(nil) // logout
        try await waitUntil { !self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        XCTAssertNil(prefs.hiddenFilmsValidators(country: pl).etag) // validators forgotten too
        _ = sync
    }

    func testNoSyncWhenNotLoggedIn() async throws {
        client.remote[pl] = ["Film A"]
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
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        let pushed = expectation(description: "hide pushed")
        client.onHide = { pushed.fulfill() }
        prefs.hide("New Hide")

        // The OLD debounce was 400ms — a push landing well before that proves
        // there's no debounce left for hiddenFilms.
        await fulfillment(of: [pushed], timeout: 0.2)
        XCTAssertEqual(client.hideCalls.last?.title, "New Hide")
        try await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl).isEmpty }
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, client.etagOf(pl))
        _ = sync
    }

    func testUnhideFiresImmediately() async throws {
        client.remote[pl] = ["Was Hidden"]
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
        client.remote[pl] = ["A", "B"]
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

    /// A hide whose push failed used to be left to a "later reconcile" that
    /// never re-sent it: the conditional fetch answered 304 (or a 200 without
    /// the title, dropping it locally). The failed write is queued and the
    /// next reconcile sends it before it fetches.
    func testAFailedHidePushIsResentOnTheNextReconcile() async throws {
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        client.shouldFailWrite = true
        let attempted = expectation(description: "hide attempted")
        client.onHide = { attempted.fulfill() }
        prefs.hide("Offline Hide")
        await fulfillment(of: [attempted], timeout: 1)
        client.onHide = nil

        client.shouldFailWrite = false
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(client.hideCalls.map(\.title), ["Offline Hide", "Offline Hide"])
        XCTAssertEqual(prefs.hiddenFilms, ["Offline Hide"])
        XCTAssertEqual(prefs.pendingHiddenFilmsChanges(country: pl), [])
        _ = sync
    }

    /// Failed unhide: the reconcile must not resurrect the title from the
    /// server's stale set before the unhide reaches it.
    func testAFailedUnhidePushIsResentBeforeTheReconcileFetches() async throws {
        client.remote[pl] = ["Was Hidden"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["Was Hidden"] }

        client.shouldFailWrite = true
        let attempted = expectation(description: "unhide attempted")
        client.onUnhide = { attempted.fulfill() }
        prefs.unhide("Was Hidden")
        await fulfillment(of: [attempted], timeout: 1)
        client.onUnhide = nil

        client.shouldFailWrite = false
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(client.unhideCalls.count, 2)
        XCTAssertEqual(prefs.hiddenFilms, [])
        _ = sync
    }

    /// A hide made while a reconcile's fetch is on the wire is newer than the
    /// set that fetch carries: applying the response would drop it locally
    /// (and store validators for a set this device no longer holds) until the
    /// next resume. The reconcile leaves local alone; the hide's own write
    /// brings the server level. Mirrors Android.
    func testAHideMadeDuringAReconcileFetchSurvivesItsResponse() async throws {
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        // The server's answer, read before the hide reaches it.
        client.remote[pl] = ["Elsewhere"] // another device
        client.fetchDelay[pl] = .milliseconds(300)
        let fetchesBefore = client.fetchedCountries.count
        let reconcile = Task { await sync.reconcileCurrentCountry() }
        try await waitUntil { self.client.fetchedCountries.count > fetchesBefore }
        prefs.hide("Mid Fetch")
        await reconcile.value

        XCTAssertEqual(prefs.hiddenFilms, ["Mid Fetch"])
        client.fetchDelay[pl] = nil
        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.hiddenFilms, ["Elsewhere", "Mid Fetch"])
    }

    /// An edit still on the wire at a logout: the logout forgets the queue,
    /// and the next session queues its own edits. When the old response
    /// lands it must not dequeue the NEW session's first edit as if it were
    /// the one just sent — that edit would never reach the server. Mirrors Android.
    func testAResponseLandingAfterALogoutLeavesTheNextSessionsQueueAlone() async throws {
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        let gate = AsyncGate()
        client.beforeWriteResponse = { await gate.wait() }
        prefs.hide("First")
        try await waitUntil { self.client.remote[self.pl] == ["First"] } // applied, response on the wire
        userSubject.send(nil)
        try await waitUntil { !self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        login()
        try await Task.sleep(for: .milliseconds(100)) // the new session is observing edits
        client.beforeWriteResponse = nil
        prefs.unhideAll()
        try await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl) == [.clearedAll] }
        await gate.open()

        try await waitUntil { self.client.clearCalls == [self.pl] && self.client.inFlight == 0 }
        XCTAssertEqual(client.remote[pl], [])
        _ = sync
    }

    /// A write's response is the server's WHOLE set. Its validators vouch for
    /// that set only — when it differs from the local bucket (another device
    /// changed it), storing them would make the next fetch a 304 that hides
    /// the difference forever. They are dropped instead.
    func testAPushResponseThatDiffersFromLocalDropsTheValidators() async throws {
        client.remote[pl] = ["A"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["A"] }

        let pushed = expectation(description: "hide pushed")
        client.onHide = { pushed.fulfill() }
        client.remote[pl]?.insert("From Elsewhere") // another device, since the last fetch
        prefs.hide("New")
        await fulfillment(of: [pushed], timeout: 1)
        try await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl).isEmpty }

        XCTAssertNil(prefs.hiddenFilmsValidators(country: pl).etag)
        _ = sync
    }

    // MARK: - Country switch

    func testCountrySwitchReconcilesTheNewlySelectedCountry() async throws {
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        XCTAssertFalse(client.fetchedCountries.contains(unitedKingdom.code))

        client.remote[unitedKingdom.code] = ["UK Film"]
        prefs.setCountry(unitedKingdom)

        try await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.unitedKingdom.code) }
        XCTAssertEqual(prefs.hiddenFilms, ["UK Film"])
        _ = sync
    }

    /// Switching to a country this device has never synced must show that
    /// country's bucket — not union the previous country's titles in and
    /// push them up as hides of the new one.
    func testCountrySwitchToUnsyncedCountryDoesNotLeakThePreviousCountrysHides() async throws {
        client.remote[pl] = ["PL Film"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }

        client.remote[unitedKingdom.code] = ["UK Film"]
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
        client.remote[pl] = ["PL Film"]
        client.remote[unitedKingdom.code] = ["UK Film"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }
        prefs.setCountry(unitedKingdom)
        try await waitUntil { self.prefs.hiddenFilms == ["UK Film"] }

        // PL's bucket hasn't changed server-side, so a conditional GET with
        // PL's validators would answer 304.
        prefs.setCountry(Country.all.first { $0.code == pl }!)

        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }
        _ = sync
    }

    /// A reconcile still in flight when the user switches country must not
    /// land its (old-country) result over the newly selected country's set —
    /// it belongs in the old country's own bucket.
    func testReconcileResultForAPreviouslySelectedCountryLandsInItsOwnBucket() async throws {
        client.remote[pl] = ["PL Film"]
        client.remote[unitedKingdom.code] = ["UK Film"]
        let sync = makeSyncService()
        login()
        try await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }

        client.remote[pl] = ["PL Film", "PL Film 2"]
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
        client.remote[pl] = []
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
