import XCTest
import KinowoTestSupport
@testable import KinowoAuth

@MainActor
final class StateSyncServiceTests: XCTestCase {

    private var defaults: UserDefaults!
    private var prefs: UserPreferences!
    private var client: FakeHiddenFilmsClient!
    private var languageClient: FakeLanguageClient!
    private var account: SignedInAccount!

    override func setUp() async throws {
        try await super.setUp()
        defaults = .scratch(suiteName: "StateSyncServiceTests")
        prefs = UserPreferences(store: defaults)
        client = FakeHiddenFilmsClient()
        languageClient = FakeLanguageClient()
        account = SignedInAccount()
    }

    override func tearDown() async throws {
        defaults.discardScratch(suiteName: "StateSyncServiceTests")
        try await super.tearDown()
    }

    /// The language push's debounce, advanced by hand (`debounce.fire()`),
    /// so a pick's push costs no 400 ms wait. The production clock has its own
    /// tests (`TaskDebouncerTests`).
    private let debounce = ManualDebouncer()

    private func makeSyncService() -> StateSyncService {
        StateSyncService(
            prefs: prefs,
            signedInUser: { [account] in account!.user },
            client: client,
            languageClient: languageClient,
            languagePushDebouncer: debounce
        )
    }

    /// Take main-queue turns until `cond` holds. No wall clock: the service,
    /// its fakes and `AsyncGate` all run on the main queue, so a state the
    /// code under test reaches, it reaches within a bounded number of turns
    /// however loaded the machine is — a wall-clock budget is spent by a
    /// process merely waiting for a core.
    private func waitUntil(turns: Int = 1_000, _ cond: () -> Bool,
                           file: StaticString = #filePath, line: UInt = #line) async {
        for _ in 0..<turns {
            if cond() { return }
            await nextMainQueueTurn()
        }
        if !cond() { XCTFail("condition not met within \(turns) main-queue turns", file: file, line: line) }
    }

    /// One turn of the main queue: resumes behind every block already queued
    /// on it. Not `Task.yield()` — that lets main-actor jobs run but can come
    /// back before a `DispatchQueue.main` block gets its turn.
    private func nextMainQueueTurn() async {
        await withCheckedContinuation { continuation in DispatchQueue.main.async { continuation.resume() } }
    }

    /// Let whatever the last step set off run out — for an assertion that
    /// something did NOT happen. A (wrongly) triggered request is a few
    /// main-queue turns away, well inside these.
    /// Let a change to observed state (the signed-in user, the selected
    /// country) reach the service: its observation takes a few main-actor hops.
    private func letTheServiceHear() async {
        for _ in 0..<5 { await nextMainQueueTurn() }
    }

    private func settle() async {
        for _ in 0..<50 { await nextMainQueueTurn() }
    }

    /// The login reconcile has finished — language adopted, the country's
    /// set migrated, nothing on the wire — so the change observers are live.
    private func loginSettled(language: String) async {
        await waitUntil {
            self.prefs.selectedLanguage == language && self.languageClient.inFlight == 0
                && self.prefs.isHiddenFilmsMigrated(country: self.pl)
        }
    }

    private func login() {
        account.signIn()
    }

    private let pl = "pl"
    private let unitedKingdom = Country.all.first { $0.code == "uk" }!

    // MARK: - First sync (per-country migration)

    func testLoginSyncsRemoteHiddenIntoEmptyLocal() async throws {
        client.remote[pl] = ["Film A", "Film B"]
        let sync = makeSyncService()

        login()
        await waitUntil { self.prefs.hiddenFilms == ["Film A", "Film B"] }

        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, client.etagOf(pl))
        XCTAssertTrue(client.hideCalls.isEmpty) // nothing local-only to push
        _ = sync
    }

    func testLoginMergesLocalAndRemoteHiddenPushingOnlyTheLocalOnlyDiff() async throws {
        prefs.hide("Local Only")
        client.remote[pl] = ["Remote Only"]
        let sync = makeSyncService()

        login()
        await waitUntil { self.client.hideCalls.map(\.title) == ["Local Only"] }
        await waitUntil { self.prefs.hiddenFilms == ["Local Only", "Remote Only"] }
        // Migrated once the queued hide's response has landed.
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        XCTAssertEqual(client.hideCalls.count, 1)
        XCTAssertEqual(client.hideCalls.first?.country, pl)
        XCTAssertEqual(client.hideCalls.first?.title, "Local Only")
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
        await waitUntil { self.prefs.hiddenFilms == ["Film A"] }

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

        await waitUntil { self.prefs.selectedLanguage == "de" }
        _ = sync
    }

    /// The account has no pick yet, but this device does (set before login,
    /// e.g. while anonymous) — it gets adopted as the account's, same
    /// "migrate this device's local state up" spirit as the sets.
    func testLoginPushesLocalExplicitLanguageWhenAccountHasNone() async throws {
        prefs.setLanguage("es")
        languageClient.remote = nil
        let sync = makeSyncService()

        login()
        await waitUntil { self.languageClient.pushes == ["es"] }
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
        await loginSettled(language: resolvedAtInit)
        XCTAssertFalse(debounce.hasPending)

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
        // expectation to await; wait for it to settle so the observer the
        // pick below needs is live.
        await loginSettled(language: prefs.selectedLanguage)

        prefs.setLanguage("de")
        debounce.fire()
        await waitUntil { self.languageClient.pushes == ["de"] }
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
        await loginSettled(language: "de")

        prefs.setLanguage("es")
        await Task.yield()
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.selectedLanguage, "es")
        await waitUntil { self.languageClient.remote == "es" }
        // The debounce runs out after the reconcile already sent the pick:
        // it must find nothing left to send.
        debounce.fire()
        await Task.yield()
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
        await loginSettled(language: "de")

        languageClient.remote = "pl"
        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.selectedLanguage, "pl")

        // Whatever debounce the adoption left armed runs out: still no push.
        XCTAssertFalse(debounce.hasPending)
        debounce.fire()
        await Task.yield()
        XCTAssertEqual(languageClient.pushes, [])
        _ = sync
    }

    /// A push the server rejected is still pending: the next reconcile
    /// retries it rather than adopting the account's older value.
    func testAFailedLanguagePushIsRetriedOnTheNextReconcile() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await loginSettled(language: "de")

        languageClient.shouldFailPush = true
        let attemptsBefore = languageClient.pushesStarted
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted > attemptsBefore && self.languageClient.inFlight == 0 }

        languageClient.shouldFailPush = false
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.selectedLanguage, "es")
        XCTAssertEqual(languageClient.remote, "es")
        _ = sync
    }

    /// A hide the server refuses for good (a title over its length bound: a
    /// 400) can never land: kept queued, it was re-sent on every reconcile and
    /// held back every edit queued behind it, and the reconcile never fetched
    /// again. It is dropped — as the web drops it — the edits behind it land,
    /// and the server's list replaces the title nobody will ever store.
    func testAHideRefusedForGoodIsDroppedAndTheEditsBehindItLand() async throws {
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        let overLong = String(repeating: "x", count: FakeHiddenFilmsClient.maxTitleLength + 1)
        prefs.hide(overLong)
        prefs.hide("Film A")
        await waitUntil { self.client.remote[self.pl] == ["Film A"] }
        await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl).isEmpty }

        let refusedSends = client.hideCalls.filter { $0.title == overLong }.count
        await sync.reconcileCurrentCountry()
        XCTAssertEqual(client.hideCalls.filter { $0.title == overLong }.count, refusedSends,
                       "the refused hide must not be sent again")
        XCTAssertEqual(prefs.hiddenFilms(country: pl), ["Film A"])
        _ = sync
    }

    /// The same refusal met by a first sign-in's union: it must not stop the
    /// country migrating (every later reconcile would redo the union and meet
    /// it again), and the list still ends on the server's.
    func testAHideRefusedForGoodInTheFirstSyncUnionStillMigrates() async throws {
        let overLong = String(repeating: "x", count: FakeHiddenFilmsClient.maxTitleLength + 1)
        prefs.hide(overLong)
        prefs.hide("Film A")
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        XCTAssertEqual(client.remote[pl], ["Film A"])

        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.hiddenFilms(country: pl), ["Film A"])
        XCTAssertEqual(client.hideCalls.filter { $0.title == overLong }.count, 1,
                       "the refused hide must not be sent again")
        _ = sync
    }

    /// The first sign-in's union uploads each local-only title while the user
    /// can already edit: an unhide of one of them, sent beside the union's own
    /// hide, could reach the server first and leave the title hidden there but
    /// not here — with validators vouching for the server's set, so every
    /// later fetch was a 304 that kept the two apart. The union's hides go
    /// through the edit queue instead, so the user's edit is sent after them.
    func testAnUnhideDuringTheFirstSyncUnionLandsAfterItsHide() async throws {
        prefs.hide("Local Only")
        client.remote[pl] = []
        let gate = AsyncGate()
        client.beforeWriteApplied = { [client] in
            if client!.hideCalls.count == 1 && client!.unhideCalls.isEmpty { await gate.wait() }
        }
        let sync = makeSyncService()
        login()
        await waitUntil { self.client.hideCalls.count == 1 } // the union's hide, on its way

        prefs.unhide("Local Only")
        await settle() // an unhide sent beside it lands first
        gate.open()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) && self.client.inFlight == 0 }
        await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl).isEmpty && self.client.inFlight == 0 }

        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.hiddenFilms(country: pl), [])
        XCTAssertEqual(client.remote[pl], [])
    }

    /// A push the server refuses for good (a language this server does not
    /// know) can never land: kept pending, it was re-pushed on every reconcile
    /// and the account's own pick never came back. It is dropped, and the
    /// account's pick adopted, without pushing the refused one again.
    func testALanguagePushRefusedForGoodIsDroppedForTheAccountsPick() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await loginSettled(language: "de")

        languageClient.refusePush = true
        let attemptsBefore = languageClient.pushesStarted
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted > attemptsBefore && self.languageClient.inFlight == 0 }

        await waitUntil { self.prefs.selectedLanguage == "de" }
        let attempts = languageClient.pushesStarted
        await sync.reconcileCurrentCountry()
        XCTAssertEqual(prefs.selectedLanguage, "de")
        XCTAssertEqual(languageClient.pushesStarted, attempts, "the refused pick must not be pushed again")
        _ = sync
    }

    /// After a refusal the account's pick is fetched to take its place — but a
    /// pick the user makes while that fetch is on the wire is newer than its
    /// answer. Adopting it anyway reverted the screen to the account's
    /// language while the new pick was still pushed, leaving the device and
    /// the account on different languages.
    func testAPickMadeWhileARefusedPushFetchesTheAccountsPickIsKept() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await loginSettled(language: "de")

        languageClient.refusePush = true
        let gate = AsyncGate()
        languageClient.onPush = { [languageClient] _ in
            languageClient!.refusePush = false
            languageClient!.beforeFetch = { await gate.wait() }
        }
        let fetchesBefore = languageClient.fetchesStarted
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.fetchesStarted > fetchesBefore } // refused, fetching
        prefs.setLanguage("fr")
        gate.open()
        debounce.fire()

        await waitUntil { self.languageClient.remote == "fr" && self.languageClient.inFlight == 0 }
        XCTAssertEqual(prefs.selectedLanguage, "fr")
        _ = sync
    }

    /// A pick whose push failed survives a relaunch: the next session's
    /// login reconcile pushes it instead of restoring the account's older
    /// value over it.
    func testAFailedLanguagePushSurvivesARelaunch() async throws {
        languageClient.remote = "de"
        let sync1 = makeSyncService()
        login()
        await loginSettled(language: "de")

        languageClient.shouldFailPush = true
        let attemptsBefore = languageClient.pushesStarted
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted > attemptsBefore && self.languageClient.inFlight == 0 }
        languageClient.shouldFailPush = false

        // Relaunch: fresh prefs over the same store, fresh service, session restored.
        prefs = UserPreferences(store: defaults)
        let account2 = SignedInAccount()
        let sync2 = StateSyncService(prefs: prefs, signedInUser: { account2.user }, client: client, languageClient: languageClient)
        account2.signIn()
        await waitUntil { self.languageClient.remote == "es" }
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
        await loginSettled(language: "de")

        let gate = AsyncGate()
        languageClient.beforePush = { await gate.wait() }
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted == 1 }
        prefs.setLanguage("de")
        languageClient.beforePush = nil
        gate.open()
        debounce.fire()

        await waitUntil { self.languageClient.pushes.last == "de" }
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
        await loginSettled(language: "de")

        let gate = AsyncGate()
        languageClient.beforePushResponse = { await gate.wait(); throw URLError(.networkConnectionLost) }
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted == 1 }
        prefs.setLanguage("de")
        gate.open()
        await waitUntil { self.languageClient.inFlight == 0 }
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
        await loginSettled(language: "de")

        let gate = AsyncGate()
        languageClient.beforePushResponse = { await gate.wait() }
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted == 1 }
        prefs.setLanguage("fr")
        debounce.fire() // past the debounce, with the first push still on the wire
        await Task.yield()
        XCTAssertEqual(languageClient.pushesStarted, 1)
        gate.open()

        await waitUntil { self.languageClient.remote == "fr" && self.languageClient.inFlight == 0 }
        XCTAssertEqual(languageClient.maxPushesInFlight, 1)
        XCTAssertEqual(languageClient.pushes, ["es", "fr"])
        _ = sync
    }

    /// A pick whose debounce fires while an earlier pick's push is on the
    /// wire waits for that push — and must still be sent when that push
    /// FAILS: the failure was the older pick's, not this one's. It used to
    /// wait on the failed send and then give up, leaving the newer pick
    /// unsent until the next reconcile. Mirrors Android, whose waiter takes
    /// the send lock after the failure and sends it.
    func testAPickWaitingOnAFailedPushIsSentAfterIt() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.selectedLanguage == "de" && self.languageClient.inFlight == 0 }

        // Only the held push fails: it arms the failure once released, and
        // disarms it as it answers.
        let gate = AsyncGate()
        languageClient.beforePush = { await gate.wait(); self.languageClient.shouldFailPush = true }
        languageClient.onPush = { _ in self.languageClient.shouldFailPush = false }
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushesStarted == 1 }
        languageClient.beforePush = nil
        prefs.setLanguage("fr")
        debounce.fire()
        await Task.yield()
        gate.open()

        await waitUntil { self.languageClient.pushes == ["fr"] && self.languageClient.inFlight == 0 }
        XCTAssertEqual(languageClient.remote, "fr")
        _ = sync
    }

    /// A pick made AND sent while a reconcile's fetch is on the wire: the
    /// fetch's answer predates it, so adopting that answer would put the
    /// account's older pick back over the one it just confirmed. Mirrors Android.
    func testAPickSentDuringAReconcileFetchSurvivesItsAnswer() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.selectedLanguage == "de" && self.languageClient.inFlight == 0 }

        let gate = AsyncGate()
        languageClient.beforeFetchResponse = { await gate.wait() }
        let reconcile = Task { await sync.reconcileCurrentCountry() }
        await waitUntil { self.languageClient.fetchesStarted == 2 && self.languageClient.inFlight == 1 }
        prefs.setLanguage("es")
        debounce.fire()
        await waitUntil { self.languageClient.pushes == ["es"] }
        gate.open()
        await reconcile.value

        XCTAssertEqual(prefs.selectedLanguage, "es")
        XCTAssertEqual(languageClient.remote, "es")
    }

    /// Picking pl, then es, then pl again while the first pl is on the wire,
    /// with another device picking en in between: the final pl is a NEW pick,
    /// newer than en, even though it equals the value just confirmed — it
    /// must be sent, not taken as already done. Mirrors Android.
    func testARepeatedPickMadeWhileItsTwinIsOnTheWireIsSentAgain() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.selectedLanguage == "de" && self.languageClient.inFlight == 0 }

        let gate = AsyncGate()
        languageClient.beforePushResponse = { await gate.wait() }
        prefs.setLanguage("pl")
        debounce.fire()
        await waitUntil { self.languageClient.remote == "pl" } // applied, its response on the wire
        languageClient.beforePushResponse = nil
        prefs.setLanguage("es")
        languageClient.remote = "en" // another device
        prefs.setLanguage("pl")
        debounce.fire()
        gate.open()

        await waitUntil { self.languageClient.inFlight == 0 && self.languageClient.pushesStarted >= 2 }
        XCTAssertEqual(languageClient.remote, "pl")
        _ = sync
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
        await waitUntil { self.languageClient.fetchesStarted == 1 }

        prefs.setLanguage("es")
        debounce.fire()
        languageClient.beforeFetch = nil
        gate.open()

        await waitUntil { self.languageClient.remote == "es" }
        XCTAssertEqual(prefs.selectedLanguage, "es")
        _ = sync
    }

    /// A genuine logout forgets the unsent pick — the next sign-in may be a
    /// different account, which must not inherit it.
    func testLogoutForgetsAnUnsentLanguagePick() async throws {
        languageClient.remote = "de"
        let sync = makeSyncService()
        login()
        await loginSettled(language: "de")
        languageClient.shouldFailPush = true
        prefs.setLanguage("es")
        XCTAssertEqual(prefs.pendingLanguagePush, "es")

        account.user = nil
        await waitUntil { self.prefs.pendingLanguagePush == nil }
        _ = sync
    }

    // MARK: - Server authoritative after first sync

    func testServerAuthoritativeAfterFirstSyncDropsStaleLocal() async throws {
        client.remote[pl] = ["Film A"]
        let sync1 = makeSyncService()
        login()
        await waitUntil { self.prefs.hiddenFilms == ["Film A"] }
        XCTAssertTrue(prefs.isHiddenFilmsMigrated(country: pl))

        // Another device removed "Film A" from the account.
        client.remote[pl] = []

        // Fresh session restore (publisher starts nil, then the user) — must
        // NOT clear the flag on the initial nil, and must MIRROR the now-empty
        // server rather than re-union the stale local copy back in.
        let account2 = SignedInAccount()
        let sync2 = StateSyncService(prefs: prefs, signedInUser: { account2.user }, client: client, languageClient: languageClient)
        account2.signIn()

        await waitUntil { self.prefs.hiddenFilms.isEmpty }
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, client.etagOf(pl))
        _ = (sync1, sync2)
    }

    func testNotModifiedLeavesLocalStateUntouched() async throws {
        client.remote[pl] = ["Film A"]
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

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
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        client.remote[pl] = ["Hidden Elsewhere"]
        await sync.reconcileCurrentCountry()

        XCTAssertEqual(prefs.hiddenFilms, ["Hidden Elsewhere"])
    }

    func testLogoutReArmsMigrationForEveryCountry() async throws {
        client.remote[pl] = ["Film A"]
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        account.user = nil // logout
        await waitUntil { !self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        XCTAssertNil(prefs.hiddenFilmsValidators(country: pl).etag) // validators forgotten too
        _ = sync
    }

    func testNoSyncWhenNotLoggedIn() async throws {
        client.remote[pl] = ["Film A"]
        let sync = makeSyncService()

        // Nothing to wait FOR: give a (wrongly) triggered sync its turns.
        await settle()

        XCTAssertTrue(prefs.hiddenFilms.isEmpty)
        XCTAssertTrue(client.fetchedCountries.isEmpty)
        _ = sync
    }

    func testFetchFailurePreservesLocalStateAndLeavesMigrationUnset() async throws {
        prefs.hide("My Film")
        client.shouldFailFetch = true
        let sync = makeSyncService()

        login()
        await waitUntil { self.client.fetchedCountries == [self.pl] && self.client.inFlight == 0 }

        XCTAssertEqual(prefs.hiddenFilms, ["My Film"])
        XCTAssertFalse(prefs.isHiddenFilmsMigrated(country: pl))
        _ = sync
    }

    // MARK: - Immediate (non-debounced) writes

    /// A write sent "immediately" is on the wire within this many main-queue
    /// turns of the edit (the edit's Task, the flush's Task, the request).
    private let immediateTurns = 10

    func testHideFiresImmediatelyNoDebounce() async throws {
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        prefs.hide("New Hide")

        // The OLD debounce was 400ms — a push sent within a few main-queue
        // turns, with no time passing on any clock, proves there's no debounce
        // left for hiddenFilms.
        await waitUntil(turns: immediateTurns) { !self.client.hideCalls.isEmpty }
        XCTAssertEqual(client.hideCalls.last?.title, "New Hide")
        await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl).isEmpty }
        XCTAssertEqual(prefs.hiddenFilmsValidators(country: pl).etag, client.etagOf(pl))
        _ = sync
    }

    func testUnhideFiresImmediately() async throws {
        client.remote[pl] = ["Was Hidden"]
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.hiddenFilms == ["Was Hidden"] }

        prefs.unhide("Was Hidden")

        await waitUntil(turns: immediateTurns) { !self.client.unhideCalls.isEmpty }
        XCTAssertEqual(client.unhideCalls.last?.title, "Was Hidden")
        _ = sync
    }

    func testUnhideAllCallsClearNotIndividualUnhides() async throws {
        client.remote[pl] = ["A", "B"]
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.hiddenFilms == ["A", "B"] }

        prefs.unhideAll()

        await waitUntil(turns: immediateTurns) { !self.client.clearCalls.isEmpty }
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
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        client.shouldFailWrite = true
        prefs.hide("Offline Hide")
        await waitUntil { self.client.hideCalls.count == 1 && self.client.inFlight == 0 }

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
        await waitUntil { self.prefs.hiddenFilms == ["Was Hidden"] }

        client.shouldFailWrite = true
        prefs.unhide("Was Hidden")
        await waitUntil { self.client.unhideCalls.count == 1 && self.client.inFlight == 0 }

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
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        // The server's answer, read before the hide reaches it.
        client.remote[pl] = ["Elsewhere"] // another device
        let onTheWire = AsyncGate()
        client.fetchHold[pl] = onTheWire
        let fetchesBefore = client.fetchedCountries.count
        let reconcile = Task { await sync.reconcileCurrentCountry() }
        await waitUntil { self.client.fetchedCountries.count > fetchesBefore }
        prefs.hide("Mid Fetch")
        onTheWire.open()
        await reconcile.value

        XCTAssertEqual(prefs.hiddenFilms, ["Mid Fetch"])
        client.fetchHold[pl] = nil
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
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        let gate = AsyncGate()
        client.beforeWriteResponse = { await gate.wait() }
        prefs.hide("First")
        await waitUntil { self.client.remote[self.pl] == ["First"] } // applied, response on the wire
        account.user = nil
        await waitUntil { !self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        let fetchesBefore = languageClient.fetchesStarted
        login()
        await waitUntil { self.languageClient.fetchesStarted > fetchesBefore } // the new session's reconcile started, so it is observing edits
        client.beforeWriteResponse = nil
        prefs.unhideAll()
        await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl) == [.clearedAll] }
        gate.open()

        await waitUntil { self.client.clearCalls == [self.pl] && self.client.inFlight == 0 }
        XCTAssertEqual(client.remote[pl], [])
        _ = sync
    }

    /// The login reconcile's first-sync fetch still on the wire at a logout:
    /// when it lands it must neither queue this device's hides — the next
    /// sign-in would send them to whichever account that is — nor merge the
    /// signed-out account's set into the device's. Mirrors Android.
    func testAFirstSyncFetchLandingAfterALogoutQueuesNothingForTheNextAccount() async throws {
        prefs.hide("Mine")
        client.remote[pl] = ["Theirs"]
        let gate = AsyncGate()
        client.beforeFetchResponse = { await gate.wait() }
        let sync = makeSyncService()
        login()
        await waitUntil { self.client.fetchedCountries.contains(self.pl) } // parked on the wire
        account.user = nil
        await letTheServiceHear() // the logout
        gate.open()
        await waitUntil { self.client.inFlight == 0 }
        await settle()

        XCTAssertEqual(prefs.pendingHiddenFilmsChanges(country: pl), [])
        XCTAssertEqual(prefs.hiddenFilms(country: pl), ["Mine"])
        XCTAssertTrue(client.hideCalls.isEmpty)
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
        await waitUntil { self.prefs.hiddenFilms == ["A"] }

        client.remote[pl]?.insert("From Elsewhere") // another device, since the last fetch
        prefs.hide("New")
        await waitUntil { !self.client.hideCalls.isEmpty }
        await waitUntil { self.prefs.pendingHiddenFilmsChanges(country: self.pl).isEmpty }

        XCTAssertNil(prefs.hiddenFilmsValidators(country: pl).etag)
        _ = sync
    }

    // MARK: - Country switch

    func testCountrySwitchReconcilesTheNewlySelectedCountry() async throws {
        client.remote[pl] = []
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        XCTAssertFalse(client.fetchedCountries.contains(unitedKingdom.code))

        client.remote[unitedKingdom.code] = ["UK Film"]
        prefs.setCountry(unitedKingdom)

        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.unitedKingdom.code) }
        XCTAssertEqual(prefs.hiddenFilms, ["UK Film"])
        _ = sync
    }

    /// A country switch while the login reconcile's fetch is on the wire: the
    /// switch was only observed once that reconcile finished, so the new
    /// country went unsynced until the next resume.
    func testCountrySwitchDuringTheLoginReconcileReconcilesTheNewCountry() async throws {
        client.remote[pl] = ["PL Film"]
        client.remote[unitedKingdom.code] = ["UK Film"]
        let onTheWire = AsyncGate()
        client.fetchHold[pl] = onTheWire
        let sync = makeSyncService()
        login()
        await waitUntil { self.client.fetchedCountries.contains(self.pl) }

        prefs.setCountry(unitedKingdom)
        await letTheServiceHear() // the switch
        onTheWire.open()

        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.unitedKingdom.code) }
        XCTAssertEqual(prefs.hiddenFilms, ["UK Film"])
        XCTAssertEqual(prefs.hiddenFilms(country: pl), ["PL Film"])
        _ = sync
    }

    /// A hide belongs to the country it was made in, even when the country
    /// changes before the next main-queue turn: the edit was queued only after
    /// a hop to the main queue, under whatever country was selected by then.
    func testAHideIsSentToTheCountryItWasMadeIn() async throws {
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }

        prefs.hide("PL Film")
        prefs.setCountry(unitedKingdom)

        await waitUntil { !self.client.hideCalls.isEmpty && self.client.inFlight == 0 }
        XCTAssertEqual(client.hideCalls.map(\.country), [pl])
        XCTAssertEqual(client.remote[pl], ["PL Film"])
        _ = sync
    }

    /// Switching to a country this device has never synced must show that
    /// country's bucket — not union the previous country's titles in and
    /// push them up as hides of the new one.
    func testCountrySwitchToUnsyncedCountryDoesNotLeakThePreviousCountrysHides() async throws {
        client.remote[pl] = ["PL Film"]
        let sync = makeSyncService()
        login()
        await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }

        client.remote[unitedKingdom.code] = ["UK Film"]
        prefs.setCountry(unitedKingdom)

        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.unitedKingdom.code) }
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
        await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }
        prefs.setCountry(unitedKingdom)
        await waitUntil { self.prefs.hiddenFilms == ["UK Film"] }

        // PL's bucket hasn't changed server-side, so a conditional GET with
        // PL's validators would answer 304.
        prefs.setCountry(Country.all.first { $0.code == pl }!)

        await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }
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
        await waitUntil { self.prefs.hiddenFilms == ["PL Film"] }

        client.remote[pl] = ["PL Film", "PL Film 2"]
        let onTheWire = AsyncGate()
        client.fetchHold[pl] = onTheWire
        let slowResume = Task { await sync.reconcileCurrentCountry() }
        await waitUntil { self.client.fetchedCountries.count == 2 } // the slow fetch is on the wire
        prefs.setCountry(unitedKingdom)
        await waitUntil { self.prefs.hiddenFilms == ["UK Film"] }

        onTheWire.open()
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
        await waitUntil { self.prefs.isHiddenFilmsMigrated(country: self.pl) }
        login()
        await settle() // the second emission is handled (or, wrongly, stacks observers)

        prefs.hide("New Hide")
        await waitUntil { self.client.hideCalls.count >= 1 && self.client.inFlight == 0 }
        await settle() // a stacked observer's second PUT would be here by now

        XCTAssertEqual(client.hideCalls.map(\.title), ["New Hide"])
        _ = sync
    }
}
