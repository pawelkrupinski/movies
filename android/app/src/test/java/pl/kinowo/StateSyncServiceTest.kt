package pl.kinowo

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.launch
import kotlinx.coroutines.test.TestScope
import kotlinx.coroutines.test.UnconfinedTestDispatcher
import kotlinx.coroutines.test.advanceTimeBy
import kotlinx.coroutines.test.advanceUntilIdle
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import pl.kinowo.auth.StateSyncService
import pl.kinowo.auth.UserProfile
import pl.kinowo.data.HiddenFilmsOp

/**
 * Mirrors iOS `StateSyncServiceTests`: per-country merge-on-login, immediate
 * per-title push (no debounce), the server-authoritative-after-first-sync
 * guarantee, foreground-resume reconcile, and the offline-preserves-local
 * guarantee — against an in-memory prefs + a fake per-country client.
 */
@OptIn(ExperimentalCoroutinesApi::class)
class StateSyncServiceTest {

    private lateinit var prefs: FakeSyncPrefs
    private lateinit var client: FakeHiddenFilmsClient
    private lateinit var languageClient: FakeLanguageClient
    private lateinit var userFlow: MutableStateFlow<UserProfile?>

    @Before
    fun setUp() {
        prefs = FakeSyncPrefs()
        client = FakeHiddenFilmsClient()
        languageClient = FakeLanguageClient()
        userFlow = MutableStateFlow(null)
    }

    private fun TestScope.startService(): StateSyncService =
        StateSyncService(prefs, userFlow, client, languageClient, backgroundScope).also { it.start() }

    private fun login() {
        userFlow.value = UserProfile(displayName = "Test", email = "test@test.com", provider = "google")
    }

    @Test
    fun loginSyncsRemoteHiddenIntoEmptyLocal() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film A", "Film B")
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Film A", "Film B"), prefs.hiddenState)
    }

    @Test
    fun loginMergesLocalAndRemoteHiddenPushingOnlyTheLocalOnlyTitles() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        prefs.hiddenByCountry["pl"] = setOf("Local Only")
        client.remote["pl"] = setOf("Remote Only")
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Local Only", "Remote Only"), prefs.hiddenState)
        // Only the title the server didn't already have was pushed — there is
        // no bulk write, so the diff must be exact, not "everything local".
        assertEquals(listOf("Local Only" to "pl"), client.hideCalls)
    }

    @Test
    fun noSyncWhenNotLoggedIn() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film A")
        startService()
        advanceUntilIdle()

        assertTrue(prefs.hiddenState.isEmpty())
        assertTrue(client.hideCalls.isEmpty())
    }

    @Test
    fun noCountryPickedYetSyncsTheDefaultCountryTheAppIsBrowsing() = runTest(UnconfinedTestDispatcher()) {
        // countryState left null — a manual city pick at the gate, or an
        // install that predates the country picker. The app is still browsing
        // Poland (Country.byCode(null)), so its hides must still sync as "pl".
        client.remote["pl"] = setOf("Film A")
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Film A"), prefs.hiddenState)
        assertEquals(listOf("pl"), client.fetchCalls)
    }

    @Test
    fun aLegacyUppercaseCountryCodeSyncsUnderItsCurrentServerCode() = runTest(UnconfinedTestDispatcher()) {
        // Earlier builds persisted ISO codes; the per-country API and the
        // migration-flag keys use the server's code space.
        prefs.countryState.value = "GB"
        prefs.hiddenByCountry["uk"] = setOf("Local Only")
        startService()
        login()
        advanceUntilIdle()

        assertEquals(listOf("Local Only" to "uk"), client.hideCalls)
    }

    @Test
    fun aHidePushedWithNoCountryPickedGoesToTheDefaultCountry() = runTest(UnconfinedTestDispatcher()) {
        val service = startService()
        login()
        advanceUntilIdle()

        service.hide("Film A")
        advanceUntilIdle()

        assertEquals(listOf("Film A" to "pl"), client.hideCalls)
    }

    @Test
    fun fetchFailurePreservesLocalState() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        prefs.hiddenByCountry["pl"] = setOf("My Film")
        client.shouldFailFetch = true
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("My Film"), prefs.hiddenState)
        assertTrue(client.hideCalls.isEmpty())
    }

    /** Regression: once migration has run for a country, a later reconcile
     *  MIRRORS the server for THAT country, not union. A film removed on
     *  another device (server now empty) must not be resurrected from this
     *  device's stale local copy. */
    @Test
    fun serverAuthoritativeAfterFirstSyncDropsStaleLocal() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film A")
        startService()
        login()
        advanceUntilIdle()
        assertEquals(setOf("Film A"), prefs.hiddenState)
        assertTrue(prefs.isHiddenFilmsMigrated("pl"))

        // Another device removes Film A from the account.
        client.remote["pl"] = emptySet()

        val userFlow2 = MutableStateFlow<UserProfile?>(null)
        StateSyncService(prefs, userFlow2, client, languageClient, backgroundScope).also { it.start() }
        userFlow2.value = UserProfile(displayName = "Test", email = "test@test.com", provider = "google")
        advanceUntilIdle()

        assertEquals(emptySet<String>(), prefs.hiddenState)
    }

    /** Each country migrates independently — a country already synced doesn't
     *  make a DIFFERENT, never-reconciled country server-authoritative too. */
    @Test
    fun migrationIsPerCountryNotGlobal() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film PL")
        startService()
        login()
        advanceUntilIdle()
        assertTrue(prefs.isHiddenFilmsMigrated("pl"))
        assertTrue(!prefs.isHiddenFilmsMigrated("us"))
    }

    @Test
    fun logoutReArmsMigration() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film A")
        startService()
        login()
        advanceUntilIdle()
        assertTrue(prefs.isHiddenFilmsMigrated("pl"))

        userFlow.value = null // logout
        advanceUntilIdle()
        assertTrue(!prefs.isHiddenFilmsMigrated("pl"))
    }

    @Test
    fun hideFiresImmediatelyWithNoDebounce() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        val service = startService()
        login()
        advanceUntilIdle() // merge completes

        service.hide("New Hide")
        runCurrent() // no advanceTimeBy — proves there's no debounce left to wait out

        assertTrue(client.hideCalls.contains("New Hide" to "pl"))
    }

    @Test
    fun unhideFiresImmediately() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        val service = startService()
        login()
        advanceUntilIdle()

        service.unhide("Gone")
        runCurrent()

        assertEquals(listOf("Gone" to "pl"), client.unhideCalls)
    }

    @Test
    fun clearFiresImmediately() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        val service = startService()
        login()
        advanceUntilIdle()

        service.clear()
        runCurrent()

        assertEquals(listOf("pl"), client.clearCalls)
    }

    /** A hide whose push failed used to be left to a "later reconcile" that
     *  never re-sent it: the conditional fetch answered 304 (or a 200 without
     *  the title, dropping it locally). The failed write is queued and the
     *  next reconcile sends it before it fetches. Mirrors iOS. */
    @Test
    fun aFailedHidePushIsResentOnTheNextReconcile() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.notModified = true
        val service = startService()
        login()
        advanceUntilIdle()

        client.shouldFailWrite = true
        prefs.hiddenByCountry["pl"] = setOf("Offline Hide") // the ViewModel's local write
        service.hide("Offline Hide")
        advanceUntilIdle()

        client.shouldFailWrite = false
        service.reconcileCurrentCountry()
        advanceUntilIdle()

        assertEquals(listOf("Offline Hide" to "pl", "Offline Hide" to "pl"), client.hideCalls)
        assertEquals(setOf("Offline Hide"), client.remote["pl"])
        assertEquals(setOf("Offline Hide"), prefs.hiddenState)
        assertEquals(emptyList<HiddenFilmsOp>(), prefs.pendingHiddenFilmsOps("pl"))
    }

    /** Failed unhide: the reconcile must not resurrect the title from the
     *  server's stale set before the unhide reaches it. */
    @Test
    fun aFailedUnhidePushIsResentBeforeTheReconcileFetches() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Was Hidden")
        val service = startService()
        login()
        advanceUntilIdle()

        client.shouldFailWrite = true
        prefs.hiddenByCountry["pl"] = emptySet()
        service.unhide("Was Hidden")
        advanceUntilIdle()

        client.shouldFailWrite = false
        service.reconcileCurrentCountry()
        advanceUntilIdle()

        assertEquals(2, client.unhideCalls.size)
        assertEquals(emptySet<String>(), prefs.hiddenState)
    }

    /** A write's response is the server's WHOLE set. Its validators vouch for
     *  that set only — when it differs from the local bucket (another device
     *  changed it), storing them would make the next fetch a 304 that hides
     *  the difference forever. They are dropped instead. */
    @Test
    fun aPushResponseThatDiffersFromLocalDropsTheValidators() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("A")
        val service = startService()
        login()
        advanceUntilIdle()

        client.remote["pl"] = setOf("A", "From Elsewhere")
        prefs.hiddenByCountry["pl"] = setOf("A", "New")
        service.hide("New")
        advanceUntilIdle()

        assertNull(prefs.hiddenFilmsEtag("pl"))
    }

    @Test
    fun writesNeverFireWhenLoggedOut() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        val service = startService()
        // never logged in
        service.hide("Nope")
        runCurrent()

        assertTrue(client.hideCalls.isEmpty())
    }

    @Test
    fun reconcileCurrentCountryPicksUpARemoteChangeOnForegroundResume() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film A")
        val service = startService()
        login()
        advanceUntilIdle()
        assertEquals(setOf("Film A"), prefs.hiddenState)

        client.remote["pl"] = setOf("Film A", "Film B") // changed elsewhere while backgrounded
        service.reconcileCurrentCountry()
        advanceUntilIdle()

        assertEquals(setOf("Film A", "Film B"), prefs.hiddenState)
    }

    // ── Country switches — hiddenFilms is per country on both sides ────────

    /** A switch re-runs the first-login union for a never-synced country.
     *  With one device-wide set, that union pulled the OLD country's hides in
     *  and uploaded them to the new country's account row. */
    @Test
    fun switchingToANeverSyncedCountryNeverUploadsTheOldCountrysHides() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film PL")
        startService()
        login()
        advanceUntilIdle()

        // MainActivity recreates on a country switch: a fresh service, same prefs.
        prefs.countryState.value = "uk"
        client.remote["uk"] = setOf("Film UK")
        val userAfterSwitch = MutableStateFlow<UserProfile?>(null)
        StateSyncService(prefs, userAfterSwitch, client, languageClient, backgroundScope).also { it.start() }
        userAfterSwitch.value = UserProfile(displayName = "Test", email = "test@test.com", provider = "google")
        advanceUntilIdle()

        assertTrue("nothing from Poland may be pushed to the UK row", client.hideCalls.none { it.second == "uk" })
        assertEquals(setOf("Film UK"), prefs.hiddenState)
        assertEquals(setOf("Film PL"), prefs.hiddenByCountry["pl"])
    }

    /** Switching back to an already-synced country usually answers 304 —
     *  which must leave THAT country's own set showing, not whatever the
     *  other country last put in a shared one. */
    @Test
    fun aNotModifiedOnSwitchBackKeepsThatCountrysOwnSet() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film PL")
        startService()
        login()
        advanceUntilIdle()
        prefs.setHiddenFilms("uk", setOf("Film UK"))

        client.notModified = true
        val service = StateSyncService(prefs, userFlow, client, languageClient, backgroundScope)
        service.reconcileCurrentCountry()

        assertEquals(setOf("Film PL"), prefs.hiddenState)
    }

    /** A reconcile still in flight when the country changes writes the result
     *  to the country it FETCHED, never onto the one selected by the time it
     *  lands. */
    @Test
    fun aReconcileLandingAfterASwitchWritesOnlyItsOwnCountry() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film PL")
        val gate = kotlinx.coroutines.CompletableDeferred<Unit>()
        client.beforeFetch = { gate.await() }
        startService()
        login()
        runCurrent() // the pl reconcile is now parked inside fetch

        prefs.countryState.value = "uk"
        prefs.setHiddenFilms("uk", setOf("Film UK"))
        gate.complete(Unit)
        advanceUntilIdle()

        assertEquals(setOf("Film UK"), prefs.hiddenState)
        assertEquals(setOf("Film PL"), prefs.hiddenByCountry["pl"])
    }

    /** A hide made while a reconcile's fetch is on the wire is newer than the
     *  set that fetch carries: applying the response would drop it locally
     *  (and store validators for a set this device no longer holds) until the
     *  next resume. The reconcile leaves local alone; the hide's own write
     *  brings the server level. Mirrors iOS. */
    @Test
    fun aHideMadeDuringAReconcileFetchSurvivesItsResponse() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        val service = startService()
        login()
        advanceUntilIdle()

        client.remote["pl"] = setOf("Elsewhere") // so the fetch answers 200, not 304
        val gate = CompletableDeferred<Unit>()
        client.beforeFetchResponse = { gate.await() }
        launch { service.reconcileCurrentCountry() }
        runCurrent() // the fetch has read the server's set and is parked
        prefs.setHiddenFilms("pl", setOf("Mid Fetch"))
        service.hide("Mid Fetch")
        runCurrent()
        gate.complete(Unit)
        advanceUntilIdle()

        assertEquals(setOf("Mid Fetch"), prefs.hiddenState)
        client.beforeFetchResponse = {}
        service.reconcileCurrentCountry()
        assertEquals(setOf("Elsewhere", "Mid Fetch"), prefs.hiddenState)
    }

    /** An edit is persisted BEFORE anything is sent, even while an earlier
     *  edit's request is still in flight — or a process death during that
     *  request loses it: local shows it, the server never gets it, and the
     *  next reconcile takes the server's set back. */
    @Test
    fun anEditIsQueuedWhileAnEarlierOneIsStillInFlight() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        val service = startService()
        login()
        advanceUntilIdle()

        val gate = CompletableDeferred<Unit>()
        client.beforeWrite = { gate.await() }
        service.hide("First")
        runCurrent() // First is on the wire
        service.hide("Second")
        runCurrent()

        assertEquals(listOf(HiddenFilmsOp.Hide("First"), HiddenFilmsOp.Hide("Second")), prefs.pendingHiddenFilmsOps("pl"))
        gate.complete(Unit)
        advanceUntilIdle()
        assertEquals(emptyList<HiddenFilmsOp>(), prefs.pendingHiddenFilmsOps("pl"))
        assertEquals(setOf("First", "Second"), client.remote["pl"])
    }

    /** A second non-null user emission (session re-check returning a changed
     *  profile) restarts the sync job rather than stacking a second one, so a
     *  local change is still pushed exactly once. */
    @Test
    fun aRepeatedLoginEmissionDoesNotDoubleTheLanguagePush() = runTest(UnconfinedTestDispatcher()) {
        startService()
        login()
        advanceUntilIdle()
        userFlow.value = UserProfile(displayName = "Test Renamed", email = "test@test.com", provider = "google")
        advanceUntilIdle()
        languageClient.pushCount = 0

        prefs.setLanguageTag("de")
        advanceTimeBy(500)
        runCurrent()

        assertEquals(1, languageClient.pushCount)
    }

    /** A reconcile cancelled mid-fetch (the ViewModel cleared on a country
     *  switch) must propagate the cancellation, not return as if it finished. */
    @Test
    fun aCancelledReconcileDoesNotCompleteNormally() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.beforeFetch = { kotlinx.coroutines.awaitCancellation() }
        val service = StateSyncService(prefs, userFlow, client, languageClient, backgroundScope)
        var completedNormally = false
        val job = launch { service.reconcileCurrentCountry(); completedNormally = true }
        runCurrent()

        job.cancel()
        runCurrent()

        assertEquals(false, completedNormally)
    }

    // ── Language sync — a scalar, so no migration-flag dance, and its own
    // fetch/push via LanguageClient (HiddenFilmsClient's response never
    // carries it) ────────────────────────────────────────────────────────

    /** The account's pick is restored regardless of the per-country migration
     *  flags — this is the very first merge (flags unset), which the sets'
     *  union path shares, but language must not wait for a second login. */
    @Test
    fun loginRestoresAccountLanguage() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        startService()
        login()
        advanceUntilIdle()

        assertEquals("de", prefs.languageState.value)
    }

    /** The account has no pick yet, but this device does (set before login,
     *  e.g. while anonymous) — it gets adopted as the account's, same
     *  "migrate this device's local state up" spirit as the sets. */
    @Test
    fun loginPushesLocalExplicitLanguageWhenAccountHasNone() = runTest(UnconfinedTestDispatcher()) {
        prefs.languageState.value = "es"
        languageClient.remote = null
        startService()
        login()
        advanceUntilIdle()

        assertEquals("es", languageClient.lastPushed)
    }

    /** Neither side has an explicit pick — nothing to restore or stamp onto
     *  the account, and no push at all: unlike the sets, language reconcile
     *  makes no fixed first write. */
    @Test
    fun loginLeavesNoLanguageAloneWhenNeitherSideHasAPick() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = null
        startService()
        login()
        advanceUntilIdle()

        assertNull(prefs.languageState.value)
        assertNull(languageClient.lastPushed)
    }

    /** A pick made AFTER login (not just the merge-on-login case above)
     *  reaches the server too, through language's own debounced push. */
    @Test
    fun languagePickAfterLoginIsPushed() = runTest(UnconfinedTestDispatcher()) {
        startService()
        login()
        advanceUntilIdle() // merge completes; the post-merge baseline is dropped
        languageClient.lastPushed = null // ignore any merge-time push

        prefs.setLanguageTag("de")
        advanceTimeBy(500) // past the 400 ms debounce window
        runCurrent()

        assertEquals("de", languageClient.lastPushed)
    }

    /** A pick recreates the activity, whose onResume reconciles inside the
     *  400 ms push debounce: the fetch still returns the account's OLDER pick.
     *  It used to be written over the new one (and pushed back) — the pending
     *  pick must win and reach the server instead. Mirrors iOS. */
    @Test
    fun reconcileRightAfterAPickKeepsThePickAndPushesIt() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        val service = startService()
        login()
        advanceUntilIdle()
        assertEquals("de", prefs.languageState.value)

        prefs.setLanguageTag("es")
        service.reconcileCurrentCountry()
        advanceTimeBy(1_000)
        runCurrent()

        assertEquals("es", prefs.languageState.value)
        assertEquals("es", languageClient.remote)
        assertEquals(listOf("es"), languageClient.pushes)
    }

    /** Adopting the account's pick on a resume reconcile (another device
     *  changed it) is not a local pick: it must not be echoed back. */
    @Test
    fun adoptingTheAccountLanguageDoesNotPushItBack() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        val service = startService()
        login()
        advanceUntilIdle()

        languageClient.remote = "pl"
        service.reconcileCurrentCountry()
        advanceTimeBy(1_000)
        runCurrent()

        assertEquals("pl", prefs.languageState.value)
        assertEquals(emptyList<String>(), languageClient.pushes)
    }

    /** A push the server rejected is still pending: the next reconcile
     *  retries it rather than adopting the account's older value. */
    @Test
    fun aFailedLanguagePushIsRetriedOnTheNextReconcile() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        val service = startService()
        login()
        advanceUntilIdle()

        languageClient.shouldFailPush = true
        prefs.setLanguageTag("es")
        advanceTimeBy(500)
        runCurrent()
        assertEquals(1, languageClient.pushAttempts)

        languageClient.shouldFailPush = false
        service.reconcileCurrentCountry()

        assertEquals("es", prefs.languageState.value)
        assertEquals("es", languageClient.remote)
    }

    /** A pick whose push failed survives a relaunch: the next session's login
     *  reconcile pushes it instead of restoring the account's older value. */
    @Test
    fun aFailedLanguagePushSurvivesARelaunch() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        startService()
        login()
        advanceUntilIdle()
        languageClient.shouldFailPush = true
        prefs.setLanguageTag("es")
        advanceTimeBy(500)
        runCurrent()
        languageClient.shouldFailPush = false

        // Relaunch: the same persisted prefs, a fresh service and a restored session.
        val relaunchedUser = MutableStateFlow<UserProfile?>(null)
        StateSyncService(prefs, relaunchedUser, client, languageClient, backgroundScope).start()
        relaunchedUser.value = userFlow.value
        advanceUntilIdle()

        assertEquals("es", languageClient.remote)
        assertEquals("es", prefs.languageState.value)
    }

    /** Picking back to the account's language while another pick's push is
     *  still in flight: the pick-back matches what the account held when it
     *  was made, so nothing was sent for it, and the in-flight push then left
     *  the server on the abandoned pick. The final pick must win. Mirrors iOS. */
    /** Picking back to the account's language INSIDE the debounce — nothing
     *  has been sent yet — leaves nothing to push, as on iOS. Only a push
     *  already on the wire makes the pick-back worth sending. */
    @Test
    fun pickingBackInsideTheDebounceSendsNothing() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        startService()
        login()
        advanceUntilIdle()

        prefs.setLanguageTag("es")
        advanceTimeBy(100)
        runCurrent()
        prefs.setLanguageTag("de")
        advanceTimeBy(1_000)
        runCurrent()

        assertEquals(0, languageClient.pushAttempts)
        assertNull(prefs.pendingLanguagePush())
    }

    @Test
    fun pickingBackWhileAPushIsInFlightEndsOnTheFinalPick() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        startService()
        login()
        advanceUntilIdle()

        val gate = CompletableDeferred<Unit>()
        languageClient.beforePushResponse = { gate.await() }
        prefs.setLanguageTag("es")
        advanceTimeBy(500)
        runCurrent()
        assertEquals(1, languageClient.pushAttempts)
        prefs.setLanguageTag("de")
        runCurrent()
        languageClient.beforePushResponse = {}
        gate.complete(Unit)
        advanceTimeBy(500)
        runCurrent()

        assertEquals("de", languageClient.remote)
    }

    /** A pick made while the LOGIN reconcile's fetch is in flight: the
     *  observer used to start only after that reconcile, so the pick was never
     *  marked pending and the account's older value overwrote it. Mirrors iOS. */
    @Test
    fun aPickDuringTheLoginFetchIsKeptAndPushed() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        val gate = CompletableDeferred<Unit>()
        languageClient.beforeFetch = { gate.await() }
        startService()
        login()
        runCurrent()

        prefs.setLanguageTag("es")
        runCurrent()
        languageClient.beforeFetch = {}
        gate.complete(Unit)
        advanceTimeBy(1_000)
        runCurrent()

        assertEquals("es", prefs.languageState.value)
        assertEquals("es", languageClient.remote)
    }

    /** A genuine logout forgets the unsent pick — the next sign-in may be a
     *  different account, which must not inherit it. */
    @Test
    fun logoutForgetsAnUnsentLanguagePick() = runTest(UnconfinedTestDispatcher()) {
        languageClient.remote = "de"
        startService()
        login()
        advanceUntilIdle()
        languageClient.shouldFailPush = true
        prefs.setLanguageTag("es")
        runCurrent()
        assertEquals("es", prefs.pendingLanguagePush())

        userFlow.value = null
        advanceUntilIdle()

        assertNull(prefs.pendingLanguagePush())
    }
}
