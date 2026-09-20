package pl.kinowo

import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.MutableStateFlow
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
import pl.kinowo.auth.UserStateClient
import pl.kinowo.auth.UserSyncState
import pl.kinowo.data.SyncPrefs
import java.io.IOException

/**
 * Mirrors iOS `StateSyncServiceTests`: merge-on-login, push, and the
 * offline-preserves-local guarantee, against an in-memory prefs + a fake
 * state client.
 */
@OptIn(ExperimentalCoroutinesApi::class)
class StateSyncServiceTest {

    private lateinit var prefs: FakeSyncPrefs
    private lateinit var client: FakeUserStateClient
    private lateinit var userFlow: MutableStateFlow<UserProfile?>

    @Before
    fun setUp() {
        prefs = FakeSyncPrefs()
        client = FakeUserStateClient()
        userFlow = MutableStateFlow(null)
    }

    private fun TestScope.startService(): StateSyncService =
        StateSyncService(prefs, userFlow, client, backgroundScope).also { it.start() }

    private fun login() {
        userFlow.value = UserProfile(displayName = "Test", email = "test@test.com", provider = "google")
    }

    @Test
    fun loginSyncsRemoteHiddenIntoEmptyLocal() = runTest(UnconfinedTestDispatcher()) {
        client.remoteState = UserSyncState(setOf("Film A", "Film B"), emptySet())
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Film A", "Film B"), prefs.hiddenState.value)
        assertEquals(setOf("Film A", "Film B"), client.lastPushed?.hiddenFilms)
    }

    @Test
    fun loginMergesLocalAndRemoteHidden() = runTest(UnconfinedTestDispatcher()) {
        prefs.hiddenState.value = setOf("Local Only")
        client.remoteState = UserSyncState(setOf("Remote Only"), emptySet())
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Local Only", "Remote Only"), prefs.hiddenState.value)
        assertEquals(setOf("Local Only", "Remote Only"), client.lastPushed?.hiddenFilms)
    }

    @Test
    fun loginSyncsDisabledCinemas() = runTest(UnconfinedTestDispatcher()) {
        client.remoteState = UserSyncState(emptySet(), setOf("Cinema X"))
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Cinema X"), prefs.disabledState.value)
    }

    @Test
    fun loginPushesMergedStateToServer() = runTest(UnconfinedTestDispatcher()) {
        prefs.hiddenState.value = setOf("Already Hidden")
        prefs.disabledState.value = setOf("Local Cinema")
        client.remoteState = UserSyncState(setOf("From Server"), setOf("Remote Cinema"))
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Already Hidden", "From Server"), client.lastPushed?.hiddenFilms)
        assertEquals(setOf("Local Cinema", "Remote Cinema"), client.lastPushed?.disabledCinemas)
    }

    @Test
    fun noSyncWhenNotLoggedIn() = runTest(UnconfinedTestDispatcher()) {
        client.remoteState = UserSyncState(setOf("Film A"), emptySet())
        startService()
        advanceUntilIdle()

        assertTrue(prefs.hiddenState.value.isEmpty())
        assertNull(client.lastPushed)
    }

    @Test
    fun fetchFailurePreservesLocalState() = runTest(UnconfinedTestDispatcher()) {
        prefs.hiddenState.value = setOf("My Film")
        client.shouldFailFetch = true
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("My Film"), prefs.hiddenState.value)
        assertNull(client.lastPushed)
    }

    /** Regression: once migration has run, a later launch must MIRROR the
     *  server, not union. A film removed on another device (server now empty)
     *  must not be resurrected from this device's stale local copy. The old
     *  union-on-every-login made removals impossible. */
    @Test
    fun serverAuthoritativeAfterFirstSyncDropsStaleLocal() = runTest(UnconfinedTestDispatcher()) {
        // Launch 1: migrate from server = {Film A}, flag flips on.
        client.remoteState = UserSyncState(setOf("Film A"), emptySet())
        startService()
        login()
        advanceUntilIdle()
        assertEquals(setOf("Film A"), prefs.hiddenState.value)
        assertTrue(prefs.isServerStateSynced())

        // Another device removes Film A from the account.
        client.remoteState = UserSyncState(emptySet(), emptySet())

        // Launch 2: same persisted prefs (flag still set), fresh session restore
        // (flow starts null, then the user) — the initial null must NOT clear
        // the flag, and the now-empty server must win.
        val userFlow2 = MutableStateFlow<UserProfile?>(null)
        StateSyncService(prefs, userFlow2, client, backgroundScope).also { it.start() }
        userFlow2.value = UserProfile(displayName = "Test", email = "test@test.com", provider = "google")
        advanceUntilIdle()

        assertEquals(emptySet<String>(), prefs.hiddenState.value)
    }

    /** A genuine logout re-arms migration so the next sign-in carries this
     *  device's current local picks up again. */
    @Test
    fun logoutReArmsMigration() = runTest(UnconfinedTestDispatcher()) {
        client.remoteState = UserSyncState(setOf("Film A"), emptySet())
        startService()
        login()
        advanceUntilIdle()
        assertTrue(prefs.isServerStateSynced())

        userFlow.value = null // logout
        advanceUntilIdle()
        assertTrue(!prefs.isServerStateSynced())
    }

    @Test
    fun localChangeAfterLoginIsPushed() = runTest(UnconfinedTestDispatcher()) {
        startService()
        login()
        advanceUntilIdle() // merge completes; the post-merge baseline is dropped
        client.lastPushed = null // ignore the merge-time push

        prefs.setDisabledCinemas(setOf("Helios"))
        advanceTimeBy(500) // past the 400 ms debounce window
        runCurrent()

        assertEquals(setOf("Helios"), client.lastPushed?.disabledCinemas)
    }

    // ── Language sync — a scalar, so no migration-flag dance ────────────────

    /** The account's pick is restored regardless of [SyncPrefs.isServerStateSynced]
     *  — this is the very first merge (flag unset), which the sets' union path
     *  shares, but language must not wait for a second login. */
    @Test
    fun loginRestoresAccountLanguage() = runTest(UnconfinedTestDispatcher()) {
        client.remoteState = UserSyncState(emptySet(), emptySet(), language = "de")
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
        client.remoteState = UserSyncState(emptySet(), emptySet(), language = null)
        startService()
        login()
        advanceUntilIdle()

        assertEquals("es", client.lastPushed?.language)
    }

    /** Neither side has an explicit pick — nothing to restore or stamp onto
     *  the account. The FIRST login's own hiddenFilms/disabledCinemas
     *  migration still pushes once regardless (pre-existing, unrelated to
     *  language), so this only checks that push's `language` came along
     *  empty, not that no push happened at all. */
    @Test
    fun loginLeavesNoLanguageAloneWhenNeitherSideHasAPick() = runTest(UnconfinedTestDispatcher()) {
        client.remoteState = UserSyncState(emptySet(), emptySet(), language = null)
        startService()
        login()
        advanceUntilIdle()

        assertNull(prefs.languageState.value)
        assertNull(client.lastPushed?.language)
    }

    /** A pick made AFTER login (not just the merge-on-login case above)
     *  reaches the server too, through the same debounced push the sets use. */
    @Test
    fun languagePickAfterLoginIsPushed() = runTest(UnconfinedTestDispatcher()) {
        startService()
        login()
        advanceUntilIdle() // merge completes; the post-merge baseline is dropped
        client.lastPushed = null // ignore the merge-time push

        prefs.setLanguageTag("de")
        advanceTimeBy(500) // past the 400 ms debounce window
        runCurrent()

        assertEquals("de", client.lastPushed?.language)
    }
}

private class FakeSyncPrefs : SyncPrefs {
    val hiddenState = MutableStateFlow<Set<String>>(emptySet())
    val disabledState = MutableStateFlow<Set<String>>(emptySet())
    val languageState = MutableStateFlow<String?>(null)
    private var synced = false
    override val hiddenFilms = hiddenState
    override val disabledCinemas = disabledState
    override suspend fun setHiddenFilms(films: Set<String>) { hiddenState.value = films }
    override suspend fun setDisabledCinemas(cinemas: Set<String>) { disabledState.value = cinemas }
    override suspend fun isServerStateSynced(): Boolean = synced
    override suspend fun setServerStateSynced(synced: Boolean) { this.synced = synced }
    override val selectedLanguageTag = languageState
    override suspend fun setLanguageTag(tag: String) { languageState.value = tag }
}

private class FakeUserStateClient : UserStateClient {
    var remoteState = UserSyncState(emptySet(), emptySet())
    var lastPushed: UserSyncState? = null
    var shouldFailFetch = false

    override suspend fun fetchState(): UserSyncState {
        if (shouldFailFetch) throw IOException("no network")
        return remoteState
    }

    override suspend fun putState(state: UserSyncState) {
        lastPushed = state
    }
}
