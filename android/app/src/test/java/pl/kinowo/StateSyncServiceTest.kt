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
import pl.kinowo.auth.HiddenFilmsClient
import pl.kinowo.auth.HiddenFilmsFetchResult
import pl.kinowo.auth.HiddenFilmsState
import pl.kinowo.auth.LanguageClient
import pl.kinowo.auth.StateSyncService
import pl.kinowo.auth.UserProfile
import pl.kinowo.data.SyncPrefs
import java.io.IOException

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

        assertEquals(setOf("Film A", "Film B"), prefs.hiddenState.value)
    }

    @Test
    fun loginMergesLocalAndRemoteHiddenPushingOnlyTheLocalOnlyTitles() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        prefs.hiddenState.value = setOf("Local Only")
        client.remote["pl"] = setOf("Remote Only")
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Local Only", "Remote Only"), prefs.hiddenState.value)
        // Only the title the server didn't already have was pushed — there is
        // no bulk write, so the diff must be exact, not "everything local".
        assertEquals(listOf("Local Only" to "pl"), client.hideCalls)
    }

    @Test
    fun mergeNeverTouchesDisabledCinemas() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        prefs.disabledState.value = setOf("Local Only Cinema")
        client.remote["pl"] = setOf("Film A")
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("Local Only Cinema"), prefs.disabledState.value)
    }

    @Test
    fun noSyncWhenNotLoggedIn() = runTest(UnconfinedTestDispatcher()) {
        prefs.countryState.value = "pl"
        client.remote["pl"] = setOf("Film A")
        startService()
        advanceUntilIdle()

        assertTrue(prefs.hiddenState.value.isEmpty())
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

        assertEquals(setOf("Film A"), prefs.hiddenState.value)
        assertEquals(listOf("pl"), client.fetchCalls)
    }

    @Test
    fun aLegacyUppercaseCountryCodeSyncsUnderItsCurrentServerCode() = runTest(UnconfinedTestDispatcher()) {
        // Earlier builds persisted ISO codes; the per-country API and the
        // migration-flag keys use the server's code space.
        prefs.countryState.value = "GB"
        prefs.hiddenState.value = setOf("Local Only")
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
        prefs.hiddenState.value = setOf("My Film")
        client.shouldFailFetch = true
        startService()
        login()
        advanceUntilIdle()

        assertEquals(setOf("My Film"), prefs.hiddenState.value)
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
        assertEquals(setOf("Film A"), prefs.hiddenState.value)
        assertTrue(prefs.isHiddenFilmsMigrated("pl"))

        // Another device removes Film A from the account.
        client.remote["pl"] = emptySet()

        val userFlow2 = MutableStateFlow<UserProfile?>(null)
        StateSyncService(prefs, userFlow2, client, languageClient, backgroundScope).also { it.start() }
        userFlow2.value = UserProfile(displayName = "Test", email = "test@test.com", provider = "google")
        advanceUntilIdle()

        assertEquals(emptySet<String>(), prefs.hiddenState.value)
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
        assertEquals(setOf("Film A"), prefs.hiddenState.value)

        client.remote["pl"] = setOf("Film A", "Film B") // changed elsewhere while backgrounded
        service.reconcileCurrentCountry()
        advanceUntilIdle()

        assertEquals(setOf("Film A", "Film B"), prefs.hiddenState.value)
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
}

private class FakeSyncPrefs : SyncPrefs {
    val hiddenState = MutableStateFlow<Set<String>>(emptySet())
    val disabledState = MutableStateFlow<Set<String>>(emptySet())
    val countryState = MutableStateFlow<String?>(null)
    val languageState = MutableStateFlow<String?>(null)
    private val migrated = mutableSetOf<String>()
    private val etags = mutableMapOf<String, String?>()
    private val lastModifieds = mutableMapOf<String, String?>()

    override val hiddenFilms = hiddenState
    override val disabledCinemas = disabledState
    override val selectedCountryCode = countryState

    override suspend fun setHiddenFilms(films: Set<String>) { hiddenState.value = films }
    override suspend fun setDisabledCinemas(cinemas: Set<String>) { disabledState.value = cinemas }

    override suspend fun isHiddenFilmsMigrated(country: String): Boolean = country in migrated
    override suspend fun setHiddenFilmsMigrated(country: String, migrated: Boolean) {
        if (migrated) this.migrated.add(country) else this.migrated.remove(country)
    }

    override suspend fun hiddenFilmsEtag(country: String): String? = etags[country]
    override suspend fun hiddenFilmsLastModified(country: String): String? = lastModifieds[country]
    override suspend fun setHiddenFilmsValidators(country: String, etag: String?, lastModified: String?) {
        etags[country] = etag
        lastModifieds[country] = lastModified
    }

    override suspend fun clearHiddenFilmsSyncState() {
        migrated.clear()
        etags.clear()
        lastModifieds.clear()
    }

    override val selectedLanguageTag = languageState
    override suspend fun setLanguageTag(tag: String) { languageState.value = tag }
}

private class FakeHiddenFilmsClient : HiddenFilmsClient {
    val remote = mutableMapOf<String, Set<String>>()
    val hideCalls = mutableListOf<Pair<String, String>>() // (title, country)
    val unhideCalls = mutableListOf<Pair<String, String>>()
    val clearCalls = mutableListOf<String>()
    val fetchCalls = mutableListOf<String>()
    var shouldFailFetch = false

    override suspend fun fetch(country: String, etag: String?, lastModified: String?): HiddenFilmsFetchResult {
        fetchCalls += country
        if (shouldFailFetch) throw IOException("no network")
        return HiddenFilmsFetchResult.Changed(HiddenFilmsState(remote[country] ?: emptySet(), "\"etag-$country\"", "lm-$country"))
    }

    override suspend fun hide(country: String, title: String): HiddenFilmsState {
        hideCalls += title to country
        remote[country] = (remote[country] ?: emptySet()) + title
        return HiddenFilmsState(remote[country]!!, "\"etag-$country\"", "lm-$country")
    }

    override suspend fun unhide(country: String, title: String): HiddenFilmsState {
        unhideCalls += title to country
        remote[country] = (remote[country] ?: emptySet()) - title
        return HiddenFilmsState(remote[country] ?: emptySet(), "\"etag-$country\"", "lm-$country")
    }

    override suspend fun clear(country: String): HiddenFilmsState {
        clearCalls += country
        remote[country] = emptySet()
        return HiddenFilmsState(emptySet(), "\"etag-$country\"", "lm-$country")
    }
}

private class FakeLanguageClient : LanguageClient {
    var remote: String? = null
    var lastPushed: String? = null

    override suspend fun fetch(): String? = remote
    override suspend fun push(language: String) { lastPushed = language }
}
