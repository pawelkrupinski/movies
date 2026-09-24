package pl.kinowo

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.launch
import kotlinx.coroutines.test.TestScope
import kotlinx.coroutines.test.UnconfinedTestDispatcher
import kotlinx.coroutines.test.advanceTimeBy
import kotlinx.coroutines.test.advanceUntilIdle
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest
import org.junit.Assert.fail
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.auth.StateSyncService
import pl.kinowo.auth.UserProfile
import pl.kinowo.data.HiddenFilmsOp
import java.util.Random

/**
 * Model-based test of [StateSyncService]: seeded random sequences of what a
 * user, the network and the account's other devices do, checked against the
 * sync's invariants. The same alphabet and invariants run against iOS
 * (`StateSyncModelTests`) and, less the refused hide, the web
 * (`HiddenFilmsSyncModelSpec`).
 *
 * THE ALPHABET: switch country, hide, a hide the server refuses for good (a
 * title over its length bound), unhide, clear, login, logout, resume (a
 * reconcile — the server answers 304 or 200 by its own content validator),
 * another device hiding / unhiding a title, the network going down, the
 * network coming back (reconnect + resume), a local language pick, another
 * device's language pick, and a stall / release of the responses on the wire
 * (so events interleave with requests the server applied but the device
 * hasn't heard back from).
 *
 * THE INVARIANTS:
 *  1. No title crosses countries: a title hidden in one country never reaches
 *     another country's server bucket (checked after EVERY event) or local list.
 *  2. Convergence: once the network is up, signed in and every country has been
 *     reconciled, each country's local list IS its server list, and the local
 *     language IS the account's.
 *  3. No op lost: an edit made while signed in reaches the server — a hide stays
 *     hidden, an unhide/clear stays unhidden — unless something later
 *     legitimately overrides it (a later edit, another device). Edits still
 *     queued at a logout are the exception every client makes: a logout
 *     forgets what the account was owed, so their titles are unconstrained.
 *     The same holds for the last language pick made while signed in.
 *
 * A failure prints the seed, the minimised sequence and the violation; paste
 * the sequence into a regression test beside [aLanguagePickQueuedAtLogout].
 */
@OptIn(ExperimentalCoroutinesApi::class)
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class StateSyncModelTest {

    @Test
    fun randomSequencesKeepTheSyncInvariants() {
        val seeds = System.getProperty("kinowo.syncModelSeeds")?.toLongOrNull() ?: DefaultSeeds
        for (seed in 1L..seeds) {
            val events = SyncModel.generate(seed)
            val violation = SyncModel.violationOf(events) ?: continue
            val minimal = SyncModel.minimise(events)
            fail("seed=$seed violates the sync model: ${SyncModel.violationOf(minimal)}\n" +
                "minimised sequence (${minimal.size} of ${events.size} events):\n  " +
                minimal.joinToString(",\n  ") + "\nfull-sequence violation: $violation")
        }
    }

    // Pinned shapes of the historical bugs, so they run on every build whatever
    // the seeds happen to generate: a write that failed offline (the pending-ops
    // queue, ec3af46b0), and a language pick still unsent at a logout.
    @Test
    fun aFailedUnhideIsResentAfterReconnect() {
        val events = listOf(SyncEvent.Login, SyncEvent.Hide, SyncEvent.NetworkDown, SyncEvent.Unhide(0),
            SyncEvent.Reconnect)
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    @Test
    fun aLanguagePickQueuedAtLogout() {
        val events = listOf(SyncEvent.Login, SyncEvent.NetworkDown, SyncEvent.PickLanguage("de"), SyncEvent.Logout,
            SyncEvent.Reconnect)
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    /** A hide refused for good sat at the head of the queue forever, holding
     *  every later edit back and the list off the server's. */
    @Test
    fun aRefusedHideDoesNotHoldTheQueue() {
        val events = listOf(SyncEvent.Login, SyncEvent.HideRefused, SyncEvent.Hide, SyncEvent.Resume)
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    /** Seed 768: a pick back to the account's language after another pick's
     *  push FAILED was dropped as "nothing to send" — but a failed push leaves
     *  the account unknown (another device had moved it on meanwhile). */
    @Test
    fun aPickBackAfterAFailedPushIsStillSent() {
        val events = listOf(SyncEvent.RemoteLanguage("de"), SyncEvent.Login, SyncEvent.NetworkDown,
            SyncEvent.PickLanguage("es"), SyncEvent.RemoteLanguage("pl"), SyncEvent.PickLanguage("de"))
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    // Shapes the stall/release events found, pinned whatever the seeds generate.

    /** Seed 49: a pick sent while a reconcile's fetch was on the wire lost to
     *  that fetch's older answer. */
    @Test
    fun aPickSentDuringAStalledFetchSurvivesItsAnswer() {
        val events = listOf(SyncEvent.Stall, SyncEvent.Login, SyncEvent.PickLanguage("de"), SyncEvent.SwitchCountry("de"),
            SyncEvent.Logout, SyncEvent.SwitchCountry("de"), SyncEvent.Login, SyncEvent.PickLanguage("en"))
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    /** Seed 1054: a write on the wire at a logout dequeued the next session's clear. */
    @Test
    fun aStalledWriteAcrossALogoutKeepsTheNextSessionsClear() {
        val events = listOf(SyncEvent.Login, SyncEvent.Stall, SyncEvent.Hide, SyncEvent.Logout, SyncEvent.Login,
            SyncEvent.Clear)
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    /** Seed 1532: a repeated pick made while its twin was on the wire was taken
     *  as already sent. */
    @Test
    fun aRepeatedPickDuringAStalledPushIsSentAgain() {
        val events = listOf(SyncEvent.Stall, SyncEvent.Login, SyncEvent.PickLanguage("pl"), SyncEvent.PickLanguage("es"),
            SyncEvent.RemoteLanguage("en"), SyncEvent.PickLanguage("pl"))
        SyncModel.violationOf(events)?.let { fail(it) }
    }

    private companion object {
        /** A push-sized run; the nightly one passes `-PsyncModelSeeds`. */
        const val DefaultSeeds = 200L
    }
}

/** One step of a generated sequence. Picks are indices resolved against the
 *  state at the moment the event runs, so a shrunk sequence stays runnable. */
sealed interface SyncEvent {
    data class SwitchCountry(val country: String) : SyncEvent
    data object Hide : SyncEvent
    /** A hide the server refuses for good: a title over its length bound. */
    data object HideRefused : SyncEvent
    data class Unhide(val pick: Int) : SyncEvent
    data object Clear : SyncEvent
    data object Login : SyncEvent
    data object Logout : SyncEvent
    data object Resume : SyncEvent
    data class RemoteHide(val country: String) : SyncEvent
    data class RemoteUnhide(val country: String, val pick: Int) : SyncEvent
    data object NetworkDown : SyncEvent
    data object Reconnect : SyncEvent
    data class PickLanguage(val language: String) : SyncEvent
    data class RemoteLanguage(val language: String) : SyncEvent
    /** From now on every response is held on the wire — the server has
     *  applied the request, the device hasn't heard back — so the events
     *  after it interleave with requests in flight. */
    data object Stall : SyncEvent
    /** Every held response arrives. */
    data object Release : SyncEvent
}

@OptIn(ExperimentalCoroutinesApi::class)
object SyncModel {
    /** Two countries that share one web origin in production (showtimes.cc). */
    val Countries = listOf("uk", "de")
    private val Languages = listOf("en", "de", "pl", "es")
    private const val Length = 30

    fun generate(seed: Long): List<SyncEvent> {
        val random = Random(seed)
        fun country() = Countries[random.nextInt(Countries.size)]
        return List(Length) {
            when (random.nextInt(108)) {
                in 0..17 -> SyncEvent.Hide
                in 18..19 -> SyncEvent.HideRefused
                in 20..31 -> SyncEvent.Unhide(random.nextInt(8))
                in 32..35 -> SyncEvent.Clear
                in 36..45 -> SyncEvent.SwitchCountry(country())
                in 46..52 -> SyncEvent.Login
                in 53..57 -> SyncEvent.Logout
                in 58..65 -> SyncEvent.Resume
                in 66..70 -> SyncEvent.RemoteHide(country())
                in 71..73 -> SyncEvent.RemoteUnhide(country(), random.nextInt(8))
                in 74..80 -> SyncEvent.NetworkDown
                in 81..88 -> SyncEvent.Reconnect
                in 89..95 -> SyncEvent.PickLanguage(Languages[random.nextInt(Languages.size)])
                in 96..99 -> SyncEvent.RemoteLanguage(Languages[random.nextInt(Languages.size)])
                in 100..103 -> SyncEvent.Stall
                else -> SyncEvent.Release
            }
        }
    }

    /** Greedily drop events while the sequence still violates the model. */
    fun minimise(events: List<SyncEvent>): List<SyncEvent> {
        var current = events
        var shrunk = true
        while (shrunk) {
            shrunk = false
            for (i in current.indices.reversed()) {
                val candidate = current.filterIndexed { index, _ -> index != i }
                if (violationOf(candidate) != null) { current = candidate; shrunk = true; break }
            }
        }
        return current
    }

    /** Run [events] and then settle; the first invariant broken, or null. */
    fun violationOf(events: List<SyncEvent>): String? {
        var violation: String? = null
        runTest(UnconfinedTestDispatcher()) { violation = Run(this).play(events) }
        return violation
    }

    private class Run(private val test: TestScope) {
        private val prefs = FakeSyncPrefs().also { it.countryState.value = Countries.first() }
        private val client = FakeHiddenFilmsClient()
        private val languageClient = FakeLanguageClient()
        private val user = MutableStateFlow<UserProfile?>(null)

        /** Completed at the next [SyncEvent.Release]; every response waits on
         *  it while set. */
        private var stall: CompletableDeferred<Unit>? = null
        /** Language requests held by [stall] right now. */
        private var languageHeldCount = 0

        init {
            session(signedIn = false)
            val held: suspend () -> Unit = { stall?.await() }
            val languageHeld: suspend () -> Unit = {
                stall?.let { gate -> languageHeldCount++; try { gate.await() } finally { languageHeldCount-- } }
            }
            client.beforeFetchResponse = held
            client.beforeWriteResponse = held
            languageClient.beforeFetchResponse = languageHeld
            languageClient.beforePushResponse = languageHeld
        }
        private var serviceScope: CoroutineScope? = null
        private lateinit var service: StateSyncService

        private var signedIn = false
        private var minted = 0
        private val mustHave = Countries.associateWith { mutableSetOf<String>() }
        private val mustNotHave = Countries.associateWith { mutableSetOf<String>() }
        private var expectedLanguage: String? = null

        private val country: String get() = prefs.countryState.value ?: Countries.first()

        /** The app rebuilds the service (a new ViewModel graph) on a country
         *  switch, cancelling whatever the old one had in flight. */
        private fun rebuild() {
            serviceScope?.cancel()
            val scope = CoroutineScope(test.backgroundScope.coroutineContext + Job(test.backgroundScope.coroutineContext[Job]))
            serviceScope = scope
            service = StateSyncService(prefs, user, client, languageClient, scope).also { it.start() }
        }

        suspend fun play(events: List<SyncEvent>): String? {
            rebuild()
            events.forEachIndexed { index, event ->
                apply(event)
                quiesce()
                isolationViolation()?.let { return "after event #$index $event: $it" }
            }
            settle()
            return isolationViolation() ?: convergenceViolation()
        }

        private suspend fun apply(event: SyncEvent) {
            when (event) {
                is SyncEvent.SwitchCountry -> { prefs.countryState.value = event.country; rebuild() }
                SyncEvent.Hide -> {
                    val title = "$country-${++minted}"
                    prefs.setHiddenFilms(country, prefs.hiddenFilmsFor(country) + title)
                    service.hide(title)
                    if (signedIn) expect(country, title, hidden = true)
                }
                SyncEvent.HideRefused -> {
                    // The server never stores it, so it must end up nowhere.
                    val title = "$country-${++minted}-" + "x".repeat(FakeHiddenFilmsClient.MaxTitleLength)
                    prefs.setHiddenFilms(country, prefs.hiddenFilmsFor(country) + title)
                    service.hide(title)
                    expect(country, title, hidden = false)
                }
                is SyncEvent.Unhide -> {
                    val local = prefs.hiddenFilmsFor(country).sorted()
                    if (local.isEmpty()) return
                    val title = local[event.pick % local.size]
                    prefs.setHiddenFilms(country, local.toSet() - title)
                    service.unhide(title)
                    if (signedIn) expect(country, title, hidden = false)
                }
                SyncEvent.Clear -> {
                    val local = prefs.hiddenFilmsFor(country)
                    prefs.setHiddenFilms(country, emptySet())
                    service.clear()
                    if (signedIn) {
                        mustNotHave.getValue(country) += local + mustHave.getValue(country)
                        mustHave.getValue(country).clear()
                    }
                }
                SyncEvent.Login -> {
                    session(signedIn = true)
                    user.value = UserProfile(displayName = "Model", email = "model@example.com", provider = "google")
                    signedIn = true
                }
                SyncEvent.Logout -> {
                    // What the account is still owed is forgotten with the session.
                    if (signedIn) Countries.forEach { c ->
                        prefs.pendingHiddenFilmsOps(c).forEach { op ->
                            when (op) {
                                is HiddenFilmsOp.Hide -> unconstrain(c, op.title)
                                is HiddenFilmsOp.Unhide -> unconstrain(c, op.title)
                                HiddenFilmsOp.Clear -> { mustHave.getValue(c).clear(); mustNotHave.getValue(c).clear() }
                            }
                        }
                    }
                    if (signedIn && prefs.pendingLanguagePush() != null) expectedLanguage = null
                    session(signedIn = false)
                    user.value = null
                    signedIn = false
                }
                SyncEvent.Resume -> resume()
                is SyncEvent.RemoteHide -> {
                    val title = "${event.country}-r${++minted}"
                    client.remote[event.country] = (client.remote[event.country] ?: emptySet()) + title
                    // A clear this device still owes the account lands after it,
                    // and a clear empties the bucket: last writer wins.
                    if (HiddenFilmsOp.Clear !in prefs.pendingHiddenFilmsOps(event.country)) expect(event.country, title, hidden = true)
                }
                is SyncEvent.RemoteUnhide -> {
                    val remote = (client.remote[event.country] ?: emptySet()).sorted()
                    if (remote.isEmpty()) return
                    val title = remote[event.pick % remote.size]
                    client.remote[event.country] = remote.toSet() - title
                    // A first sync's union may legitimately bring it back.
                    unconstrain(event.country, title)
                }
                SyncEvent.Stall -> if (stall == null) stall = CompletableDeferred()
                SyncEvent.Release -> release()
                SyncEvent.NetworkDown -> network(up = false)
                SyncEvent.Reconnect -> { network(up = true); resume() }
                is SyncEvent.PickLanguage -> {
                    // Picking the language already on screen changes nothing —
                    // a picker only reports a CHANGE.
                    if (prefs.languageState.value == event.language) return
                    prefs.setLanguageTag(event.language)
                    // Signed out, the account's own pick wins at the next login.
                    expectedLanguage = if (signedIn) event.language else null
                }
                is SyncEvent.RemoteLanguage -> {
                    languageClient.remote = event.language
                    // A pick this device still owes the account is newer; and
                    // one this device's request already on the wire races —
                    // last writer wins, there is no conditional PUT.
                    expectedLanguage =
                        if (prefs.pendingLanguagePush() != null || languageHeldCount > 0) null else event.language
                }
            }
        }

        private fun resume() {
            serviceScope!!.launch { service.reconcileCurrentCountry() }
        }

        /** Let everything the event started run out — past the service's
         *  400 ms language debounce too: it runs in the background scope,
         *  whose delays `advanceUntilIdle` alone never advances. */
        private fun quiesce() {
            test.advanceUntilIdle()
            test.advanceTimeBy(1_000)
            test.runCurrent()
            test.advanceUntilIdle()
        }

        /** What the server makes of this device's session cookie. */
        private fun session(signedIn: Boolean) {
            client.signedIn = signedIn
            languageClient.signedIn = signedIn
        }

        private fun network(up: Boolean) {
            client.shouldFailFetch = !up
            client.shouldFailWrite = !up
            languageClient.shouldFailFetch = !up
            languageClient.shouldFailPush = !up
        }

        private fun expect(country: String, title: String, hidden: Boolean) {
            if (hidden) { mustHave.getValue(country) += title; mustNotHave.getValue(country) -= title }
            else { mustNotHave.getValue(country) += title; mustHave.getValue(country) -= title }
        }

        private fun unconstrain(country: String, title: String) {
            mustHave.getValue(country) -= title
            mustNotHave.getValue(country) -= title
        }

        /** Network up, signed in, every country reconciled — twice, so a first
         *  sync's union pushes have landed before anything is compared. */
        private fun release() {
            stall?.complete(Unit)
            stall = null
        }

        private suspend fun settle() {
            release()
            network(up = true)
            if (!signedIn) apply(SyncEvent.Login)
            quiesce()
            repeat(2) {
                Countries.forEach { c -> apply(SyncEvent.SwitchCountry(c)); quiesce() }
            }
        }

        private suspend fun isolationViolation(): String? {
            Countries.forEach { c ->
                val foreignRemote = (client.remote[c] ?: emptySet()).filterNot { it.startsWith("$c-") }
                if (foreignRemote.isNotEmpty()) return "server bucket '$c' holds another country's titles $foreignRemote"
                val foreignLocal = prefs.hiddenFilmsFor(c).filterNot { it.startsWith("$c-") }
                if (foreignLocal.isNotEmpty()) return "local list '$c' holds another country's titles $foreignLocal"
            }
            return null
        }

        private suspend fun convergenceViolation(): String? {
            Countries.forEach { c ->
                val local = prefs.hiddenFilmsFor(c)
                val remote = client.remote[c] ?: emptySet()
                if (local != remote) return "after settling, '$c' local $local != server $remote"
                val lost = mustHave.getValue(c) - remote
                if (lost.isNotEmpty()) return "after settling, '$c' lost hides $lost (server $remote)"
                val resurrected = mustNotHave.getValue(c).intersect(remote)
                if (resurrected.isNotEmpty()) return "after settling, '$c' resurrected unhidden $resurrected"
            }
            val local = prefs.languageState.value
            if (languageClient.remote != null && local != languageClient.remote)
                return "after settling, local language $local != account's ${languageClient.remote}"
            if (expectedLanguage != null && languageClient.remote != expectedLanguage)
                return "after settling, the account's language is ${languageClient.remote}, expected the last pick $expectedLanguage"
            return null
        }
    }
}
