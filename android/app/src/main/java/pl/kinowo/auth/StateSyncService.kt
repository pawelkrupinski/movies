package pl.kinowo.auth

import pl.kinowo.runCatchingCancellable
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.CoroutineStart
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import pl.kinowo.data.HiddenFilmsOp
import pl.kinowo.data.SyncPrefs
import pl.kinowo.model.Country

/**
 * Keeps per-device [SyncPrefs] hiddenFilms — and the account's language pick —
 * in step with the server while signed in — the Android counterpart of iOS
 * `StateSyncService`. disabledCinemas is device-local and never touched here.
 *
 * hiddenFilms is per country on both sides (`/api/me/{country}/hidden-films`
 * — a title isn't globally unique across countries the way a cinema display
 * name is), and every read/write here names the country it fetched for, so a
 * reconcile that lands after a country switch can't touch the new country's
 * set. Migration/authority is tracked per country too: the FIRST
 * reconcile for a given country unions local + remote (so nothing set while
 * signed-out, or set for a country never reconciled on this device, is
 * lost), pushes each LOCAL-ONLY title with its own `hide` call (there is no
 * bulk write any more), and marks that country migrated. EVERY reconcile
 * after that treats the SERVER as authoritative for that country — local is
 * replaced with the server's set, so a hide removed elsewhere stays removed
 * instead of being resurrected by a blind union. `reconcile`'s fetch is
 * conditional (stored `ETag`/`Last-Modified` per country), so a reconcile
 * that finds nothing changed costs one round trip with no body.
 *
 * Reconcile runs on login, on app foreground-resume (see
 * [reconcileCurrentCountry], wired from [pl.kinowo.ui.KinowoViewModel.onResume])
 * and implicitly on a country switch (switching country persists a new code
 * and recreates the Activity — see [pl.kinowo.ui.KinowoViewModel.setCountry] —
 * which reconstructs this whole object and reruns [onLogin]/[start] against
 * the new country).
 *
 * Local hide/unhide push IMMEDIATELY now (queued first — see below) — see [hide]/[unhide]/[clear] — no
 * debounce: the old 400 ms debounce existed to batch several toggles into one
 * bulk PUT body, and there is no bulk write left to batch into.
 *
 * A failed pull leaves local prefs authoritative (no overwrite) — exactly the
 * offline behaviour iOS has. Each write is queued (persisted) before it is
 * sent and dequeued once the server accepts it, so one that failed — offline,
 * or the process killed mid-request — is re-sent by the next reconcile
 * (login, resume) before it fetches.
 *
 * The language pick rides the LEGACY `/api/me/state` document instead (via
 * [LanguageClient] — there's no granular endpoint for a single scalar), and
 * is NOT gated by the hiddenFilms per-country migration flags — see
 * [reconcileLanguage] for why a scalar pick doesn't need the two-phase dance
 * the sets do. It reconciles at the SAME triggers as hiddenFilms (login,
 * resume, country switch) but as its own independent step, and pushes a
 * local change on its own debounce — see [observeLanguage] — never echoing
 * back a value it just adopted from the server, and never letting the
 * account's older value overwrite a pick it hasn't confirmed yet.
 */
class StateSyncService(
    private val prefs: SyncPrefs,
    private val user: StateFlow<UserProfile?>,
    private val client: HiddenFilmsClient,
    private val languageClient: LanguageClient,
    private val scope: CoroutineScope,
) {
    @Volatile private var loggedIn = false
    /** Bumped by every genuine logout, which forgets the hiddenFilms queue: a
     *  response from before it must not touch the next session's queue. */
    @Volatile private var session = 0
    private var syncJob: Job? = null
    /** The 400 ms wait before a local pick is sent — only ever the wait: the
     *  send runs outside it, so cancelling a debounce never abandons a push
     *  that may already have reached the server. */
    private var languageDebounceJob: Job? = null
    /** One language push on the wire at a time — see [sendPendingLanguage]. */
    private val languageSendMutex = Mutex()
    /** Counts the language pushes the server confirmed, so a reconcile can
     *  tell that one landed while its fetch was on the wire. */
    @Volatile private var languagePushesConfirmed = 0
    /** Serialises every read-modify-write of the persisted hiddenFilms queue. */
    private val queueMutex = Mutex()
    /** Serialises the flushes, so no queued edit is sent twice. Never held
     *  while enqueueing — an edit is persisted even while another is on the wire. */
    private val flushMutex = Mutex()
    /** The language the account is known to hold — last fetched or
     *  successfully pushed; null when unknown (a push failed). A local change
     *  to this value merely adopted the server's pick, so it is never pushed back. */
    @Volatile private var accountLanguage: String? = null
    /** Whether a language push is on the wire (not merely debouncing). */
    @Volatile private var languageSendInFlight = false

    /** Begin observing the auth state. Idempotent enough for a single call
     *  from the composition root. */
    fun start() {
        scope.launch {
            user.collect { profile ->
                if (profile != null) {
                    loggedIn = true
                    onLogin()
                } else {
                    // Clear every country's migration flag only on a GENUINE
                    // logout, not the initial null the flow holds before a
                    // session restores — otherwise every cold start would
                    // re-run the first-login union instead of treating the
                    // server as authoritative.
                    // The same goes for an unsent language pick: it is
                    // persisted precisely so the session restore after a
                    // relaunch can still push it.
                    if (loggedIn) {
                        session++
                        prefs.clearHiddenFilmsSyncState()
                        prefs.setPendingLanguagePush(null)
                    }
                    loggedIn = false
                    syncJob?.cancel()
                    syncJob = null
                    languageDebounceJob?.cancel()
                    languageDebounceJob = null
                    accountLanguage = null
                }
            }
        }
    }

    private fun onLogin() {
        syncJob?.cancel()
        syncJob = scope.launch {
            // Observe local picks BEFORE the login reconcile (UNDISPATCHED, so
            // it has subscribed by the time the reconcile starts): a pick made
            // while its fetch is in flight must already be pending, or the
            // account's older value would overwrite it. Mirrors iOS.
            launch(start = CoroutineStart.UNDISPATCHED) { observeLanguage() }
            reconcileCurrentCountry() // also reconciles language — see its doc
        }
    }

    /** Reconcile whichever country is currently selected, PLUS language
     *  (which needs no country at all — see [reconcileLanguage]). Called on
     *  login and on app foreground-resume. Public so
     *  [pl.kinowo.ui.KinowoViewModel] can call it from its `onResume()`. */
    suspend fun reconcileCurrentCountry() {
        reconcileLanguage()
        reconcile(currentCountry())
    }

    /** The country whose deployment the app is browsing — the same resolution
     *  MainActivity applies to pick the API base URL: a never-picked (null)
     *  code is the default country, and a legacy ISO code (`GB`) maps onto the
     *  server's (`uk`). Using the raw pref instead skipped sync entirely for
     *  anyone who picked their city by hand, and keyed legacy installs under a
     *  country code the per-country API doesn't know. */
    private suspend fun currentCountry(): String = Country.byCode(prefs.selectedCountryCode.first()).code

    private suspend fun reconcile(country: String) {
        // Unsent local edits first: until the server has them, its set is
        // older than local and must not replace it.
        if (!sendPendingOps(country)) return
        val localBeforeFetch = prefs.hiddenFilmsFor(country)
        // A network error leaves local state authoritative: prefs + flags untouched.
        runCatchingCancellable {
            if (prefs.isHiddenFilmsMigrated(country)) {
                when (val result = client.fetch(country, prefs.hiddenFilmsEtag(country), prefs.hiddenFilmsLastModified(country))) {
                    is HiddenFilmsFetchResult.NotModified -> Unit
                    is HiddenFilmsFetchResult.Changed -> {
                        if (editedDuringFetch(country, localBeforeFetch)) return@runCatchingCancellable
                        val state = result.state
                        if (state.hiddenFilms != prefs.hiddenFilmsFor(country)) prefs.setHiddenFilms(country, state.hiddenFilms)
                        prefs.setHiddenFilmsValidators(country, state.etag, state.lastModified)
                    }
                }
            } else {
                val remote = (client.fetch(country, null, null) as? HiddenFilmsFetchResult.Changed)?.state
                if (editedDuringFetch(country, localBeforeFetch)) return@runCatchingCancellable
                val local = prefs.hiddenFilmsFor(country)
                val merged = local + (remote?.hiddenFilms ?: emptySet())
                if (merged != local) prefs.setHiddenFilms(country, merged)

                var latest = remote
                (local - (remote?.hiddenFilms ?: emptySet())).forEach { title -> latest = client.hide(country, title) }
                latest?.let { prefs.setHiddenFilmsValidators(country, it.etag, it.lastModified) }
                prefs.setHiddenFilmsMigrated(country, true)
            }
        }
    }

    /** Whether the user edited [country]'s set while its fetch was on the wire
     *  (the local set moved, or an edit is still queued). The response then
     *  predates that edit, so applying it would drop the edit locally; it is
     *  ignored instead — the edit's own write brings the server level, and
     *  the next reconcile (not yet migrated: the union) runs against both. */
    private suspend fun editedDuringFetch(country: String, localBeforeFetch: Set<String>): Boolean =
        prefs.hiddenFilmsFor(country) != localBeforeFetch || prefs.pendingHiddenFilmsOps(country).isNotEmpty()

    /** Language is a scalar, not a set, so it skips the per-country
     *  migration-flag dance [reconcile] needs entirely — there's no "removed
     *  on another device" case a blind overwrite could wrongly resurrect, so
     *  every reconcile (first or not) uses the same rule: the ACCOUNT's
     *  explicit pick wins whenever it has one (restored on login, per spec);
     *  otherwise this device's own explicit pick, if any, becomes the
     *  account's. A separate fetch from [reconcile]'s — [HiddenFilmsClient]'s
     *  response never carries `language` at all, only [LanguageClient]'s does.
     *
     *  The one exception is a PENDING local pick (see
     *  [SyncPrefs.pendingLanguagePush]): it is newer than anything the account
     *  holds, so it is pushed rather than overwritten — whether it was made
     *  just before this reconcile (a pick recreates the activity, whose
     *  onResume reconciles inside the push debounce), while the fetch was in
     *  flight, or its push failed — and a pick SENT while the fetch was in
     *  flight makes its answer stale, so it is dropped. Mirrors iOS
     *  `reconcileLanguage`. */
    private suspend fun reconcileLanguage() {
        if (!loggedIn) return
        if (prefs.pendingLanguagePush() != null) return sendPendingLanguage()
        // A network error leaves local state authoritative: prefs untouched.
        val confirmedBeforeFetch = languagePushesConfirmed
        runCatchingCancellable {
            val remoteLang = languageClient.fetch()
            if (prefs.pendingLanguagePush() != null) return sendPendingLanguage()
            // A pick sent while the fetch was on the wire is newer than its answer.
            if (languagePushesConfirmed != confirmedBeforeFetch) return
            val localLang = prefs.selectedLanguageTag.first()
            if (remoteLang != null) {
                adopt(remoteLang)
            } else if (localLang != null) {
                prefs.setPendingLanguagePush(localLang)
                sendPendingLanguage()
            }
        }
    }

    /** Send the pending pick now, superseding any debounce, ONE push at a
     *  time: a caller arriving while another push is on the wire waits for it,
     *  then sends whatever is pending by then — so two PUTs never race and the
     *  latest pick is the one the account ends on. On success the account
     *  holds the sent value, and a pick made meanwhile is sent next; on a
     *  transient failure the pick stays pending, and the next reconcile
     *  (resume, login) retries it — the same self-heal the hiddenFilms writes
     *  rely on; a permanent refusal ([LanguagePushRefused]) drops it and takes
     *  the account's pick instead. */
    private suspend fun sendPendingLanguage() {
        languageDebounceJob?.cancel()
        languageDebounceJob = null
        languageSendMutex.withLock {
            while (loggedIn) {
                val pending = prefs.pendingLanguagePush() ?: return
                languageSendInFlight = true
                runCatchingCancellable {
                    try { languageClient.push(pending) } finally { languageSendInFlight = false }
                }.onFailure { failure ->
                    // It may or may not have landed: the account's value is
                    // unknown until the next fetch, so no pick is a no-op.
                    accountLanguage = null
                    // Refused for good: it can never land, so stop owing it, and
                    // take the account's pick instead — never pushing this one back.
                    if (failure is LanguagePushRefused && prefs.pendingLanguagePush() == pending) {
                        prefs.setPendingLanguagePush(null)
                        runCatchingCancellable { languageClient.fetch() }.getOrNull()?.let { adopt(it) }
                    }
                    return
                }
                accountLanguage = pending
                languagePushesConfirmed++
                if (prefs.pendingLanguagePush() == pending) {
                    prefs.setPendingLanguagePush(null)
                    return
                }
            }
        }
    }

    /** Take the account's pick as this device's. */
    private suspend fun adopt(remoteLang: String) {
        accountLanguage = remoteLang
        if (remoteLang != prefs.selectedLanguageTag.first()) prefs.setLanguageTag(remoteLang)
    }

    /** React to every local language change after the login reconcile. A
     *  change to [accountLanguage] merely adopted the server's pick (or went
     *  back to it with no other pick's push on the wire), so it is never
     *  echoed; anything else is a local pick, marked pending and pushed after
     *  a 400 ms debounce so a rapid run of picker taps folds into one PUT. `distinctUntilChanged` because the
     *  DataStore flow re-emits the same tag on every prefs write (including
     *  the pending-pick write below); `drop(1)` skips the current
     *  (post-reconcile) value. */
    private suspend fun observeLanguage() {
        prefs.selectedLanguageTag
            .distinctUntilChanged()
            .drop(1)
            .collect { tag ->
                if (tag == null) return@collect
                // A push on the wire may already have reached the server, so
                // a pick back to [accountLanguage] must be sent after it too.
                languageDebounceJob?.cancel()
                languageDebounceJob = null
                if (tag == accountLanguage && !languageSendInFlight) {
                    prefs.setPendingLanguagePush(null)
                } else {
                    prefs.setPendingLanguagePush(tag)
                    languageDebounceJob = scope.launch {
                        delay(LANGUAGE_DEBOUNCE_MS)
                        scope.launch { sendPendingLanguage() }
                    }
                }
            }
    }

    /** Hide one film in THIS country: update local prefs immediately (the
     *  caller's responsibility — see [pl.kinowo.ui.KinowoViewModel.hide]) then
     *  queue and send the write in the background. */
    fun hide(title: String) = push(HiddenFilmsOp.Hide(title))

    fun unhide(title: String) = push(HiddenFilmsOp.Unhide(title))

    fun clear() = push(HiddenFilmsOp.Clear)

    /** Queue [op] (persisted — see [SyncPrefs.pendingHiddenFilmsOps]) and send
     *  the queue. One that fails stays queued; the next reconcile re-sends it
     *  before fetching. */
    private fun push(op: HiddenFilmsOp) {
        if (!loggedIn) return
        // UNDISPATCHED, so the (fair) queue lock is requested in call order and
        // a hide and the unhide right after it are queued in that order.
        scope.launch(start = CoroutineStart.UNDISPATCHED) {
            val country = queueMutex.withLock {
                currentCountry().also { prefs.setPendingHiddenFilmsOps(it, prefs.pendingHiddenFilmsOps(it) + op) }
            }
            sendPendingOps(country)
        }
    }

    /** Send [country]'s queued edits in order, one flush at a time. Returns
     *  whether the queue is now empty. After each accepted edit the response's
     *  validators are kept only when its set is exactly the local bucket —
     *  otherwise (another device changed the set, or more edits are still
     *  queued) they would vouch for a set this device doesn't hold, and are
     *  dropped so the next fetch is unconditional. Mirrors iOS
     *  `sendPendingChanges`. */
    private suspend fun sendPendingOps(country: String): Boolean = flushMutex.withLock {
        val startedIn = session
        var op = prefs.pendingHiddenFilmsOps(country).firstOrNull()
        while (op != null) {
            if (!loggedIn) return@withLock false
            val result = runCatchingCancellable {
                when (op) {
                    is HiddenFilmsOp.Hide -> client.hide(country, op.title)
                    is HiddenFilmsOp.Unhide -> client.unhide(country, op.title)
                    HiddenFilmsOp.Clear -> client.clear(country)
                }
            }.getOrElse { return@withLock false }
            // A logout while it was on the wire forgot the queue it came from.
            if (session != startedIn) return@withLock false
            val remaining = queueMutex.withLock {
                prefs.pendingHiddenFilmsOps(country).drop(1).also { prefs.setPendingHiddenFilmsOps(country, it) }
            }
            if (remaining.isEmpty() && result.hiddenFilms == prefs.hiddenFilmsFor(country)) {
                prefs.setHiddenFilmsValidators(country, result.etag, result.lastModified)
            } else {
                prefs.setHiddenFilmsValidators(country, null, null)
            }
            op = remaining.firstOrNull()
        }
        true
    }

    private companion object {
        const val LANGUAGE_DEBOUNCE_MS = 400L
    }
}
