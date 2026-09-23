package pl.kinowo.auth

import pl.kinowo.runCatchingCancellable
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.debounce
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch
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
 * Local hide/unhide push IMMEDIATELY now — see [hide]/[unhide]/[clear] — no
 * debounce: the old 400 ms debounce existed to batch several toggles into one
 * bulk PUT body, and there is no bulk write left to batch into.
 *
 * A failed pull leaves local prefs authoritative (no overwrite) — exactly the
 * offline behaviour iOS has. A failed push is silently swallowed the same
 * way; a later reconcile (login, resume) self-heals a write that never
 * landed, since local state already reflects it either way.
 *
 * The language pick rides the LEGACY `/api/me/state` document instead (via
 * [LanguageClient] — there's no granular endpoint for a single scalar), and
 * is NOT gated by the hiddenFilms per-country migration flags — see
 * [reconcileLanguage] for why a scalar pick doesn't need the two-phase dance
 * the sets do. It reconciles at the SAME triggers as hiddenFilms (login,
 * resume, country switch) but as its own independent step, and pushes a
 * local change on its own debounce — see [observeLanguage].
 */
class StateSyncService(
    private val prefs: SyncPrefs,
    private val user: StateFlow<UserProfile?>,
    private val client: HiddenFilmsClient,
    private val languageClient: LanguageClient,
    private val scope: CoroutineScope,
) {
    @Volatile private var loggedIn = false
    private var syncJob: Job? = null

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
                    if (loggedIn) prefs.clearHiddenFilmsSyncState()
                    loggedIn = false
                    syncJob?.cancel()
                    syncJob = null
                }
            }
        }
    }

    private fun onLogin() {
        syncJob?.cancel()
        syncJob = scope.launch {
            reconcileCurrentCountry() // also reconciles language — see its doc
            observeLanguage()
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
        // A network error leaves local state authoritative: prefs + flags untouched.
        runCatchingCancellable {
            if (prefs.isHiddenFilmsMigrated(country)) {
                when (val result = client.fetch(country, prefs.hiddenFilmsEtag(country), prefs.hiddenFilmsLastModified(country))) {
                    is HiddenFilmsFetchResult.NotModified -> Unit
                    is HiddenFilmsFetchResult.Changed -> {
                        val state = result.state
                        if (state.hiddenFilms != prefs.hiddenFilmsFor(country)) prefs.setHiddenFilms(country, state.hiddenFilms)
                        prefs.setHiddenFilmsValidators(country, state.etag, state.lastModified)
                    }
                }
            } else {
                val remote = (client.fetch(country, null, null) as? HiddenFilmsFetchResult.Changed)?.state
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

    /** Language is a scalar, not a set, so it skips the per-country
     *  migration-flag dance [reconcile] needs entirely — there's no "removed
     *  on another device" case a blind overwrite could wrongly resurrect, so
     *  every reconcile (first or not) uses the same rule: the ACCOUNT's
     *  explicit pick wins whenever it has one (restored on login, per spec);
     *  otherwise this device's own explicit pick, if any, becomes the
     *  account's. A separate fetch from [reconcile]'s — [HiddenFilmsClient]'s
     *  response never carries `language` at all, only [LanguageClient]'s does. */
    private suspend fun reconcileLanguage() {
        if (!loggedIn) return
        // A network error leaves local state authoritative: prefs untouched.
        runCatchingCancellable {
            val remoteLang = languageClient.fetch()
            val localLang = prefs.selectedLanguageTag.first()
            if (remoteLang != null && remoteLang != localLang) {
                prefs.setLanguageTag(remoteLang)
            } else if (remoteLang == null && localLang != null) {
                languageClient.push(localLang)
            }
        }
    }

    /** Push a local language change as soon as it happens — same immediate,
     *  no-debounce-batching reasoning [hide]/[unhide]/[clear] already have,
     *  though a picker choice is rare enough that batching was never really
     *  the point; matches the shape all the same. `drop(1)` skips the
     *  current (post-reconcile) value so this only reacts to a REAL local
     *  change, not the one [reconcileLanguage] itself might have just made. */
    private suspend fun observeLanguage() {
        prefs.selectedLanguageTag
            .drop(1)
            .debounce(LANGUAGE_DEBOUNCE_MS)
            .collect { tag ->
                if (loggedIn && tag != null) runCatchingCancellable { languageClient.push(tag) }
            }
    }

    /** Hide one film in THIS country: update local prefs immediately (the
     *  caller's responsibility — see [pl.kinowo.ui.KinowoViewModel.hide]) then
     *  push the write in the background, fire-and-forget. */
    fun hide(title: String) = push { country -> client.hide(country, title) }

    fun unhide(title: String) = push { country -> client.unhide(country, title) }

    fun clear() = push { country -> client.clear(country) }

    private fun push(write: suspend (country: String) -> HiddenFilmsState) {
        if (!loggedIn) return
        scope.launch {
            val country = currentCountry()
            runCatchingCancellable { write(country) }
                .onSuccess { prefs.setHiddenFilmsValidators(country, it.etag, it.lastModified) }
        }
    }

    private companion object {
        const val LANGUAGE_DEBOUNCE_MS = 400L
    }
}
