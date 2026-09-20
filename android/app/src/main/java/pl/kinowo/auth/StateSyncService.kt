package pl.kinowo.auth

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch
import pl.kinowo.data.SyncPrefs

/**
 * Keeps per-device [SyncPrefs] hiddenFilms in step with the server, per
 * COUNTRY, while signed in — the Android counterpart of iOS
 * `StateSyncService`. disabledCinemas is device-local (see [SyncPrefs]'s doc
 * comment) and never touched here.
 *
 * hiddenFilms is per-country server-side now (`/api/me/{country}/hidden-films`
 * — a title isn't globally unique across countries the way a cinema display
 * name is), so migration/authority is tracked per country too: the FIRST
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
 */
class StateSyncService(
    private val prefs: SyncPrefs,
    private val user: StateFlow<UserProfile?>,
    private val client: HiddenFilmsClient,
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
        syncJob = scope.launch { reconcileCurrentCountry() }
    }

    /** Reconcile whichever country is currently selected. Called on login and
     *  on app foreground-resume; a no-op if no country has been chosen yet
     *  (nothing to reconcile against). Public so [pl.kinowo.ui.KinowoViewModel]
     *  can call it from its `onResume()`. */
    suspend fun reconcileCurrentCountry() {
        val country = prefs.selectedCountryCode.first() ?: return
        reconcile(country)
    }

    private suspend fun reconcile(country: String) {
        try {
            if (prefs.isHiddenFilmsMigrated(country)) {
                when (val result = client.fetch(country, prefs.hiddenFilmsEtag(country), prefs.hiddenFilmsLastModified(country))) {
                    is HiddenFilmsFetchResult.NotModified -> Unit
                    is HiddenFilmsFetchResult.Changed -> {
                        val state = result.state
                        if (state.hiddenFilms != prefs.hiddenFilms.first()) prefs.setHiddenFilms(state.hiddenFilms)
                        prefs.setHiddenFilmsValidators(country, state.etag, state.lastModified)
                    }
                }
            } else {
                val remote = (client.fetch(country, null, null) as? HiddenFilmsFetchResult.Changed)?.state
                val local = prefs.hiddenFilms.first()
                val merged = local + (remote?.hiddenFilms ?: emptySet())
                if (merged != local) prefs.setHiddenFilms(merged)

                var latest = remote
                (local - (remote?.hiddenFilms ?: emptySet())).forEach { title -> latest = client.hide(country, title) }
                latest?.let { prefs.setHiddenFilmsValidators(country, it.etag, it.lastModified) }
                prefs.setHiddenFilmsMigrated(country, true)
            }
        } catch (_: Exception) {
            // Network error — local state is authoritative; leave prefs + flags alone.
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
            val country = prefs.selectedCountryCode.first() ?: return@launch
            runCatching { write(country) }
                .onSuccess { prefs.setHiddenFilmsValidators(country, it.etag, it.lastModified) }
        }
    }
}
