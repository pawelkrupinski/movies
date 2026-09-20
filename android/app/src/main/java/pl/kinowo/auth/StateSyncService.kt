package pl.kinowo.auth

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.debounce
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch
import pl.kinowo.data.SyncPrefs

/**
 * Keeps per-device [SyncPrefs] in step with the server while signed in — the
 * Android counterpart of iOS `StateSyncService`.
 *
 * hiddenFilms ONLY — disabledCinemas is device-local now (see
 * [UserSyncState]'s doc comment), so this class never reads or writes
 * `prefs.disabledCinemas` in either direction.
 *
 * The FIRST sync after a login migrates this device's local picks up: union
 * local with remote (so nothing set while signed-out is lost), write the union
 * into prefs, push it, and set the `serverStateSynced` flag. EVERY sync after
 * that treats the SERVER as the source of truth — local prefs are replaced
 * with the remote set, so a hide removed elsewhere stays removed instead of
 * being resurrected by a blind union on the next launch. Thereafter every
 * local change is pushed, debounced 400 ms so a burst of toggles is one
 * request. On logout the observation stops and the flag is cleared so the
 * next sign-in migrates afresh.
 *
 * A failed pull leaves local prefs authoritative (no overwrite, no push) —
 * exactly the offline behaviour iOS has.
 */
class StateSyncService(
    private val prefs: SyncPrefs,
    private val user: StateFlow<UserProfile?>,
    private val client: UserStateClient,
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
                    // Clear the migration flag only on a GENUINE logout, not the
                    // initial null the flow holds before a session restores —
                    // otherwise every cold start would re-run the first-login
                    // union instead of treating the server as authoritative.
                    if (loggedIn) prefs.setServerStateSynced(false)
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
            mergeWithServer()
            observePrefs()
        }
    }

    private suspend fun mergeWithServer() {
        try {
            val remote = client.fetchState()
            val localHidden = prefs.hiddenFilms.first()
            if (prefs.isServerStateSynced()) {
                // Already migrated — the server is the source of truth. Mirror it
                // so a hide removed on another device (or this one, last session)
                // stays gone instead of being unioned back.
                if (remote.hiddenFilms != localHidden) prefs.setHiddenFilms(remote.hiddenFilms)
            } else {
                // First sync after login — union local picks up so nothing set
                // while signed-out is lost, push it, then flip the flag.
                val mergedHidden = localHidden + remote.hiddenFilms
                if (mergedHidden != localHidden) prefs.setHiddenFilms(mergedHidden)
                client.putState(UserSyncState(mergedHidden))
                prefs.setServerStateSynced(true)
            }
        } catch (_: Exception) {
            // Network error — local state is authoritative; leave prefs + flag alone.
        }
    }

    /** Push subsequent local hiddenFilms edits. `drop(1)` skips the current
     *  (post-merge) value so we only react to real changes; the collect runs
     *  until the job is cancelled on logout. disabledCinemas changes never
     *  reach here at all — see the class doc. */
    private suspend fun observePrefs() {
        prefs.hiddenFilms
            .drop(1)
            .debounce(DEBOUNCE_MS)
            .collect { hidden ->
                if (loggedIn) runCatching { client.putState(UserSyncState(hidden)) }
            }
    }

    private companion object {
        const val DEBOUNCE_MS = 400L
    }
}
