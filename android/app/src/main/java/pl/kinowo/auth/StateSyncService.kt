package pl.kinowo.auth

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.combine
import kotlinx.coroutines.flow.debounce
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch
import pl.kinowo.data.SyncPrefs

/**
 * Keeps per-device [SyncPrefs] in step with the server while signed in — the
 * Android counterpart of iOS `StateSyncService`.
 *
 * On login: pull the remote state, union it with local (so neither device
 * loses a hide / a disabled cinema), write the merged set back into prefs,
 * and push the union to the server. Thereafter every local change is pushed,
 * debounced 400 ms so a burst of toggles is one request. On logout the
 * observation stops; local prefs stay put.
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
            val localDisabled = prefs.disabledCinemas.first()
            val mergedHidden = localHidden + remote.hiddenFilms
            val mergedDisabled = localDisabled + remote.disabledCinemas
            if (mergedHidden != localHidden) prefs.setHiddenFilms(mergedHidden)
            if (mergedDisabled != localDisabled) prefs.setDisabledCinemas(mergedDisabled)
            client.putState(UserSyncState(mergedHidden, mergedDisabled))
        } catch (_: Exception) {
            // Network error — local state is authoritative; leave prefs alone.
        }
    }

    /** Push subsequent local edits. `drop(1)` skips the current (post-merge)
     *  value so we only react to real changes; the collect runs until the
     *  job is cancelled on logout. */
    private suspend fun observePrefs() {
        combine(prefs.hiddenFilms, prefs.disabledCinemas) { hidden, disabled ->
            UserSyncState(hidden, disabled)
        }
            .drop(1)
            .debounce(DEBOUNCE_MS)
            .collect { state ->
                if (loggedIn) runCatching { client.putState(state) }
            }
    }

    private companion object {
        const val DEBOUNCE_MS = 400L
    }
}
