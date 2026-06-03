package pl.kinowo.data

import android.content.Context
import androidx.datastore.preferences.core.booleanPreferencesKey
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.core.stringSetPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.map

private val Context.dataStore by preferencesDataStore(name = "kinowo_prefs")

/**
 * The slice of preferences that [pl.kinowo.auth.StateSyncService] reads and
 * writes when reconciling with the server — the two sets that round-trip to
 * `/api/me/state`. Narrowing the sync service to this interface keeps it
 * unit-testable against an in-memory fake instead of a real DataStore.
 */
interface SyncPrefs {
    val hiddenFilms: Flow<Set<String>>
    val disabledCinemas: Flow<Set<String>>
    suspend fun setHiddenFilms(films: Set<String>)
    suspend fun setDisabledCinemas(cinemas: Set<String>)
}

/**
 * Per-device hidden-films + disabled-cinemas state, persisted with
 * Preferences DataStore. Mirrors what the iOS app keeps in UserDefaults and
 * the web keeps in `localStorage` for anonymous users. When the user signs
 * in, [pl.kinowo.auth.StateSyncService] mirrors these sets to the server.
 */
class UserPreferences(private val context: Context) : SyncPrefs {

    override val hiddenFilms: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_HIDDEN] ?: emptySet() }

    override val disabledCinemas: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_DISABLED] ?: emptySet() }

    /** True once the user has swiped between Filmy / Kina at least once. */
    val hasSwipedScreens: Flow<Boolean> =
        context.dataStore.data.map { it[KEY_SWIPED] ?: false }

    /** `yyyy-MM-dd` of the last day the swipe hint was shown, or "" if never. */
    val swipeHintShownDate: Flow<String> =
        context.dataStore.data.map { it[KEY_HINT_DATE] ?: "" }

    /** Poster URLs Coil has been asked to cache, persisted so the daily purge
     *  can evict the ones that later fall out of the repertoire (Coil's
     *  DiskCache can't enumerate its own keys). See [PosterCachePurge]. */
    val seenPosterUrls: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_POSTER_URLS] ?: emptySet() }

    /** `yyyy-MM-dd` of the last day the poster purge ran, or "" if never. */
    val posterPurgeDate: Flow<String> =
        context.dataStore.data.map { it[KEY_POSTER_PURGE_DATE] ?: "" }

    suspend fun hide(title: String) = context.dataStore.edit { prefs ->
        prefs[KEY_HIDDEN] = (prefs[KEY_HIDDEN] ?: emptySet()) + title
    }

    suspend fun unhide(title: String) = context.dataStore.edit { prefs ->
        prefs[KEY_HIDDEN] = (prefs[KEY_HIDDEN] ?: emptySet()) - title
    }

    suspend fun unhideAll() = context.dataStore.edit { prefs ->
        prefs[KEY_HIDDEN] = emptySet()
    }

    override suspend fun setHiddenFilms(films: Set<String>) {
        context.dataStore.edit { prefs -> prefs[KEY_HIDDEN] = films }
    }

    override suspend fun setDisabledCinemas(cinemas: Set<String>) {
        context.dataStore.edit { prefs -> prefs[KEY_DISABLED] = cinemas }
    }

    suspend fun toggleCinema(cinema: String, disabled: Boolean) = context.dataStore.edit { prefs ->
        val current = prefs[KEY_DISABLED] ?: emptySet()
        prefs[KEY_DISABLED] = if (disabled) current + cinema else current - cinema
    }

    suspend fun markSwiped() = context.dataStore.edit { prefs ->
        prefs[KEY_SWIPED] = true
    }

    suspend fun markSwipeHintShown(date: String) = context.dataStore.edit { prefs ->
        prefs[KEY_HINT_DATE] = date
    }

    suspend fun setSeenPosterUrls(urls: Set<String>) = context.dataStore.edit { prefs ->
        prefs[KEY_POSTER_URLS] = urls
    }

    suspend fun setPosterPurgeDate(date: String) = context.dataStore.edit { prefs ->
        prefs[KEY_POSTER_PURGE_DATE] = date
    }

    private companion object {
        val KEY_HIDDEN = stringSetPreferencesKey("hiddenFilms")
        val KEY_DISABLED = stringSetPreferencesKey("disabledCinemas")
        val KEY_SWIPED = booleanPreferencesKey("swipedScreens")
        val KEY_HINT_DATE = stringPreferencesKey("swipeHintShownDate")
        val KEY_POSTER_URLS = stringSetPreferencesKey("seenPosterUrls")
        val KEY_POSTER_PURGE_DATE = stringPreferencesKey("posterPurgeDate")
    }
}
