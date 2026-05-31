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
 * Per-device hidden-films + disabled-cinemas state, persisted with
 * Preferences DataStore. Mirrors what the iOS app keeps in UserDefaults and
 * the web keeps in `localStorage` for anonymous users. (v1 is local-only —
 * no server sync.)
 */
class UserPreferences(private val context: Context) {

    val hiddenFilms: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_HIDDEN] ?: emptySet() }

    val disabledCinemas: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_DISABLED] ?: emptySet() }

    /** True once the user has swiped between Filmy / Kina at least once. */
    val hasSwipedScreens: Flow<Boolean> =
        context.dataStore.data.map { it[KEY_SWIPED] ?: false }

    /** `yyyy-MM-dd` of the last day the swipe hint was shown, or "" if never. */
    val swipeHintShownDate: Flow<String> =
        context.dataStore.data.map { it[KEY_HINT_DATE] ?: "" }

    suspend fun hide(title: String) = context.dataStore.edit { prefs ->
        prefs[KEY_HIDDEN] = (prefs[KEY_HIDDEN] ?: emptySet()) + title
    }

    suspend fun unhide(title: String) = context.dataStore.edit { prefs ->
        prefs[KEY_HIDDEN] = (prefs[KEY_HIDDEN] ?: emptySet()) - title
    }

    suspend fun unhideAll() = context.dataStore.edit { prefs ->
        prefs[KEY_HIDDEN] = emptySet()
    }

    suspend fun setDisabledCinemas(cinemas: Set<String>) = context.dataStore.edit { prefs ->
        prefs[KEY_DISABLED] = cinemas
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

    private companion object {
        val KEY_HIDDEN = stringSetPreferencesKey("hiddenFilms")
        val KEY_DISABLED = stringSetPreferencesKey("disabledCinemas")
        val KEY_SWIPED = booleanPreferencesKey("swipedScreens")
        val KEY_HINT_DATE = stringPreferencesKey("swipeHintShownDate")
    }
}
