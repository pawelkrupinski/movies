package pl.kinowo.data

import android.content.Context
import androidx.datastore.preferences.core.booleanPreferencesKey
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.core.stringSetPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.runBlocking

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

    /** True once [pl.kinowo.auth.StateSyncService] has done its one-time
     *  local→server migration. After that the server is authoritative on every
     *  launch (so removals stick); cleared on logout to re-arm migration. */
    suspend fun isServerStateSynced(): Boolean
    suspend fun setServerStateSynced(synced: Boolean)
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

    /** The single cinema the top-bar pill row narrows the listing to, or null
     *  ("Wszystkie"). Device-local — NOT part of [SyncPrefs], since it's a
     *  single per-session pick rather than the cross-platform disabled-cinema
     *  set. Reset when the city changes (see [pl.kinowo.ui.KinowoViewModel]). */
    val selectedCinema: Flow<String?> =
        context.dataStore.data.map { it[KEY_SELECTED_CINEMA] }

    suspend fun setSelectedCinema(cinema: String?) = context.dataStore.edit { prefs ->
        if (cinema == null) prefs.remove(KEY_SELECTED_CINEMA) else prefs[KEY_SELECTED_CINEMA] = cinema
    }

    /** Slugs of split cities whose first-visit area picker the user has already
     *  completed, so it shows once per city. Device-local (not synced). */
    val areaPickerSeenCities: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_AREA_SEEN] ?: emptySet() }

    suspend fun markAreaPickerSeen(slug: String) = context.dataStore.edit { prefs ->
        prefs[KEY_AREA_SEEN] = (prefs[KEY_AREA_SEEN] ?: emptySet()) + slug
    }

    /** Slug of the city the user picked (or was located into), or null until
     *  the first-launch city gate resolves one. Gates the repertoire fetch. */
    val selectedCity: Flow<String?> =
        context.dataStore.data.map { it[KEY_CITY] }

    suspend fun setCity(slug: String) = context.dataStore.edit { prefs ->
        prefs[KEY_CITY] = slug
    }

    /** Forget the selected city, re-arming the first-launch city gate. Used when
     *  the country switches: each country serves a disjoint set of cities, so the
     *  old country's slug must not linger against the new country's host. */
    suspend fun clearCity() = context.dataStore.edit { prefs ->
        prefs.remove(KEY_CITY)
    }

    /** ISO country code the user picked (see [pl.kinowo.model.Country]), or null
     *  until they choose one — then [pl.kinowo.model.Country.byCode] resolves the
     *  default (Poland). Drives BOTH the API base URL and the forced UI language,
     *  so switching it re-points the network layer and re-localizes the app. */
    val selectedCountryCode: Flow<String?> =
        context.dataStore.data.map { it[KEY_COUNTRY] }

    suspend fun setCountryCode(code: String) = context.dataStore.edit { prefs ->
        prefs[KEY_COUNTRY] = code
    }

    /** The persisted country code read synchronously, or null if none. Used at
     *  activity attach/wiring time, before any coroutine scope exists, to pick
     *  the API base URL and forced locale. Everywhere else observe the
     *  [selectedCountryCode] flow instead. */
    fun blockingCountryCode(): String? = runBlocking { selectedCountryCode.first() }

    /** The `chosen→nearest` pair the "switch to a nearer city" prompt was last
     *  shown for, or null if never. Remembering only the single most-recent pair
     *  means the prompt fires once per pair but re-asks once the pair changes. */
    val citySwitchPromptKey: Flow<String?> =
        context.dataStore.data.map { it[KEY_CITY_SWITCH_PROMPT] }

    suspend fun setCitySwitchPromptKey(key: String) = context.dataStore.edit { prefs ->
        prefs[KEY_CITY_SWITCH_PROMPT] = key
    }

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

    override suspend fun isServerStateSynced(): Boolean =
        context.dataStore.data.map { it[KEY_SERVER_SYNCED] ?: false }.first()

    override suspend fun setServerStateSynced(synced: Boolean) {
        context.dataStore.edit { prefs -> prefs[KEY_SERVER_SYNCED] = synced }
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
        val KEY_SELECTED_CINEMA = stringPreferencesKey("selectedCinema")
        val KEY_CITY = stringPreferencesKey("selectedCity")
        val KEY_COUNTRY = stringPreferencesKey("selectedCountryCode")
        val KEY_CITY_SWITCH_PROMPT = stringPreferencesKey("citySwitchPromptKey")
        val KEY_SWIPED = booleanPreferencesKey("swipedScreens")
        val KEY_HINT_DATE = stringPreferencesKey("swipeHintShownDate")
        val KEY_SERVER_SYNCED = booleanPreferencesKey("serverStateSynced")
        val KEY_POSTER_URLS = stringSetPreferencesKey("seenPosterUrls")
        val KEY_POSTER_PURGE_DATE = stringPreferencesKey("posterPurgeDate")
        val KEY_AREA_SEEN = stringSetPreferencesKey("areaPickerSeenCities")
    }
}
