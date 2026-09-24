package pl.kinowo.data

import android.content.Context
import androidx.datastore.preferences.core.MutablePreferences
import androidx.datastore.preferences.core.Preferences
import androidx.datastore.preferences.core.booleanPreferencesKey
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.core.stringSetPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import pl.kinowo.model.Country

private val Context.dataStore by preferencesDataStore(name = "kinowo_prefs")

/** One local hiddenFilms edit waiting to reach the server — see
 *  [SyncPrefs.pendingHiddenFilmsOps]. [token] is the persisted form, the same
 *  `hide:`/`unhide:`/`clear` tokens iOS stores for its `HiddenFilmsChange`. */
sealed interface HiddenFilmsOp {
    val token: String

    data class Hide(val title: String) : HiddenFilmsOp { override val token get() = "hide:$title" }
    data class Unhide(val title: String) : HiddenFilmsOp { override val token get() = "unhide:$title" }
    data object Clear : HiddenFilmsOp { override val token get() = "clear" }

    companion object {
        fun fromToken(token: String): HiddenFilmsOp? = when {
            token == "clear" -> Clear
            token.startsWith("hide:") -> Hide(token.removePrefix("hide:"))
            token.startsWith("unhide:") -> Unhide(token.removePrefix("unhide:"))
            else -> null
        }
    }
}

/**
 * The slice of preferences [pl.kinowo.auth.StateSyncService] touches:
 * hiddenFilms (round-trips to `/api/me/{country}/hidden-films`) and the
 * language pick (still rides the legacy `/api/me/state` — there's no granular
 * endpoint for a single scalar). Narrowing the sync service to this interface
 * keeps it unit-testable against an in-memory fake instead of a real DataStore.
 *
 * hiddenFilms is keyed by country on this side too, because it is on the
 * server: a sync that computed a country and then wrote a device-wide set
 * could only ever be right for one country at a time.
 */
interface SyncPrefs {
    val selectedCountryCode: Flow<String?>

    /** [country]'s hidden titles (server code space — `pl`, `uk`, …). */
    suspend fun hiddenFilmsFor(country: String): Set<String>
    suspend fun setHiddenFilms(country: String, films: Set<String>)

    /** True once [pl.kinowo.auth.StateSyncService] has done its one-time
     *  local→server migration FOR THIS COUNTRY. After that the server is
     *  authoritative for that country on every reconcile (so removals stick);
     *  cleared (for every country) on logout to re-arm migration. Per-country
     *  because hiddenFilms itself now is — see [models.UserState.hiddenFilmsByCountry]
     *  server-side: a title isn't globally unique across countries the way a
     *  cinema display name is. */
    suspend fun isHiddenFilmsMigrated(country: String): Boolean
    suspend fun setHiddenFilmsMigrated(country: String, migrated: Boolean)

    /** The `ETag`/`Last-Modified` the last successful fetch or write for THIS
     *  country returned — sent back as `If-None-Match`/`If-Modified-Since` on
     *  the next conditional fetch. Null until something has synced for that
     *  country. */
    suspend fun hiddenFilmsEtag(country: String): String?
    suspend fun hiddenFilmsLastModified(country: String): String?
    suspend fun setHiddenFilmsValidators(country: String, etag: String?, lastModified: String?)

    /** [country]'s local hide/unhide/clear edits the server hasn't accepted
     *  yet, oldest first. [pl.kinowo.auth.StateSyncService] queues each edit
     *  here before sending it and removes it once sent, so one that failed (or
     *  never ran — the process died) is re-sent by the next reconcile.
     *  Forgotten on a genuine logout with the rest of the sync state. */
    suspend fun pendingHiddenFilmsOps(country: String): List<HiddenFilmsOp>
    suspend fun setPendingHiddenFilmsOps(country: String, ops: List<HiddenFilmsOp>)

    /** Forget every country's migration flag and validators — a genuine
     *  logout, so the next sign-in re-runs the union-then-push migration for
     *  whichever country is current then, fresh. */
    suspend fun clearHiddenFilmsSyncState()

    /** The explicit language pick, or null until one is made — unlike the two
     *  sets above this is already "explicit or nothing" with no separate
     *  resolved-default reading to distinguish it from (see
     *  [pl.kinowo.model.LanguageDefault] for where the resolved default,
     *  which this never carries, comes from instead). */
    val selectedLanguageTag: Flow<String?>
    suspend fun setLanguageTag(tag: String)

    /** A language pick the account hasn't confirmed yet — its debounced push
     *  is still waiting, or it failed. Persisted so a pick whose push never
     *  landed (the app killed inside the debounce, or offline) is still pushed
     *  after a relaunch instead of losing to the account's older value. See
     *  [pl.kinowo.auth.StateSyncService]; mirrors iOS `pendingLanguagePush`. */
    suspend fun pendingLanguagePush(): String?
    suspend fun setPendingLanguagePush(tag: String?)
}

/**
 * Per-device preferences, persisted with Preferences DataStore. Mirrors what
 * the iOS app keeps in UserDefaults and the web keeps in `localStorage` for
 * anonymous users. When the user signs in, [pl.kinowo.auth.StateSyncService]
 * mirrors hiddenFilms (per country) to the server — disabledCinemas is
 * device-local only, in every direction.
 */
class UserPreferences(private val context: Context) : SyncPrefs {

    /** The hidden titles of the country being browsed — re-emits on a
     *  country switch, since both come from the same snapshot. */
    val hiddenFilms: Flow<Set<String>> =
        context.dataStore.data.map { it.hiddenIn(it.currentCountry()) }

    val disabledCinemas: Flow<Set<String>> =
        context.dataStore.data.map { it[KEY_DISABLED] ?: emptySet() }

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
        // The gate is satisfied however the city was reached, so an explicit-pick
        // request never outlives the gate that asked for it.
        prefs.remove(KEY_EXPLICIT_PICK)
    }

    /** True while the city gate must present the country's list instead of
     *  offering a located city. Armed when the user picks a country themselves:
     *  they have just said which country they want, and answering that with
     *  "you're near Poznan" offers the very thing they navigated away from.
     *
     *  Persisted rather than held in memory because switching country recreates
     *  the activity (and with it the ViewModel), so an in-memory flag would be
     *  gone by the time the gate reads it. Cleared by [setCity]. */
    val awaitingExplicitCityPick: Flow<Boolean> =
        context.dataStore.data.map { it[KEY_EXPLICIT_PICK] ?: false }

    suspend fun awaitExplicitCityPick() = context.dataStore.edit { prefs ->
        prefs[KEY_EXPLICIT_PICK] = true
    }

    /** Forget the selected city, re-arming the first-launch city gate. Used when
     *  the country switches: each country serves a disjoint set of cities, so the
     *  old country's slug must not linger against the new country's host. */
    suspend fun clearCity() = context.dataStore.edit { prefs ->
        prefs.remove(KEY_CITY)
    }

    /** ISO country code the user picked (see [pl.kinowo.model.Country]), or null
     *  until they choose one — then [pl.kinowo.model.Country.byCode] resolves the
     *  default (Poland). Drives the API base URL. The forced UI language is a
     *  SEPARATE, independent pick — see [selectedLanguageTag]. */
    override val selectedCountryCode: Flow<String?> =
        context.dataStore.data.map { it[KEY_COUNTRY] }

    suspend fun setCountryCode(code: String) = context.dataStore.edit { prefs ->
        prefs.settleLegacyHiddenFilms()
        prefs[KEY_COUNTRY] = code
    }

    /** Persist a located city that sits in a DIFFERENT country than the one
     *  currently selected, together with the country switch, in ONE DataStore
     *  transaction. MUST be atomic: MainActivity recreates the activity (and
     *  clears the ViewModel's coroutine scope) the moment [selectedCountryCode]
     *  changes, so writing the country and the city as two separate [edit]
     *  calls lets that recreate race ahead of the second call and cancel it —
     *  the city is then never adopted and the "which city did you mean" dialog
     *  vanishes with the teardown. [pendingDeepLink], when given, rides the
     *  same write for the recreated ViewModel to apply. See
     *  [pl.kinowo.ui.KinowoViewModel.adoptDetectedCity]. */
    suspend fun setCityInCountry(slug: String, code: String, pendingDeepLink: String? = null) =
        context.dataStore.edit { prefs ->
            prefs.settleLegacyHiddenFilms()
            prefs[KEY_COUNTRY] = code
            prefs[KEY_CITY] = slug
            prefs.remove(KEY_EXPLICIT_PICK)
            if (pendingDeepLink != null) prefs[KEY_PENDING_DEEP_LINK] = pendingDeepLink
        }

    /** Read-and-remove the deep link a cross-country switch handed over (see
     *  [setCityInCountry]) — atomically, so it is applied at most once. */
    suspend fun takePendingDeepLink(): String? {
        var link: String? = null
        context.dataStore.edit { prefs ->
            link = prefs[KEY_PENDING_DEEP_LINK]
            prefs.remove(KEY_PENDING_DEEP_LINK)
        }
        return link
    }

    /** [selectedCountryCode] and [selectedCity] paired from the SAME
     *  snapshot. Unlike `combine(selectedCountryCode, selectedCity)`, which
     *  re-emits whenever EITHER upstream flow delivers — and each is its
     *  own independent collection of [Context.dataStore], even though both
     *  derive from it — this can never expose the torn intermediate pairing
     *  [setCityInCountry] exists to prevent: `combine`'s per-upstream
     *  re-emission is its own source of tearing, separate from whether the
     *  underlying `edit {}` write was atomic (it is). */
    val countryAndCity: Flow<Pair<String?, String?>> =
        context.dataStore.data.map { it[KEY_COUNTRY] to it[KEY_CITY] }

    /** The persisted country code read synchronously, or null if none. Used at
     *  activity attach/wiring time, before any coroutine scope exists, to pick
     *  the API base URL and forced locale. Everywhere else observe the
     *  [selectedCountryCode] flow instead. */
    fun blockingCountryCode(): String? = runBlocking { selectedCountryCode.first() }

    /** BCP-47 language tag the user explicitly picked in the Filtry sheet, or
     *  null until they choose one — then [pl.kinowo.model.LanguageDefault]
     *  resolves the default. Deliberately independent of [selectedCountryCode]:
     *  switching country must never move this (and vice versa), since a country
     *  switch only re-points the API base URL now — see [pl.kinowo.model.Country]. */
    override val selectedLanguageTag: Flow<String?> =
        context.dataStore.data.map { it[KEY_LANGUAGE] }

    override suspend fun setLanguageTag(tag: String) {
        context.dataStore.edit { prefs -> prefs[KEY_LANGUAGE] = tag }
    }

    override suspend fun pendingLanguagePush(): String? =
        context.dataStore.data.first()[KEY_PENDING_LANGUAGE]

    override suspend fun setPendingLanguagePush(tag: String?) {
        context.dataStore.edit { prefs ->
            if (tag == null) prefs.remove(KEY_PENDING_LANGUAGE) else prefs[KEY_PENDING_LANGUAGE] = tag
        }
    }

    /** The persisted language tag read synchronously, or null if none. Used at
     *  activity attach/wiring time, before any coroutine scope exists, to force
     *  the locale. Everywhere else observe the [selectedLanguageTag] flow instead. */
    fun blockingLanguageTag(): String? = runBlocking { selectedLanguageTag.first() }

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
        val country = prefs.currentCountry()
        prefs.putHidden(country, prefs.hiddenIn(country) + title)
    }

    suspend fun unhide(title: String) = context.dataStore.edit { prefs ->
        val country = prefs.currentCountry()
        prefs.putHidden(country, prefs.hiddenIn(country) - title)
    }

    /** Unhide everything in the country being browsed — the other countries'
     *  sets belong to their own server rows and stay put. */
    suspend fun unhideAll() = context.dataStore.edit { prefs ->
        prefs.putHidden(prefs.currentCountry(), emptySet())
    }

    /** Forget every country's hidden titles (account deletion). */
    suspend fun clearAllHiddenFilms() = context.dataStore.edit { prefs ->
        prefs.asMap().keys.filter { it.name.startsWith(HIDDEN_PREFIX) }.forEach { prefs.remove(it) }
        prefs.remove(KEY_HIDDEN_LEGACY)
    }

    override suspend fun hiddenFilmsFor(country: String): Set<String> =
        context.dataStore.data.first().hiddenIn(country)

    override suspend fun setHiddenFilms(country: String, films: Set<String>) {
        context.dataStore.edit { prefs -> prefs.putHidden(country, films) }
    }

    suspend fun setDisabledCinemas(cinemas: Set<String>) {
        context.dataStore.edit { prefs -> prefs[KEY_DISABLED] = cinemas }
    }

    /** The DataStore is a process singleton, shared by every Robolectric
     *  test in a Gradle fork and by every instrumented test on a device (where
     *  it also outlives the run) — the tests' `FreshUserPreferences` rule
     *  wipes it around each test. */
    @androidx.annotation.VisibleForTesting
    internal suspend fun clearAllForTest() {
        context.dataStore.edit { it.clear() }
    }

    @androidx.annotation.VisibleForTesting
    internal suspend fun writeLegacyHiddenFilms(films: Set<String>) {
        context.dataStore.edit { prefs ->
            prefs.remove(hiddenKey(prefs.currentCountry()))
            prefs[KEY_HIDDEN_LEGACY] = films
        }
    }

    // ── per-country hiddenFilms ──────────────────────────────────────────
    // Builds before per-country sets kept ONE device-wide set under
    // KEY_HIDDEN_LEGACY. It is read as the set of the country being browsed
    // until either that country's own set is first written (the legacy set is
    // folded into it) or the country changes (it is pinned to the country it
    // was made in) — so it never follows the user into another country.

    /** The country being browsed, resolved the way MainActivity picks the
     *  API base URL (null → default, legacy `GB` → `uk`). */
    private fun Preferences.currentCountry(): String = Country.byCode(this[KEY_COUNTRY]).code

    private fun Preferences.hiddenIn(country: String): Set<String> =
        this[hiddenKey(country)]
            ?: this[KEY_HIDDEN_LEGACY]?.takeIf { country == currentCountry() }
            ?: emptySet()

    private fun MutablePreferences.putHidden(country: String, films: Set<String>) {
        if (country == currentCountry()) dropLegacyHiddenFilms()
        this[hiddenKey(country)] = films
    }

    private fun MutablePreferences.settleLegacyHiddenFilms() {
        val legacy = this[KEY_HIDDEN_LEGACY] ?: return
        val country = currentCountry()
        if (this[hiddenKey(country)] == null) this[hiddenKey(country)] = legacy
        dropLegacyHiddenFilms()
    }

    /** Retire the legacy set together with every stored validator: the
     *  device-wide set was reconciled against whichever country was selected
     *  at the time, so no country's ETag describes what its bucket now holds,
     *  and replaying one would draw a 304 that strands the wrong set. The
     *  migrated flags stay, so each country's next reconcile takes a fresh 200
     *  and REPLACES its bucket. Mirrors iOS `settleLegacyHiddenFilms`. */
    private fun MutablePreferences.dropLegacyHiddenFilms() {
        if (remove(KEY_HIDDEN_LEGACY) == null) return
        asMap().keys
            .filter { it.name.startsWith(ETAG_PREFIX) || it.name.startsWith(LAST_MODIFIED_PREFIX) }
            .forEach { remove(it) }
    }

    /** A stored validator, unless the legacy set is still unsettled — see
     *  [dropLegacyHiddenFilms] for why none can be trusted until it is. */
    private fun Preferences.validator(key: Preferences.Key<String>): String? =
        if (this[KEY_HIDDEN_LEGACY] != null) null else this[key]

    override suspend fun isHiddenFilmsMigrated(country: String): Boolean =
        context.dataStore.data.map { it[migratedKey(country)] ?: false }.first()

    override suspend fun setHiddenFilmsMigrated(country: String, migrated: Boolean) {
        context.dataStore.edit { prefs -> prefs[migratedKey(country)] = migrated }
    }

    override suspend fun hiddenFilmsEtag(country: String): String? =
        context.dataStore.data.first().validator(etagKey(country))

    override suspend fun hiddenFilmsLastModified(country: String): String? =
        context.dataStore.data.first().validator(lastModifiedKey(country))

    override suspend fun setHiddenFilmsValidators(country: String, etag: String?, lastModified: String?) {
        context.dataStore.edit { prefs ->
            if (etag == null) prefs.remove(etagKey(country)) else prefs[etagKey(country)] = etag
            if (lastModified == null) prefs.remove(lastModifiedKey(country)) else prefs[lastModifiedKey(country)] = lastModified
        }
    }

    override suspend fun pendingHiddenFilmsOps(country: String): List<HiddenFilmsOp> =
        context.dataStore.data.first()[pendingOpsKey(country)]
            ?.let { runCatching { Json.decodeFromString<List<String>>(it) }.getOrNull() }
            .orEmpty()
            .mapNotNull(HiddenFilmsOp::fromToken)

    override suspend fun setPendingHiddenFilmsOps(country: String, ops: List<HiddenFilmsOp>) {
        context.dataStore.edit { prefs ->
            if (ops.isEmpty()) prefs.remove(pendingOpsKey(country))
            else prefs[pendingOpsKey(country)] = Json.encodeToString(ops.map { it.token })
        }
    }

    override suspend fun clearHiddenFilmsSyncState() {
        context.dataStore.edit { prefs ->
            val toRemove = prefs.asMap().keys.filter {
                it.name.startsWith(MIGRATED_PREFIX) || it.name.startsWith(ETAG_PREFIX) ||
                    it.name.startsWith(LAST_MODIFIED_PREFIX) || it.name.startsWith(PENDING_OPS_PREFIX)
            }
            toRemove.forEach { prefs.remove(it) }
        }
    }

    // Dynamic, per-country keys — DataStore Preferences keys need not be static
    // vals, so "one key per country" is just a computed name rather than a
    // schema migration every time a country is added.
    private fun hiddenKey(country: String) = stringSetPreferencesKey("$HIDDEN_PREFIX$country")
    private fun migratedKey(country: String) = booleanPreferencesKey("$MIGRATED_PREFIX$country")
    private fun etagKey(country: String) = stringPreferencesKey("$ETAG_PREFIX$country")
    private fun lastModifiedKey(country: String) = stringPreferencesKey("$LAST_MODIFIED_PREFIX$country")
    // An ordered list, so a JSON array in one string rather than a (unordered) string set.
    private fun pendingOpsKey(country: String) = stringPreferencesKey("$PENDING_OPS_PREFIX$country")

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
        val KEY_HIDDEN_LEGACY = stringSetPreferencesKey("hiddenFilms")
        const val HIDDEN_PREFIX = "hiddenFilms_"
        val KEY_DISABLED = stringSetPreferencesKey("disabledCinemas")
        val KEY_CITY = stringPreferencesKey("selectedCity")
        val KEY_COUNTRY = stringPreferencesKey("selectedCountryCode")
        val KEY_LANGUAGE = stringPreferencesKey("selectedLanguageTag")
        val KEY_PENDING_LANGUAGE = stringPreferencesKey("pendingLanguagePush")
        val KEY_CITY_SWITCH_PROMPT = stringPreferencesKey("citySwitchPromptKey")
        val KEY_SWIPED = booleanPreferencesKey("swipedScreens")
        val KEY_HINT_DATE = stringPreferencesKey("swipeHintShownDate")
        const val MIGRATED_PREFIX = "hiddenFilmsMigrated_"
        const val ETAG_PREFIX = "hiddenFilmsEtag_"
        const val LAST_MODIFIED_PREFIX = "hiddenFilmsLastModified_"
        const val PENDING_OPS_PREFIX = "pendingHiddenFilmsOps_"
        val KEY_POSTER_URLS = stringSetPreferencesKey("seenPosterUrls")
        val KEY_POSTER_PURGE_DATE = stringPreferencesKey("posterPurgeDate")
        val KEY_AREA_SEEN = stringSetPreferencesKey("areaPickerSeenCities")
        val KEY_EXPLICIT_PICK = booleanPreferencesKey("awaitingExplicitCityPick")
        val KEY_PENDING_DEEP_LINK = stringPreferencesKey("pendingDeepLink")
    }
}
