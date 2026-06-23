package pl.kinowo.ui

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.flow.SharingStarted
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.collectLatest
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filterNotNull
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.stateIn
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.coroutines.withTimeoutOrNull
import android.content.Context
import coil.annotation.ExperimentalCoilApi
import coil.imageLoader
import coil.memory.MemoryCache
import pl.kinowo.auth.AuthRepository
import pl.kinowo.data.PosterCachePurge
import pl.kinowo.auth.StateSyncService
import pl.kinowo.auth.UserProfile
import pl.kinowo.auth.UserStateClient
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.deeplink.DeepLinkFilters
import pl.kinowo.location.LocationCityResolver
import pl.kinowo.model.Cities
import pl.kinowo.model.CitySwitchSuggestion
import pl.kinowo.model.FilmDetails
import pl.kinowo.filter.CinemaCityFilter
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.filter.filteredFor
import pl.kinowo.filter.sortedFor
import pl.kinowo.model.Film

/** A name with how many films carry it — drives the country/director/cast lists. */
data class NameCount(val name: String, val count: Int)

/**
 * Single screen-state holder — the Android counterpart of iOS `ContentView`'s
 * `@State`. The repertoire payload + prefs arrive as flows from the
 * repository / DataStore; the per-screen filter axes live as Compose state
 * here so the UI recomposes on change. Derived lists (filtered films, cinema
 * sections, filter option lists) are computed on demand by the screens.
 */
class KinowoViewModel(
    private val repository: RepertoireRepository,
    private val detailsRepository: DetailsRepository,
    private val prefs: UserPreferences,
    private val authRepository: AuthRepository,
    userStateClient: UserStateClient,
) : ViewModel() {

    val films: StateFlow<List<Film>> = repository.films
    val isLoading: StateFlow<Boolean> = repository.isLoading
    val error: StateFlow<String?> = repository.error
    val details: StateFlow<Map<String, FilmDetails>> = detailsRepository.byTitle

    /** The signed-in user, or null when anonymous. Drives the Filtry → Konto UI. */
    val user: StateFlow<UserProfile?> = authRepository.user

    // Mirror prefs to the server while signed in. Constructed here so it shares
    // the ViewModel's scope; `start()` makes it observe the auth state.
    private val sync = StateSyncService(prefs, authRepository.user, userStateClient, viewModelScope)

    // Skips the one nearer-city check that the post-OAuth resume would otherwise
    // fire — armed when a web sign-in starts (see [signInWithGoogle]).
    private val citySwitchSuppressor = CitySwitchSuppressor()

    init {
        sync.start()
        // Re-hydrate a session persisted across launches (iOS does this in
        // `KinowoApp.task { await authService.checkSession() }`).
        viewModelScope.launch { authRepository.checkSession() }
    }

    /** Slug of the active city, or null until the first-launch gate resolves
     *  one. The repertoire/details fetch is gated on this being non-null. */
    val selectedCity: StateFlow<String?> =
        prefs.selectedCity.stateIn(viewModelScope, SharingStarted.Eagerly, null)

    val hiddenFilms: StateFlow<Set<String>> =
        prefs.hiddenFilms.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())
    val disabledCinemas: StateFlow<Set<String>> =
        prefs.disabledCinemas.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())

    // Per-screen filter axes (Compose state).
    /** A pending "you're nearer another city — switch?" prompt, or null. Set by
     *  [checkCitySwitch] when a granted-only location lands nearer a different
     *  supported city; cleared on accept ([setCity]) or decline
     *  ([dismissCitySwitch]). */
    var citySwitchSuggestion by mutableStateOf<CitySwitchSuggestion?>(null)
        private set

    var dateFilter by mutableStateOf<DateFilter>(DateFilter.Today)
    var sortBy by mutableStateOf(SortOption.DEFAULT)
    var formatFilter by mutableStateOf(FormatFilter.EMPTY)
    var search by mutableStateOf("")
    var excludedCountries by mutableStateOf<Set<String>>(emptySet())
    var excludedGenres by mutableStateOf<Set<String>>(emptySet())
    var excludedDirectors by mutableStateOf<Set<String>>(emptySet())
    var excludedCast by mutableStateOf<Set<String>>(emptySet())

    /** A film title a deep link asked to open, once it's confirmed present in the
     *  loaded repertoire. [Repertoire] navigates to it then calls
     *  [clearPendingFilmNav]. Null when there's nothing pending. */
    var pendingFilmNav by mutableStateOf<String?>(null)
        private set

    fun clearPendingFilmNav() { pendingFilmNav = null }

    // `allCinemas` of the current city is passed in so the cinema clause is
    // scoped to it — a cinema deselected in another city lingers in the global
    // set but must not light up the Filtry icon here. See [CinemaCityFilter].
    fun filtersActive(allCinemas: List<String>): Boolean =
        !formatFilter.isEmpty ||
            CinemaCityFilter.disabledIn(disabledCinemas.value, allCinemas).isNotEmpty() ||
            hiddenFilms.value.isNotEmpty() ||
            excludedCountries.isNotEmpty() ||
            excludedGenres.isNotEmpty() ||
            excludedDirectors.isNotEmpty() ||
            excludedCast.isNotEmpty()

    // `hidden`/`disabled` are passed in (not read off the StateFlow here) so the
    // caller composable observes them via collectAsState — reading them at the
    // call site is what makes Compose recompute the grid when a film is hidden
    // or a cinema toggled. Reading `.value` here would be invisible to Compose.
    fun filmsForFilmsTab(all: List<Film>, hidden: Set<String>, disabled: Set<String>): List<Film> =
        filmsFor(dateFilter, all, hidden, disabled)

    // The day-swipe carousel needs the listing for an ARBITRARY day preset (the
    // revealed previous/next neighbour), not just the selected one — so the date is
    // an explicit parameter here. [filmsForFilmsTab] is the special case of the
    // currently-selected day.
    fun filmsFor(date: DateFilter, all: List<Film>, hidden: Set<String>, disabled: Set<String>): List<Film> =
        all.filteredFor(
            date = date,
            format = formatFilter,
            query = search,
            hidden = hidden,
            disabledCinemas = disabled,
            excludedCountries = excludedCountries,
            excludedGenres = excludedGenres,
            excludedDirectors = excludedDirectors,
            excludedCast = excludedCast,
        ).sortedFor(sortBy)

    /** Distinct cinema names anywhere in the payload, sorted by pill name. */
    fun allCinemas(all: List<Film>): List<String> {
        val seen = LinkedHashSet<String>()
        for (film in all) for (day in film.showings) for (c in day.cinemas) seen.add(c.cinema)
        return seen.sortedBy { CinemaSection.pillName(it) }
    }

    fun allCountries(all: List<Film>): List<NameCount> = countBy(all) { it.countries }
    fun allGenres(all: List<Film>): List<NameCount> = countBy(all) { it.genres }
    fun allDirectors(all: List<Film>): List<NameCount> = countBy(all) { it.directors }
    fun allCast(all: List<Film>): List<NameCount> = countBy(all) { it.cast }

    private fun countBy(all: List<Film>, select: (Film) -> List<String>): List<NameCount> {
        val counts = HashMap<String, Int>()
        for (film in all) for (name in select(film)) counts[name] = (counts[name] ?: 0) + 1
        return counts.entries
            .map { NameCount(it.key, it.value) }
            .sortedWith(compareByDescending<NameCount> { it.count }.thenBy(String.CASE_INSENSITIVE_ORDER) { it.name })
    }

    fun clearFilters() {
        formatFilter = FormatFilter.EMPTY
        excludedCountries = emptySet()
        excludedGenres = emptySet()
        excludedDirectors = emptySet()
        excludedCast = emptySet()
        viewModelScope.launch { prefs.setDisabledCinemas(emptySet()) }
    }

    // ── lifecycle / data ──────────────────────────────────────────────────
    fun start() {
        repository.loadCachedData()
        detailsRepository.loadCachedData()
        repository.pruneStaleShowings()
        // The network fetch is gated on a city being chosen — until the
        // first-launch gate resolves one, `selectedCity` is null and nothing
        // hits the wire. Each distinct (non-null) slug triggers a fresh load,
        // so switching cities in the filters re-fetches that city's repertoire.
        viewModelScope.launch {
            selectedCity
                .filterNotNull()
                .distinctUntilChanged()
                .collectLatest { slug -> fetchAll(slug) }
        }
    }

    /** Listing + details fetched concurrently for [citySlug] — the grid paints
     *  as soon as the listing lands; details merge in when ready. */
    private suspend fun fetchAll(citySlug: String) = coroutineScope {
        val listing = async { repository.reload(citySlug) }
        val det = async { detailsRepository.reload(citySlug) }
        listing.await(); det.await()
    }

    fun reload() = viewModelScope.launch {
        selectedCity.value?.let { fetchAll(it) }
    }

    fun onResume() {
        repository.pruneStaleShowings()
        val slug = selectedCity.value ?: return
        viewModelScope.launch {
            coroutineScope {
                async { repository.reloadIfStale(slug) }
                async { detailsRepository.reloadIfStale(slug) }
            }
        }
    }

    /**
     * Once a day, after the repertoire has loaded, evict cached posters for
     * films that no longer have any future screening. [films] is already pruned
     * (server-side and by `prunedPastShowings`) to future-screening films, so
     * its poster URLs are exactly what's worth keeping; Coil's `DiskCache`
     * can't enumerate its keys, so we diff against the URL set persisted last
     * run and remove the ones that fell out. Mirrors iOS
     * `RepertoireStore.reconcilePostersIfNeeded`. Guarded on a non-empty list
     * so a failed cold load can't wipe the cache.
     */
    @OptIn(ExperimentalCoilApi::class) // ImageLoader.diskCache / memoryCache accessors
    fun purgePostersIfNeeded(context: Context, today: String) = viewModelScope.launch {
        val current = films.value
        if (current.isEmpty()) return@launch
        if (prefs.posterPurgeDate.first() == today) return@launch
        val keep = PosterCachePurge.keepUrls(current)
        val toEvict = PosterCachePurge.toEvict(prefs.seenPosterUrls.first(), keep)
        if (toEvict.isNotEmpty()) withContext(Dispatchers.IO) {
            val loader = context.applicationContext.imageLoader
            for (url in toEvict) {
                loader.diskCache?.remove(url)
                loader.memoryCache?.remove(MemoryCache.Key(url))
            }
        }
        prefs.setSeenPosterUrls(keep)
        prefs.setPosterPurgeDate(today)
    }

    // ── prefs mutations ───────────────────────────────────────────────────
    /**
     * One-shot read of the persisted swipe-hint state, evaluated against
     * `today` (`yyyy-MM-dd`). Reads straight from DataStore rather than a
     * cached StateFlow so the decision can't race the flow's initial value.
     */
    suspend fun shouldShowSwipeHint(today: String): Boolean = SwipeHint.shouldShow(
        hasSwiped = prefs.hasSwipedScreens.first(),
        lastShownDate = prefs.swipeHintShownDate.first(),
        today = today,
    )

    fun markSwiped() = viewModelScope.launch { prefs.markSwiped() }
    fun markSwipeHintShown(date: String) = viewModelScope.launch { prefs.markSwipeHintShown(date) }

    // ── deep links ────────────────────────────────────────────────────────
    /**
     * Apply an inbound App Link / kinowo:// link. Switches the city eagerly (so
     * a cold launch from a link lands on the linked city), applies the scalar
     * filters immediately, and defers the film push + multi-value (exclusion /
     * cinema) filters until the repertoire loads — both need the loaded films
     * (the value universe to invert the link's inclusion lists, and the title to
     * confirm before navigating). Mirrors iOS `ContentView.consumeDeepLink`.
     */
    fun handleDeepLink(rawUrl: String) {
        val link = DeepLink.parse(rawUrl) ?: return
        if (link.citySlug != selectedCity.value) setCity(link.citySlug)
        applyScalarFilters(link.filters)
        viewModelScope.launch {
            // Wait for the TARGET city's repertoire to be the loaded one — NOT
            // merely for `films` to be non-empty. On a warm app (or a cached
            // cold start) `films` still holds the PREVIOUS city's list, so
            // matching the deep-linked film against it misses and the film page
            // never opens — the bug MIUI hits every time, since it keeps the app
            // warm. Bounded so a film that genuinely left the repertoire, or a
            // city whose load fails, falls through to a no-op instead of hanging.
            withTimeoutOrNull(10_000) { repository.loadedCity.first { it == link.citySlug } }
            applyRepertoireDependent(link, films.value)
        }
    }

    @androidx.annotation.VisibleForTesting
    internal fun applyScalarFilters(filters: DeepLinkFilters) {
        filters.date?.let { dateFilter = it }
        formatFilter = filters.formatFilter(formatFilter)
        filters.query?.let { search = it }
        filters.sort?.let { sortBy = it }
    }

    @androidx.annotation.VisibleForTesting
    internal fun applyRepertoireDependent(link: DeepLink, loaded: List<Film>) {
        val f = link.filters
        if (f.includedCountries.isNotEmpty()) excludedCountries = f.excluded(f.includedCountries, allCountries(loaded).map { it.name }.toSet())
        if (f.includedGenres.isNotEmpty()) excludedGenres = f.excluded(f.includedGenres, allGenres(loaded).map { it.name }.toSet())
        if (f.includedDirectors.isNotEmpty()) excludedDirectors = f.excluded(f.includedDirectors, allDirectors(loaded).map { it.name }.toSet())
        if (f.includedCast.isNotEmpty()) excludedCast = f.excluded(f.includedCast, allCast(loaded).map { it.name }.toSet())
        // Cinemas are a single global set across cities; re-derive only the ones
        // in THIS city, preserving deselections elsewhere (CinemaCityFilter).
        val cityCinemas = allCinemas(loaded)
        f.disabledCinemas(cityCinemas.toSet())?.let { disabledHere ->
            setDisabledCinemas((disabledCinemas.value - cityCinemas.toSet()) + disabledHere)
        }
        // Only navigate to a film that's actually in the listing (one that left
        // the repertoire just no-ops, like iOS).
        link.filmTitle?.let { title -> if (loaded.any { it.title == title }) pendingFilmNav = title }
    }

    /** Persist the chosen city. `start()`'s `selectedCity` collector picks up
     *  the change and re-fetches that city's repertoire — no explicit reload.
     *  Also clears any pending switch prompt, since accepting one lands here. */
    fun setCity(slug: String) = viewModelScope.launch {
        citySwitchSuggestion = null
        prefs.setCity(slug)
    }

    /** Adopt a city the user deliberately picked at the first-launch gate. When
     *  it differs from the location-detected [nearestSlug], pre-record that pair
     *  so [checkCitySwitch] doesn't fire the "you're nearer …" prompt the moment
     *  the repertoire appears — the pick was intentional. Seeds the key before
     *  persisting the city so the prompt check sees it. */
    fun chooseCityAtGate(slug: String, nearestSlug: String?) = viewModelScope.launch {
        Cities.initialChoiceSuppressKey(slug, nearestSlug)?.let { prefs.setCitySwitchPromptKey(it) }
        citySwitchSuggestion = null
        prefs.setCity(slug)
    }

    /**
     * Offer to switch to a nearer supported city when the device — with location
     * already granted — is closer to a different city than the chosen one. Reads
     * a fix only if `ACCESS_COARSE_LOCATION` is granted (never prompts), then
     * defers to [Cities.switchSuggestion] for the once-per-pair decision. On a
     * hit, persists the pair key immediately so the prompt shows at most once per
     * pair regardless of accept/decline, and surfaces it as [citySwitchSuggestion]
     * for [KinowoApp] to render.
     */
    fun checkCitySwitch(context: Context) = viewModelScope.launch {
        // A web sign-in just returned via a Custom Tab resume — skip the one
        // check that would re-surface the prompt the user already answered.
        if (citySwitchSuppressor.consumeShouldSkip()) return@launch
        if (citySwitchSuggestion != null) return@launch
        val chosen = selectedCity.value ?: return@launch
        val fix = LocationCityResolver(context).resolveIfGranted() ?: return@launch
        val suggestion = Cities.switchSuggestion(
            chosenSlug = chosen,
            lat = fix.first,
            lon = fix.second,
            lastPromptKey = prefs.citySwitchPromptKey.first(),
        ) ?: return@launch
        prefs.setCitySwitchPromptKey(suggestion.key)
        citySwitchSuggestion = suggestion
    }

    /** Decline the nearer-city prompt — just clears the dialog; the pair key was
     *  already persisted in [checkCitySwitch], so we won't re-ask for it. */
    fun dismissCitySwitch() { citySwitchSuggestion = null }

    fun hide(title: String) = viewModelScope.launch { prefs.hide(title) }
    fun unhide(title: String) = viewModelScope.launch { prefs.unhide(title) }
    fun unhideAll() = viewModelScope.launch { prefs.unhideAll() }
    fun toggleCinema(cinema: String, disabled: Boolean) =
        viewModelScope.launch { prefs.toggleCinema(cinema, disabled) }
    fun setDisabledCinemas(set: Set<String>) =
        viewModelScope.launch { prefs.setDisabledCinemas(set) }

    /** Scoped "Wszystkie kina" select-all / deselect-all: flips only this
     *  city's cinemas, preserving deselections made in other cities. */
    fun setAllCinemas(allCinemas: List<String>, selected: Boolean) =
        setDisabledCinemas(CinemaCityFilter.afterToggleAll(disabledCinemas.value, allCinemas, selected))

    fun filmByTitle(title: String): Film? = films.value.firstOrNull { it.title == title }
    fun detailsByTitle(title: String): FilmDetails? = details.value[title]

    // ── auth ──────────────────────────────────────────────────────────────
    // Arm the suppressor before launching the Custom Tab: the resume when it
    // returns must not re-fire the nearer-city prompt the user just dealt with.
    fun signInWithGoogle(context: Context) {
        citySwitchSuppressor.suppressNextCheck()
        authRepository.startWebSignIn(context, "google")
    }

    fun signInWithFacebook(context: Context) {
        citySwitchSuppressor.suppressNextCheck()
        authRepository.startWebSignIn(context, "facebook")
    }

    /** Redeem the one-shot code delivered by the `kinowo://auth-done` deep link. */
    fun handleAuthRedirect(code: String) = viewModelScope.launch { authRepository.exchangeCode(code) }

    fun signOut() = viewModelScope.launch { authRepository.signOut() }

    /** Delete the account, then wipe local prefs — matches iOS, which clears
     *  hidden films + disabled cinemas after `deleteAccount()`. */
    fun deleteAccount() = viewModelScope.launch {
        authRepository.deleteAccount()
        prefs.unhideAll()
        prefs.setDisabledCinemas(emptySet())
    }

    class Factory(
        private val repository: RepertoireRepository,
        private val detailsRepository: DetailsRepository,
        private val prefs: UserPreferences,
        private val authRepository: AuthRepository,
        private val userStateClient: UserStateClient,
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            KinowoViewModel(repository, detailsRepository, prefs, authRepository, userStateClient) as T
    }
}
