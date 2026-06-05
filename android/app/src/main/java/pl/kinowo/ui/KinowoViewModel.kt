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
import pl.kinowo.model.FilmDetails
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.filter.filteredFor
import pl.kinowo.filter.groupedByCinema
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
    private val repo: RepertoireRepository,
    private val detailsRepo: DetailsRepository,
    private val prefs: UserPreferences,
    private val authRepo: AuthRepository,
    userStateClient: UserStateClient,
) : ViewModel() {

    val films: StateFlow<List<Film>> = repo.films
    val isLoading: StateFlow<Boolean> = repo.isLoading
    val error: StateFlow<String?> = repo.error
    val details: StateFlow<Map<String, FilmDetails>> = detailsRepo.byTitle

    /** The signed-in user, or null when anonymous. Drives the Filtry → Konto UI. */
    val user: StateFlow<UserProfile?> = authRepo.user

    // Mirror prefs to the server while signed in. Constructed here so it shares
    // the ViewModel's scope; `start()` makes it observe the auth state.
    private val sync = StateSyncService(prefs, authRepo.user, userStateClient, viewModelScope)

    init {
        sync.start()
        // Re-hydrate a session persisted across launches (iOS does this in
        // `KinowoApp.task { await authService.checkSession() }`).
        viewModelScope.launch { authRepo.checkSession() }
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
    var dateFilter by mutableStateOf<DateFilter>(DateFilter.Today)
    var sortBy by mutableStateOf(SortOption.DEFAULT)
    var formatFilter by mutableStateOf(FormatFilter.EMPTY)
    var search by mutableStateOf("")
    var pinnedCinema by mutableStateOf<String?>(null)
    var excludedCountries by mutableStateOf<Set<String>>(emptySet())
    var excludedGenres by mutableStateOf<Set<String>>(emptySet())
    var excludedDirectors by mutableStateOf<Set<String>>(emptySet())
    var excludedCast by mutableStateOf<Set<String>>(emptySet())

    val filtersActive: Boolean
        get() = !formatFilter.isEmpty ||
            disabledCinemas.value.isNotEmpty() ||
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
        all.filteredFor(
            date = dateFilter,
            format = formatFilter,
            query = search,
            hidden = hidden,
            disabledCinemas = disabled,
            excludedCountries = excludedCountries,
            excludedGenres = excludedGenres,
            excludedDirectors = excludedDirectors,
            excludedCast = excludedCast,
        ).sortedFor(sortBy)

    fun cinemaSections(all: List<Film>, hidden: Set<String>): List<CinemaSection> {
        // Web's /kina ignores the persistent disabledCinemas set — pinning one
        // cinema is equivalent to disabling every other; no pin shows all.
        val disabled = pinnedCinema?.let { pin -> allCinemas(all).toSet() - pin } ?: emptySet()
        return all.filteredFor(
            date = dateFilter,
            format = formatFilter,
            query = search,
            hidden = hidden,
            disabledCinemas = disabled,
            excludedCountries = excludedCountries,
            excludedDirectors = excludedDirectors,
            excludedCast = excludedCast,
        ).groupedByCinema()
            // Sort within each cinema: after grouping every film carries only
            // this cinema's showings, so `earliestShowing` is the per-cinema
            // nearest slot — the right key for the section's order.
            .map { it.copy(films = it.films.sortedFor(sortBy)) }
    }

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
        repo.loadCachedData()
        detailsRepo.loadCachedData()
        repo.pruneStaleShowings()
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
        val listing = async { repo.reload(citySlug) }
        val det = async { detailsRepo.reload(citySlug) }
        listing.await(); det.await()
    }

    fun reload() = viewModelScope.launch {
        selectedCity.value?.let { fetchAll(it) }
    }

    fun onResume() {
        repo.pruneStaleShowings()
        val slug = selectedCity.value ?: return
        viewModelScope.launch {
            coroutineScope {
                async { repo.reloadIfStale(slug) }
                async { detailsRepo.reloadIfStale(slug) }
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

    /** Persist the chosen city. `start()`'s `selectedCity` collector picks up
     *  the change and re-fetches that city's repertoire — no explicit reload. */
    fun setCity(slug: String) = viewModelScope.launch { prefs.setCity(slug) }

    fun hide(title: String) = viewModelScope.launch { prefs.hide(title) }
    fun unhide(title: String) = viewModelScope.launch { prefs.unhide(title) }
    fun unhideAll() = viewModelScope.launch { prefs.unhideAll() }
    fun toggleCinema(cinema: String, disabled: Boolean) =
        viewModelScope.launch { prefs.toggleCinema(cinema, disabled) }
    fun setDisabledCinemas(set: Set<String>) =
        viewModelScope.launch { prefs.setDisabledCinemas(set) }

    fun filmByTitle(title: String): Film? = films.value.firstOrNull { it.title == title }
    fun detailsByTitle(title: String): FilmDetails? = details.value[title]

    // ── auth ──────────────────────────────────────────────────────────────
    fun signInWithGoogle(context: Context) = authRepo.startWebSignIn(context, "google")
    fun signInWithFacebook(context: Context) = authRepo.startWebSignIn(context, "facebook")

    /** Redeem the one-shot code delivered by the `kinowo://auth-done` deep link. */
    fun handleAuthRedirect(code: String) = viewModelScope.launch { authRepo.exchangeCode(code) }

    fun signOut() = viewModelScope.launch { authRepo.signOut() }

    /** Delete the account, then wipe local prefs — matches iOS, which clears
     *  hidden films + disabled cinemas after `deleteAccount()`. */
    fun deleteAccount() = viewModelScope.launch {
        authRepo.deleteAccount()
        prefs.unhideAll()
        prefs.setDisabledCinemas(emptySet())
    }

    class Factory(
        private val repo: RepertoireRepository,
        private val detailsRepo: DetailsRepository,
        private val prefs: UserPreferences,
        private val authRepo: AuthRepository,
        private val userStateClient: UserStateClient,
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            KinowoViewModel(repo, detailsRepo, prefs, authRepo, userStateClient) as T
    }
}
