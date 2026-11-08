package pl.kinowo.ui

import pl.kinowo.runCatchingCancellable
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.SharingStarted
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.collectLatest
import kotlinx.coroutines.flow.combine
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.flow.filterNotNull
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.stateIn
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import android.content.Context
import coil3.imageLoader
import coil3.memory.MemoryCache
import pl.kinowo.auth.AuthRepository
import pl.kinowo.data.PosterCachePurge
import pl.kinowo.auth.StateSync
import pl.kinowo.auth.StateSyncService
import pl.kinowo.auth.UserProfile
import pl.kinowo.auth.HiddenFilmsClient
import pl.kinowo.auth.LanguageClient
import pl.kinowo.data.CatalogRepository
import pl.kinowo.data.ClientSupportGate
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.City
import pl.kinowo.net.CinemaCatalogApi
import pl.kinowo.location.GrantedLocationSource
import pl.kinowo.model.CitySwitchSuggestion
import pl.kinowo.model.zoneFor
import pl.kinowo.model.currentSlugByFormer
import pl.kinowo.model.city
import pl.kinowo.model.Country
import pl.kinowo.model.LanguageDefault
import pl.kinowo.ui.city.CityGateStart
import pl.kinowo.model.selected
import pl.kinowo.model.FilmDetails
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.SearchDay
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Film
import pl.kinowo.filter.FilterFacets

/**
 * Single screen-state holder — the Android counterpart of iOS `ContentView`'s
 * `@State`. The repertoire payload + prefs arrive as flows from the
 * repository / DataStore; the per-screen filter axes live as Compose state in
 * [ListingFilterState]. Which city is shown is [CitySelection]'s, landing a
 * link [DeepLinkRouter]'s, and the derived lists (filtered films, filter option
 * lists) [FilterFacets]'; this class wires them over one scope and delegates.
 */
class KinowoViewModel(
    private val repository: RepertoireRepository,
    private val detailsRepository: DetailsRepository,
    private val prefs: UserPreferences,
    private val authRepository: AuthRepository,
    /** Mirrors prefs to the server while signed in; `init` makes it observe the
     *  auth state. Built by [Factory] over the same [scope]. */
    private val sync: StateSync,
    private val catalogApi: CinemaCatalogApi,
    private val catalogRepository: CatalogRepository,
    private val location: GrantedLocationSource,
    /** Whether this app version is still served; asked once in [start]. */
    private val clientSupportGate: ClientSupportGate,
    /** Becomes [viewModelScope] — cancelled when the ViewModel is cleared. Passed
     *  in so [Factory] can hand the same scope to [sync]. */
    scope: CoroutineScope,
) : ViewModel(scope) {

    /** The live country + city catalog (fetched on open, seeded from the distro).
     *  The pickers and the nearest-city gate read `countryCatalog.value.cities` /
     *  `.countries` and run the `List<City>`/`List<Country>` query extensions.
     *  (Distinct from `catalog` below, which is the per-city cinema universe.) */
    val countryCatalog: StateFlow<pl.kinowo.model.Catalog> = catalogRepository.catalog

    /** The store page to update from when the server no longer serves this app
     *  version, else null — [KinowoApp] then shows only the update screen. */
    val updateStoreUrl: StateFlow<String?> = clientSupportGate.updateStoreUrl

    val films: StateFlow<List<Film>> = repository.films
    val isLoading: StateFlow<Boolean> = repository.isLoading
    val error: StateFlow<String?> = repository.error
    val details: StateFlow<Map<String, FilmDetails>> = detailsRepository.byTitle

    /** The signed-in user, or null when anonymous. Drives the Filtry → Konto UI. */
    val user: StateFlow<UserProfile?> = authRepository.user

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

    /** ISO code of the selected country (see [pl.kinowo.model.Country]), or null
     *  until the user picks one — [pl.kinowo.model.Country.byCode] then treats
     *  null as the default (Poland). Persisting a new code re-points the API base
     *  URL and forces the country's UI language (MainActivity recreates on change). */
    val selectedCountryCode: StateFlow<String?> =
        prefs.selectedCountryCode.stateIn(viewModelScope, SharingStarted.Eagerly, null)

    /** Public origin of the country being browsed, for links we hand OUT (share,
     *  copy). `byCode` maps a null or unknown code onto Poland, the same default
     *  the API base uses, so this can never be empty. */
    fun shareOrigin(): String = Country.byCode(selectedCountryCode.value).baseUrl

    /** BCP-47 UI language tag, independent of [selectedCountryCode] (see
     *  [pl.kinowo.data.UserPreferences.selectedLanguageTag]). Never null: a
     *  missing pick resolves through [LanguageDefault] against the device's own
     *  locale/region — the [java.util.Locale] overload, since this ViewModel
     *  deliberately holds no Context reference (see [purgePostersIfNeeded] and
     *  friends, which take one per-call instead). [MainActivity] applies the
     *  same fallback (its Context-aware overload) at attach time. */
    val selectedLanguage: StateFlow<String> =
        prefs.selectedLanguageTag
            .map { it ?: LanguageDefault.resolve(java.util.Locale.getDefault()) }
            .stateIn(viewModelScope, SharingStarted.Eagerly, LanguageDefault.resolve(java.util.Locale.getDefault()))

    /** Persist the chosen language. The activity observes [pl.kinowo.data.UserPreferences.selectedLanguageTag]
     *  and recreates itself so the new locale takes effect — see MainActivity's
     *  language watcher for why that recreate() does NOT also clear the ViewModel. */
    fun setLanguage(tag: String) = viewModelScope.launch { prefs.setLanguageTag(tag) }

    /** Which city is shown and how it changes — the gate, a country switch,
     *  the nearer-city prompt. */
    private val citySelection = CitySelection(
        prefs, location, countryCatalog, selectedCity, selectedCountryCode, viewModelScope,
    )

    /** See [CitySelection.gateStart]. */
    val gateStart: StateFlow<CityGateStart?> = citySelection.gateStart

    fun setCountry(code: String) = citySelection.setCountry(code)
    fun pickAnotherCity() = citySelection.pickAnotherCity()

    val hiddenFilms: StateFlow<Set<String>> =
        prefs.hiddenFilms.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())
    val disabledCinemas: StateFlow<Set<String>> =
        prefs.disabledCinemas.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())

    /** The current city's cinema universe + area grouping (`/api/cinemas`).
     *  `EMPTY` (flat) until fetched. Drives the Filtry sheet's "Kina" section:
     *  a split city (London) renders `catalog.areas` as groups, a flat city one
     *  checkbox per cinema. */
    private val _catalog = MutableStateFlow(CinemaCatalog.EMPTY)
    val catalog: StateFlow<CinemaCatalog> = _catalog.asStateFlow()
    /** The city `_catalog` was fetched for — so the static catalog isn't
     *  re-fetched on every reload, only on an actual city switch. */
    private var catalogCitySlug: String? = null

    /** Split cities whose first-visit area picker the user has completed — the
     *  entry dialog shows once per city (never on a flat city). */
    val areaPickerSeenCities: StateFlow<Set<String>> =
        prefs.areaPickerSeenCities.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())

    /** See [CitySelection.citySwitchSuggestion]. */
    val citySwitchSuggestion: CitySwitchSuggestion? get() = citySelection.citySwitchSuggestion

    // Per-screen filter axes (Compose state), read and written by the screens.
    private val filters = ListingFilterState()
    var dateFilter: DateFilter by filters::dateFilter
    var sortBy: SortOption by filters::sortBy
    var formatFilter: FormatFilter by filters::formatFilter
    var search: String by filters::search
    val searchDay: SearchDay get() = filters.searchDay
    var excludedCountries: Set<String> by filters::excludedCountries
    var excludedGenres: Set<String> by filters::excludedGenres
    var excludedDirectors: Set<String> by filters::excludedDirectors
    var excludedCast: Set<String> by filters::excludedCast

    /** On the page for [page], the days the search brought in, drawn lighter; null when none is. */
    fun outsideDays(page: DateFilter): ((String) -> Boolean)? =
        // `now` as `filmsFor`'s: the filter's own default, read as the page is drawn.
        searchDay.outsideDays(page, zone = currentZone())

    /** Lands inbound links; see [DeepLinkRouter]. */
    internal val deepLinks = DeepLinkRouter(
        prefs, countryCatalog, repository.loadedCity, films, disabledCinemas, filters, citySelection, viewModelScope,
    )

    /** See [DeepLinkRouter.pendingFilmNav]. */
    val pendingFilmNav: String? get() = deepLinks.pendingFilmNav

    fun clearPendingFilmNav() = deepLinks.clearPendingFilmNav()

    /** Whether any filter narrows the listing — see [FilterFacets.filtersActive]. */
    fun filtersActive(cityCinemas: List<String>): Boolean = FilterFacets.filtersActive(
        films.value, filters.axes, cityCinemas, disabledCinemas.value, hiddenFilms.value,
    )

    // `hidden`/`disabledCinemas` are passed in (not read off the StateFlow here)
    // so the caller composable observes them via collectAsState — reading them at
    // the call site is what makes Compose recompute the grid when a film is hidden
    // or a cinema unticked. Reading `.value` here would be invisible to Compose. The
    // day-swipe carousel needs the listing for an ARBITRARY day preset (the
    // revealed previous/next neighbour), not just the selected one — so the date is
    // an explicit parameter here.
    fun filmsFor(
        date: DateFilter,
        all: List<Film>,
        hidden: Set<String>,
        disabledCinemas: Set<String> = emptySet(),
    ): List<Film> = FilterFacets.filmsFor(all, date, filters.axes, hidden, disabledCinemas, currentZone())

    /** The selected CITY's local zone — its own where the catalog gave it one,
     *  else its country's (live catalog entry when present, else the compile-time
     *  registry). Drives timezone-correct pruning and the Dziś/Jutro day buckets:
     *  a London show disappears on London time, and a Knoxville one on Eastern
     *  rather than on whichever single zone the US had to publish. */
    private fun currentZone(): java.time.ZoneId {
        val country = countryCatalog.value.countries.selected(selectedCountryCode.value)
        return countryCatalog.value.cities.zoneFor(selectedCity.value, country.code, country.zoneId)
    }

    fun clearFilters() {
        filters.clear()
        viewModelScope.launch { prefs.setDisabledCinemas(emptySet()) }
    }

    // ── lifecycle / data ──────────────────────────────────────────────────
    private var started = false

    /** Once per ViewModel. [KinowoApp] calls this from a `LaunchedEffect`,
     *  which re-runs whenever the activity is recreated around this RETAINED
     *  instance (rotation, a language switch) — a second `selectedCity`
     *  collector would fetch every city change twice. */
    fun start() {
        if (started) return
        started = true
        repository.loadCachedData()
        detailsRepository.loadCachedData()
        repository.pruneStaleShowings(zone = currentZone())
        // Revalidate the country/city catalog on cold open (conditional GET; a
        // 304 costs only headers). Non-blocking — the UI renders from the
        // seeded/persisted catalog meanwhile.
        viewModelScope.launch { catalogRepository.reload() }
        // Alongside it, ask whether this version is still served (best-effort:
        // any failure leaves the app usable).
        viewModelScope.launch { clientSupportGate.check() }
        // A saved city whose page was retired or renamed answers at another slug
        // now ([pl.kinowo.model.slugsLeadingHere]): its listing still arrives
        // through the server's 301, but the picker found no city by the old slug
        // and labelled the list with the country's default. Adopt the slug it
        // answers at, as the web's redirect does for a browser.
        viewModelScope.launch {
            combine(prefs.selectedCity, prefs.selectedCountryCode, countryCatalog) { slug, code, catalog ->
                val country = Country.byCode(code).code
                slug?.takeIf { catalog.cities.city(it, country) == null }?.let(catalog.cities.currentSlugByFormer(country)::get)
            }
                .filterNotNull()
                .collect { current -> prefs.setCity(current) }
        }
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
        // A cross-country deep link the previous ViewModel handed over (see
        // [DeepLinkRouter.handleDeepLink]) — its country and city are already stored.
        viewModelScope.launch { prefs.takePendingDeepLink()?.let { handleDeepLink(it) } }
    }

    /** Listing + details fetched concurrently for [citySlug] — the grid paints
     *  as soon as the listing lands; details merge in when ready. */
    private suspend fun fetchAll(citySlug: String) = coroutineScope {
        // Static per-city catalog (cinema universe + areas): fetch once per city,
        // best-effort (a failure leaves EMPTY = flat, so the pill-bar path works).
        if (catalogCitySlug != citySlug) {
            _catalog.value = CinemaCatalog.EMPTY   // don't show the old city's areas mid-switch
            launch {
                runCatchingCancellable { catalogApi.fetchCinemas(citySlug) }.getOrNull()?.let {
                    _catalog.value = it
                    catalogCitySlug = citySlug
                }
            }
        }
        val listing = async { repository.reload(citySlug) }
        val det = async { detailsRepository.reload(citySlug) }
        listing.await(); det.await()
    }

    fun reload() = viewModelScope.launch {
        selectedCity.value?.let { fetchAll(it) }
    }

    fun onResume() {
        repository.pruneStaleShowings(zone = currentZone())
        // Revalidate the catalog on each foreground (city-independent, so before
        // the early return below when no city is chosen yet).
        viewModelScope.launch { catalogRepository.reload() }
        // Foreground-resume reconcile: closes the gap `mergeWithServer` never had
        // a trigger for besides login — a hide made on another device/platform
        // while this app sat backgrounded is picked up the moment it's foregrounded
        // again, not just on the next cold start.
        viewModelScope.launch { sync.reconcileCurrentCountry() }
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
    /** Apply an inbound App Link / kinowo:// link — see [DeepLinkRouter.handleDeepLink]. */
    fun handleDeepLink(rawUrl: String) = deepLinks.handleDeepLink(rawUrl)

    // ── city ──────────────────────────────────────────────────────────────
    fun setCity(slug: String) = citySelection.setCity(slug)
    fun adoptDetectedCity(city: City) = citySelection.adoptDetectedCity(city)
    fun chooseCityAtGate(slug: String, nearestSlug: String?) = citySelection.chooseCityAtGate(slug, nearestSlug)
    fun checkCitySwitch() = citySelection.checkCitySwitch()
    fun dismissCitySwitch() = citySelection.dismissCitySwitch()

    fun hide(title: String) = viewModelScope.launch { sync.hide(prefs.hide(title), title) }
    fun unhide(title: String) = viewModelScope.launch { sync.unhide(prefs.unhide(title), title) }
    fun unhideAll() = viewModelScope.launch { sync.clear(prefs.unhideAll()) }
    /** Replace the excluded-cinemas set. The Filtry sheet's "Kina" section works
     *  out the new set via [pl.kinowo.filter.CinemaFilterSection] and hands it
     *  here; deep links write through the same path. */
    fun setDisabledCinemas(set: Set<String>) =
        viewModelScope.launch { prefs.setDisabledCinemas(set) }

    /** Complete the first-visit area picker for [citySlug]: keep only the areas
     *  in [keptAreaSlugs] (disable the rest's cinemas), then mark the city seen
     *  so the dialog doesn't reappear. Resets this city's slice first so a repeat
     *  reflects exactly the picked areas. */
    fun completeAreaPicker(citySlug: String, keptAreaSlugs: Set<String>) = viewModelScope.launch {
        val cat = catalog.value
        val rest = disabledCinemas.value - cat.cinemas.toSet()
        prefs.setDisabledCinemas(rest + cat.cinemasToDisable(keptAreaSlugs).toSet())
        prefs.markAreaPickerSeen(citySlug)
    }

    fun filmByTitle(title: String): Film? = films.value.firstOrNull { it.title == title }
    fun detailsByTitle(title: String): FilmDetails? = details.value[title]

    // ── auth ──────────────────────────────────────────────────────────────
    // Arm the suppressor before launching the Custom Tab: the resume when it
    // returns must not re-fire the nearer-city prompt the user just dealt with.
    fun signInWithGoogle(context: Context) {
        citySelection.suppressNextCheck()
        authRepository.startWebSignIn(context, "google")
    }

    fun signInWithFacebook(context: Context) {
        citySelection.suppressNextCheck()
        authRepository.startWebSignIn(context, "facebook")
    }

    /** Redeem the one-shot code delivered by the `kinowo://auth-done` deep link. */
    fun handleAuthRedirect(code: String) = viewModelScope.launch { authRepository.exchangeCode(code) }

    fun signOut() = viewModelScope.launch { authRepository.signOut() }

    /** Delete the account, then wipe local prefs — matches iOS, which clears
     *  hidden films (every country's) + disabled cinemas after `deleteAccount()`. */
    fun deleteAccount() = viewModelScope.launch {
        authRepository.deleteAccount()
        prefs.clearAllHiddenFilms()
        prefs.setDisabledCinemas(emptySet())
        // Otherwise a stale per-country migration flag survives into the next
        // login: reconcile would see "already migrated" for a country whose
        // server-side row no longer exists, skip the union, and read back an
        // empty hiddenFilms set as if it were authoritative.
        prefs.clearHiddenFilmsSyncState()
    }

    class Factory(
        private val repository: RepertoireRepository,
        private val detailsRepository: DetailsRepository,
        private val prefs: UserPreferences,
        internal val authRepository: AuthRepository,
        internal val hiddenFilmsClient: HiddenFilmsClient,
        internal val languageClient: LanguageClient,
        private val catalogApi: CinemaCatalogApi,
        private val catalogRepository: CatalogRepository,
        private val location: GrantedLocationSource,
        private val clientSupportGate: ClientSupportGate,
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T {
            // What the default `viewModelScope` would be; built here so the sync
            // service shares it and is cancelled with the ViewModel.
            val scope = CoroutineScope(SupervisorJob() + Dispatchers.Main.immediate)
            val sync = StateSyncService(prefs, authRepository.user, hiddenFilmsClient, languageClient, scope)
            return KinowoViewModel(
                repository, detailsRepository, prefs, authRepository, sync,
                catalogApi, catalogRepository, location, clientSupportGate, scope,
            ) as T
        }
    }
}
