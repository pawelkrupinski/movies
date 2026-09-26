package pl.kinowo.ui

import pl.kinowo.runCatchingCancellable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
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
import kotlinx.coroutines.withTimeoutOrNull
import android.content.Context
import coil.annotation.ExperimentalCoilApi
import coil.imageLoader
import coil.memory.MemoryCache
import pl.kinowo.auth.AuthRepository
import pl.kinowo.data.PosterCachePurge
import pl.kinowo.auth.StateSync
import pl.kinowo.auth.StateSyncService
import pl.kinowo.auth.UserProfile
import pl.kinowo.auth.HiddenFilmsClient
import pl.kinowo.auth.LanguageClient
import pl.kinowo.data.CatalogRepository
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.deeplink.DeepLinkFilters
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.City
import pl.kinowo.net.CinemaCatalogApi
import pl.kinowo.deeplink.DeepLinkTitle
import pl.kinowo.location.GrantedLocationSource
import pl.kinowo.model.Cities
import pl.kinowo.model.CitySwitchSuggestion
import pl.kinowo.model.zoneFor
import pl.kinowo.model.countryOf
import pl.kinowo.model.switchSuggestion
import pl.kinowo.model.Country
import pl.kinowo.model.LanguageDefault
import pl.kinowo.ui.city.CityGateStart
import pl.kinowo.model.selected
import pl.kinowo.model.FilmDetails
import pl.kinowo.filter.CinemaFilterSection
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
    /** Mirrors prefs to the server while signed in; `init` makes it observe the
     *  auth state. Built by [Factory] over the same [scope]. */
    private val sync: StateSync,
    private val catalogApi: CinemaCatalogApi,
    private val catalogRepository: CatalogRepository,
    private val location: GrantedLocationSource,
    /** Becomes [viewModelScope] — cancelled when the ViewModel is cleared. Passed
     *  in so [Factory] can hand the same scope to [sync]. */
    scope: CoroutineScope,
) : ViewModel(scope) {

    /** The live country + city catalog (fetched on open, seeded from the distro).
     *  The pickers and the nearest-city gate read `countryCatalog.value.cities` /
     *  `.countries` and run the `List<City>`/`List<Country>` query extensions.
     *  (Distinct from `catalog` below, which is the per-city cinema universe.) */
    val countryCatalog: StateFlow<pl.kinowo.model.Catalog> = catalogRepository.catalog

    val films: StateFlow<List<Film>> = repository.films
    val isLoading: StateFlow<Boolean> = repository.isLoading
    val error: StateFlow<String?> = repository.error
    val details: StateFlow<Map<String, FilmDetails>> = detailsRepository.byTitle

    /** The signed-in user, or null when anonymous. Drives the Filtry → Konto UI. */
    val user: StateFlow<UserProfile?> = authRepository.user

    // Skips the one nearer-city check that the post-OAuth resume would otherwise
    // fire — armed when a web sign-in starts (see [signInWithGoogle]) or a city
    // is re-picked with no detected nearest (see [chooseCityAtGate]).
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

    /** What the first-launch gate should do — which country, and whether it may
     *  offer a located city (see [CityGateStart]).
     *
     *  Null means "the stored choices have not been read yet" and NOTHING else —
     *  unlike [selectedCountryCode], whose null conflates that with "never
     *  chosen". The gate must wait for a non-null value instead of filling the
     *  gap itself: the DataStore reads are asynchronous, so on the first
     *  composition after a country switch nothing has landed, and defaulting
     *  there scopes the nearest-city search to Poland and greets a Berlin user
     *  with "You're near Poznan". */
    val gateStart: StateFlow<CityGateStart?> =
        combine(prefs.selectedCountryCode, prefs.awaitingExplicitCityPick) { code, explicit ->
            CityGateStart(
                countryCode = Country.normalizeCode(code) ?: Country.default.code,
                locate = !explicit,
            )
        }.stateIn(viewModelScope, SharingStarted.Eagerly, null)

    /** Persist the chosen country. The activity observes [selectedCountryCode] and
     *  recreates itself so the new base URL + locale take effect.
     *
     *  Each country is its own deployment serving a DISJOINT set of cities, so the
     *  switch also drops the current city: keeping the old
     *  country's slug would request it against the new country's host, and re-arming
     *  the city gate lets the user pick a city that actually exists there. Clearing
     *  the city before persisting the code means the gate is already re-armed by the
     *  time MainActivity recreates. Harmless at the first-launch gate, where the
     *  city is null anyway. */
    fun setCountry(code: String) = viewModelScope.launch {
        reArmCityGate()
        prefs.setCountryCode(code)
    }

    /** Drop the current city and arm the gate for an explicit pick, without
     *  touching the country. Backs the Filtry sheet's "Pick another city"
     *  button — the sole replacement for the old inline Kraj/Miasto pickers —
     *  which re-gates to [pl.kinowo.ui.city.CityChoiceScreen] the same way
     *  [setCountry] already did for a country switch. */
    fun pickAnotherCity() = viewModelScope.launch { reArmCityGate() }

    /** Clear the persisted city and ask the gate for an explicit pick rather
     *  than a located offer — shared by [setCountry] (a new country's
     *  deployment may not serve the old city) and [pickAnotherCity] (the user
     *  asked to switch city or country from within the app). */
    private suspend fun reArmCityGate() {
        prefs.clearCity()
        // The user just asked to pick, so let them: the re-armed gate offers
        // an explicit choice rather than whatever city the device happens to
        // sit near.
        prefs.awaitExplicitCityPick()
    }

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

    // The cinema pill row shows the current selection directly, so cinema
    // choice no longer feeds the Filtry icon — only the sheet's own axes do.
    // [cityCinemas] scopes the cinema clause to the current city — the excluded
    // set is global, so a cinema unticked in another city lingers in it and must
    // not light up the Filtry icon here (see CinemaFilterSection.enabledCount).
    fun filtersActive(cityCinemas: List<String>): Boolean =
        !formatFilter.applicable(films.value).isEmpty ||
            CinemaFilterSection(CinemaCatalog(cityCinemas, emptyList()), disabledCinemas.value)
                .let { it.enabledCount < it.cityCinemas.size } ||
            hiddenFilms.value.isNotEmpty() ||
            excludedCountries.isNotEmpty() ||
            excludedGenres.isNotEmpty() ||
            excludedDirectors.isNotEmpty() ||
            excludedCast.isNotEmpty()

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
    ): List<Film> =
        all.filteredFor(
            date = date,
            format = formatFilter.applicable(all),
            query = search,
            hidden = hidden,
            disabledCinemas = disabledCinemas,
            excludedCountries = excludedCountries,
            excludedGenres = excludedGenres,
            excludedDirectors = excludedDirectors,
            excludedCast = excludedCast,
            zone = currentZone(),
        ).sortedFor(sortBy)

    /** The selected CITY's local zone — its own where the catalog gave it one,
     *  else its country's (live catalog entry when present, else the compile-time
     *  registry). Drives timezone-correct pruning and the Dziś/Jutro day buckets:
     *  a London show disappears on London time, and a Knoxville one on Eastern
     *  rather than on whichever single zone the US had to publish. */
    private fun currentZone(): java.time.ZoneId {
        val country = countryCatalog.value.countries.selected(selectedCountryCode.value)
        return countryCatalog.value.cities.zoneFor(selectedCity.value, country.zoneId)
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
        // [handleDeepLink]) — its country and city are already stored.
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
     * confirm before navigating). A link into another country is handed over
     * to the ViewModel the country switch recreates. Mirrors iOS
     * `ContentView.consumeDeepLink`.
     */
    fun handleDeepLink(rawUrl: String) = viewModelScope.launch {
        // Parse against the LIVE catalog's slugs (like iOS `catalog.allSlugs`), so
        // a city that ships only via `/api/catalog` — every German city — is
        // recognised, not just the compile-time `Cities.all` roster.
        val catalog = countryCatalog.value
        val link = DeepLink.parse(rawUrl, catalog.cities.map { it.slug }.toSet(), catalog::versionTokensOf) ?: return@launch
        // The STORED choices, not this ViewModel's `stateIn` mirrors, which
        // still read null until DataStore's first emission lands.
        val (storedCountry, storedCity) = prefs.countryAndCity.first()
        val current = Country.byCode(storedCountry).code
        val target = catalog.cities.countryOf(link.citySlug) ?: current
        citySwitchSuggestion = null
        if (target != current) {
            // Another country's deployment: switch country and city in ONE
            // write (see [adoptDetectedCity]) and hand the link itself over in that
            // same write. MainActivity clears this ViewModel and recreates the
            // instant the country changes, so nothing here survives; the fresh
            // ViewModel applies the pending link from [start].
            prefs.setCityInCountry(link.citySlug, target, pendingDeepLink = rawUrl)
            return@launch
        }
        if (link.citySlug != storedCity) prefs.setCity(link.citySlug)
        applyScalarFilters(link.filters)
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
        // A slug link resolves exactly — the server minted both sides from the
        // same fold. A legacy `?title=` link matches the way the web's
        // MovieController.film does: by normalized title (Arabic→Roman fold),
        // NOT byte-for-byte, so a link to a numbered sequel ("…Prady 2") finds
        // the stored "…Prady II". Either way we navigate with the FOUND film's
        // real title, since the detail route looks films up by exact title. A
        // film that left the listing just no-ops, like iOS.
        val target = link.filmSlug?.let { slug -> loaded.firstOrNull { it.slug == slug } }
            ?: link.filmTitle?.let { title -> loaded.firstOrNull { DeepLinkTitle.matches(it.title, title) } }
        target?.let { pendingFilmNav = it.title }
    }

    /** Persist the chosen city. `start()`'s `selectedCity` collector picks up
     *  the change and re-fetches that city's repertoire — no explicit reload.
     *  Also clears any pending switch prompt, since accepting one lands here. */
    fun setCity(slug: String) = viewModelScope.launch {
        citySwitchSuggestion = null
        // The excluded-cinema set is global and scoped per city at read time, so
        // a new city needs no reset — its own cinemas simply start unticked-free.
        prefs.setCity(slug)
    }

    /** Adopt a location-detected [city] at the gate's confirm screen — the
     *  first-launch hit, or a manual "use my location" hit from the picker.
     *  The manual button now searches EVERY country, so [city] may sit in a
     *  country other than the one currently open; when it does, switch the
     *  country and set the city in ONE atomic write via `prefs.setCityInCountry`
     *  rather than [setCountry], which clears the city — as [handleDeepLink]
     *  does for a cross-country link: we're about to set this exact city, not
     *  re-arm the gate for a fresh pick. */
    fun adoptDetectedCity(city: City) = viewModelScope.launch {
        citySwitchSuggestion = null
        // Never two writes: MainActivity recreates the activity (cancelling
        // this coroutine) the instant the country pref changes, so a
        // `setCountryCode` followed by a `setCity` exposes the new country with
        // the old city and can lose the city to that teardown.
        if (city.country != selectedCountryCode.value) prefs.setCityInCountry(city.slug, city.country)
        else prefs.setCity(city.slug)
    }

    /** Adopt a city the user deliberately picked at the gate. When it differs
     *  from the location-detected [nearestSlug], pre-record that pair so
     *  [checkCitySwitch] doesn't fire the "you're nearer …" prompt the moment
     *  the repertoire appears — the pick was intentional. Seeds the key before
     *  persisting the city so the prompt check sees it.
     *
     *  [nearestSlug] is only ever non-null on the first-launch flow, which
     *  resolved a location fix before landing here. Reached any other way —
     *  Filtry's "Pick another city", or a country switch — there's no detected
     *  nearest to build a precise key from, so fall back to skipping the ONE
     *  check [checkCitySwitch] fires right after this pick (the same one-shot
     *  suppressor a web sign-in's Custom Tab resume uses): the pick was still
     *  deliberate, and a genuine later foreground still re-arms the check
     *  normally. */
    fun chooseCityAtGate(slug: String, nearestSlug: String?) = viewModelScope.launch {
        val key = Cities.initialChoiceSuppressKey(slug, nearestSlug)
        if (key != null) {
            prefs.setCitySwitchPromptKey(key)
        } else if (nearestSlug == null) {
            citySwitchSuppressor.suppressNextCheck()
        }
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
    fun checkCitySwitch() = viewModelScope.launch {
        // A web sign-in just returned via a Custom Tab resume — skip the one
        // check that would re-surface the prompt the user already answered.
        if (citySwitchSuppressor.consumeShouldSkip()) return@launch
        if (citySwitchSuggestion != null) return@launch
        val chosen = selectedCity.value ?: return@launch
        val fix = location.resolveIfGranted() ?: return@launch
        val suggestion = countryCatalog.value.cities.switchSuggestion(
            chosenSlug = chosen,
            lat = fix.first,
            lon = fix.second,
            lastPromptKey = prefs.citySwitchPromptKey.first(),
            countryCode = selectedCountryCode.value ?: Country.default.code,
        ) ?: return@launch
        prefs.setCitySwitchPromptKey(suggestion.key)
        citySwitchSuggestion = suggestion
    }

    /** Decline the nearer-city prompt — just clears the dialog; the pair key was
     *  already persisted in [checkCitySwitch], so we won't re-ask for it. */
    fun dismissCitySwitch() { citySwitchSuggestion = null }

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
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T {
            // What the default `viewModelScope` would be; built here so the sync
            // service shares it and is cancelled with the ViewModel.
            val scope = CoroutineScope(SupervisorJob() + Dispatchers.Main.immediate)
            val sync = StateSyncService(prefs, authRepository.user, hiddenFilmsClient, languageClient, scope)
            return KinowoViewModel(
                repository, detailsRepository, prefs, authRepository, sync,
                catalogApi, catalogRepository, location, scope,
            ) as T
        }
    }
}
