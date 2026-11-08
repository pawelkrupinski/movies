package pl.kinowo.ui

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.launch
import kotlinx.coroutines.withTimeoutOrNull
import pl.kinowo.data.UserPreferences
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.deeplink.DeepLinkFilters
import pl.kinowo.deeplink.DeepLinkTitle
import pl.kinowo.filter.FilterFacets
import pl.kinowo.filter.NameCount
import pl.kinowo.model.Catalog
import pl.kinowo.model.Country
import pl.kinowo.model.Film

/**
 * Lands an inbound App Link / kinowo:// link: switches the city (or hands a
 * link into another country to the ViewModel the country switch recreates),
 * writes its filters into [filters], and queues its film as [pendingFilmNav].
 * Built and held by [KinowoViewModel] over its scope. Mirrors iOS
 * `ContentView.consumeDeepLink`.
 */
class DeepLinkRouter(
    private val prefs: UserPreferences,
    private val countryCatalog: StateFlow<Catalog>,
    /** The city whose listing [films] holds — what a link waits on before matching its film. */
    private val loadedCity: Flow<String?>,
    private val films: StateFlow<List<Film>>,
    private val disabledCinemas: StateFlow<Set<String>>,
    private val filters: ListingFilterState,
    private val citySelection: CitySelection,
    private val scope: CoroutineScope,
) {
    /** A film title a deep link asked to open, once it's confirmed present in the
     *  loaded repertoire. [Repertoire] navigates to it then calls
     *  [clearPendingFilmNav]. Null when there's nothing pending. */
    var pendingFilmNav by mutableStateOf<String?>(null)
        private set

    fun clearPendingFilmNav() { pendingFilmNav = null }

    /**
     * Apply [rawUrl]. Switches the city eagerly (so a cold launch from a link
     * lands on the linked city), applies the scalar filters immediately, and
     * defers the film push + multi-value (exclusion / cinema) filters until the
     * repertoire loads — both need the loaded films (the value universe to
     * invert the link's inclusion lists, and the title to confirm before
     * navigating). A link into another country is handed over to the ViewModel
     * the country switch recreates.
     */
    fun handleDeepLink(rawUrl: String) = scope.launch {
        // Parse against the LIVE catalog's slugs (like iOS `catalog.allSlugs`), so
        // a city that ships only via `/api/catalog` — every German city — is
        // recognised, not just the compile-time `Cities.all` roster.
        val catalog = countryCatalog.value
        val link = DeepLink.parse(rawUrl, cities = catalog.cities, bareSlugOwners = catalog.bareSlugOwners,
                                  versionTokensOfCountry = catalog::versionTokensOfCountry,
                                  genreSpellings = catalog.genreSpellings) ?: return@launch
        // The STORED choices, not the ViewModel's `stateIn` mirrors, which
        // still read null until DataStore's first emission lands.
        val (storedCountry, storedCity) = prefs.countryAndCity.first()
        val current = Country.byCode(storedCountry).code
        val target = link.countryCode
        citySelection.dismissCitySwitch()
        if (target != current) {
            // Another country's deployment: switch country and city in ONE
            // write (see [CitySelection.adoptDetectedCity]) and hand the link itself
            // over in that same write. MainActivity clears the ViewModel and
            // recreates the instant the country changes, so nothing here survives;
            // the fresh ViewModel applies the pending link from its `start`.
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
        withTimeoutOrNull(10_000) { loadedCity.first { it == link.citySlug } }
        applyRepertoireDependent(link, films.value)
    }

    @androidx.annotation.VisibleForTesting
    internal fun applyScalarFilters(linked: DeepLinkFilters) = filters.applyScalars(linked)

    @androidx.annotation.VisibleForTesting
    internal fun applyRepertoireDependent(link: DeepLink, loaded: List<Film>) {
        val f = link.filters
        fun universe(names: List<NameCount>) = names.map { it.name }.toSet()
        if (f.includedCountries.isNotEmpty()) filters.excludedCountries = f.excluded(f.includedCountries, universe(FilterFacets.allCountries(loaded)))
        if (f.includedGenres.isNotEmpty()) filters.excludedGenres = f.excluded(f.includedGenres, universe(FilterFacets.allGenres(loaded)))
        if (f.includedDirectors.isNotEmpty()) filters.excludedDirectors = f.excluded(f.includedDirectors, universe(FilterFacets.allDirectors(loaded)))
        if (f.includedCast.isNotEmpty()) filters.excludedCast = f.excluded(f.includedCast, universe(FilterFacets.allCast(loaded)))
        // Cinemas are a single global set across cities; re-derive only the ones
        // in THIS city, preserving deselections elsewhere (CinemaCityFilter).
        val cityCinemas = FilterFacets.allCinemas(loaded)
        f.disabledCinemas(cityCinemas.toSet())?.let { disabledHere ->
            val next = (disabledCinemas.value - cityCinemas.toSet()) + disabledHere
            scope.launch { prefs.setDisabledCinemas(next) }
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
}
