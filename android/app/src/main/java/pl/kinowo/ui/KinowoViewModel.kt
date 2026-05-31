package pl.kinowo.ui

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import kotlinx.coroutines.flow.SharingStarted
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.stateIn
import kotlinx.coroutines.launch
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.filter.CinemaSection
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.filteredFor
import pl.kinowo.filter.groupedByCinema
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
    private val prefs: UserPreferences,
) : ViewModel() {

    val films: StateFlow<List<Film>> = repo.films
    val isLoading: StateFlow<Boolean> = repo.isLoading
    val error: StateFlow<String?> = repo.error

    val hiddenFilms: StateFlow<Set<String>> =
        prefs.hiddenFilms.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())
    val disabledCinemas: StateFlow<Set<String>> =
        prefs.disabledCinemas.stateIn(viewModelScope, SharingStarted.Eagerly, emptySet())

    // Per-screen filter axes (Compose state).
    var dateFilter by mutableStateOf<DateFilter>(DateFilter.Today)
    var formatFilter by mutableStateOf(FormatFilter.EMPTY)
    var search by mutableStateOf("")
    var pinnedCinema by mutableStateOf<String?>(null)
    var excludedCountries by mutableStateOf<Set<String>>(emptySet())
    var excludedDirectors by mutableStateOf<Set<String>>(emptySet())
    var excludedCast by mutableStateOf<Set<String>>(emptySet())

    val filtersActive: Boolean
        get() = !formatFilter.isEmpty ||
            disabledCinemas.value.isNotEmpty() ||
            hiddenFilms.value.isNotEmpty() ||
            excludedCountries.isNotEmpty() ||
            excludedDirectors.isNotEmpty() ||
            excludedCast.isNotEmpty()

    fun filmsForFilmsTab(all: List<Film>): List<Film> = all.filteredFor(
        date = dateFilter,
        format = formatFilter,
        query = search,
        hidden = hiddenFilms.value,
        disabledCinemas = disabledCinemas.value,
        excludedCountries = excludedCountries,
        excludedDirectors = excludedDirectors,
        excludedCast = excludedCast,
    )

    fun cinemaSections(all: List<Film>): List<CinemaSection> {
        // Web's /kina ignores the persistent disabledCinemas set — pinning one
        // cinema is equivalent to disabling every other; no pin shows all.
        val disabled = pinnedCinema?.let { pin -> allCinemas(all).toSet() - pin } ?: emptySet()
        return all.filteredFor(
            date = dateFilter,
            format = formatFilter,
            query = search,
            hidden = hiddenFilms.value,
            disabledCinemas = disabled,
            excludedCountries = excludedCountries,
            excludedDirectors = excludedDirectors,
            excludedCast = excludedCast,
        ).groupedByCinema()
    }

    /** Distinct cinema names anywhere in the payload, sorted by pill name. */
    fun allCinemas(all: List<Film>): List<String> {
        val seen = LinkedHashSet<String>()
        for (film in all) for (day in film.showings) for (c in day.cinemas) seen.add(c.cinema)
        return seen.sortedBy { CinemaSection.pillName(it) }
    }

    fun allCountries(all: List<Film>): List<NameCount> = countBy(all) { it.countries }
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
        excludedDirectors = emptySet()
        excludedCast = emptySet()
        viewModelScope.launch { prefs.setDisabledCinemas(emptySet()) }
    }

    // ── lifecycle / data ──────────────────────────────────────────────────
    fun start() {
        repo.loadCachedData()
        repo.pruneStaleShowings()
        viewModelScope.launch { repo.reload() }
    }

    fun reload() = viewModelScope.launch { repo.reload() }

    fun onResume() {
        repo.pruneStaleShowings()
        viewModelScope.launch { repo.reloadIfStale() }
    }

    // ── prefs mutations ───────────────────────────────────────────────────
    fun hide(title: String) = viewModelScope.launch { prefs.hide(title) }
    fun unhide(title: String) = viewModelScope.launch { prefs.unhide(title) }
    fun unhideAll() = viewModelScope.launch { prefs.unhideAll() }
    fun toggleCinema(cinema: String, disabled: Boolean) =
        viewModelScope.launch { prefs.toggleCinema(cinema, disabled) }
    fun setDisabledCinemas(set: Set<String>) =
        viewModelScope.launch { prefs.setDisabledCinemas(set) }

    fun filmByTitle(title: String): Film? = films.value.firstOrNull { it.title == title }

    class Factory(
        private val repo: RepertoireRepository,
        private val prefs: UserPreferences,
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T =
            KinowoViewModel(repo, prefs) as T
    }
}
