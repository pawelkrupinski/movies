package pl.kinowo.ui

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import pl.kinowo.deeplink.DeepLinkFilters
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FilterAxes
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SearchDay
import pl.kinowo.filter.SortOption

/**
 * The listing's per-screen filter axes as Compose state — the day, the search
 * (and its [SearchDay]), sort, format and the four name exclusions — so the UI
 * recomposes on change. The Android counterpart of iOS `ContentView`'s filter
 * `@State`s. Held by [KinowoViewModel], written by the screens and by
 * [DeepLinkRouter].
 */
class ListingFilterState {
    private var shownDay by mutableStateOf<DateFilter>(DateFilter.Today)

    /** The listing's day. Set from outside (a pill tap, a swipe, a deep link) it is a pick, which
     *  outlasts a running search ([SearchDay]); the search's own moves go through [shownDay]. */
    var dateFilter: DateFilter
        get() = shownDay
        set(value) {
            if (value != shownDay) searchDay = searchDay.dayPicked()
            shownDay = value
        }
    var sortBy by mutableStateOf(SortOption.DEFAULT)
    var formatFilter by mutableStateOf(FormatFilter.EMPTY)
    private var searchText by mutableStateOf("")

    /** The search box. A search looks through every day ([SearchDay]): the first letter moves the
     *  listing to "All", and clearing it goes back to the day it began on, unless one was picked. */
    var search: String
        get() = searchText
        set(value) {
            searchText = value
            val change = searchDay.queryChanged(value, showing = shownDay)
            searchDay = change.next
            change.moveTo?.let { shownDay = it }
        }

    /** The search's day: what clearing the search goes back to, and which days it brought in. */
    var searchDay by mutableStateOf(SearchDay())
        private set

    var excludedCountries by mutableStateOf<Set<String>>(emptySet())
    var excludedGenres by mutableStateOf<Set<String>>(emptySet())
    var excludedDirectors by mutableStateOf<Set<String>>(emptySet())
    var excludedCast by mutableStateOf<Set<String>>(emptySet())

    /** The axes as [pl.kinowo.filter.FilterFacets] reads them. */
    val axes: FilterAxes
        get() = FilterAxes(
            format = formatFilter,
            query = search,
            sort = sortBy,
            excludedCountries = excludedCountries,
            excludedGenres = excludedGenres,
            excludedDirectors = excludedDirectors,
            excludedCast = excludedCast,
        )

    /** Reset format and the name exclusions (the Filtry sheet's Clear). */
    fun clear() {
        formatFilter = FormatFilter.EMPTY
        excludedCountries = emptySet()
        excludedGenres = emptySet()
        excludedDirectors = emptySet()
        excludedCast = emptySet()
    }

    /** A deep link's scalar axes, which need no repertoire. Absent axes are left alone. */
    fun applyScalars(filters: DeepLinkFilters) {
        filters.date?.let { dateFilter = it }
        formatFilter = filters.formatFilter(formatFilter)
        filters.query?.let { search = it }
        filters.sort?.let { sortBy = it }
    }
}
