package pl.kinowo.filter

import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.Film
import java.time.ZoneId

/** A name with how many films carry it — drives the country/director/cast lists. */
data class NameCount(val name: String, val count: Int)

/**
 * What the loaded repertoire offers the Filtry sheet — its cinemas and the four
 * name universes — and the listing it leaves for a set of filter axes. Pure:
 * every input is a parameter, so the screens and the deep-link router share one
 * rule and a test needs no ViewModel. iOS's `FilterFacets` / `ListingFilters`.
 */
object FilterFacets {
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

    /** How often each value appears across [all], most frequent first, ties in
     *  case-insensitive name order. */
    fun countBy(all: List<Film>, select: (Film) -> List<String>): List<NameCount> {
        val counts = HashMap<String, Int>()
        for (film in all) for (name in select(film)) counts[name] = (counts[name] ?: 0) + 1
        return counts.entries
            .map { NameCount(it.key, it.value) }
            .sortedWith(compareByDescending<NameCount> { it.count }.thenBy(String.CASE_INSENSITIVE_ORDER) { it.name })
    }

    /** The filtered, sorted listing of one [date] page. */
    fun filmsFor(
        all: List<Film>,
        date: DateFilter,
        axes: FilterAxes,
        hidden: Set<String>,
        disabledCinemas: Set<String>,
        zone: ZoneId,
    ): List<Film> =
        all.filteredFor(
            date = date,
            format = axes.format.applicable(all),
            query = axes.query,
            hidden = hidden,
            disabledCinemas = disabledCinemas,
            excludedCountries = axes.excludedCountries,
            excludedGenres = axes.excludedGenres,
            excludedDirectors = axes.excludedDirectors,
            excludedCast = axes.excludedCast,
            zone = zone,
        ).sortedFor(axes.sort)

    /**
     * Whether any filter narrows the listing — lights the Filtry icon.
     * [cityCinemas] scopes the cinema clause to the current city: the excluded
     * set is global, so a cinema unticked in another city lingers in it and must
     * not light the icon here (see [CinemaFilterSection.enabledCount]).
     */
    fun filtersActive(
        all: List<Film>,
        axes: FilterAxes,
        cityCinemas: List<String>,
        disabledCinemas: Set<String>,
        hidden: Set<String>,
    ): Boolean =
        !axes.format.applicable(all).isEmpty ||
            CinemaFilterSection(CinemaCatalog(cityCinemas, emptyList()), disabledCinemas)
                .let { it.enabledCount < it.cityCinemas.size } ||
            hidden.isNotEmpty() ||
            axes.excludedCountries.isNotEmpty() ||
            axes.excludedGenres.isNotEmpty() ||
            axes.excludedDirectors.isNotEmpty() ||
            axes.excludedCast.isNotEmpty()
}

/** One snapshot of the listing's per-screen filter axes, as [FilterFacets] reads them. */
data class FilterAxes(
    val format: FormatFilter = FormatFilter.EMPTY,
    val query: String = "",
    val sort: SortOption = SortOption.DEFAULT,
    val excludedCountries: Set<String> = emptySet(),
    val excludedGenres: Set<String> = emptySet(),
    val excludedDirectors: Set<String> = emptySet(),
    val excludedCast: Set<String> = emptySet(),
)
