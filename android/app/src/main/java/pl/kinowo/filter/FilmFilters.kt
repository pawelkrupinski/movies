package pl.kinowo.filter

import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import java.time.Instant
import java.time.ZoneId

/**
 * One cinema's slice of a filtered film list — every film that plays there,
 * with each film's showings restricted to this cinema. Drives the Kina tab's
 * cinema-grouped layout.
 */
data class CinemaSection(val cinema: String, val films: List<Film>) {
    val id: String get() = cinema

    companion object {
        /**
         * Long cinema name → short pill label, one entry per cinema across every
         * city — mirrors the web's `Cinema.pillMap` (same city grouping/order) so
         * a pill reads identically on every platform. A name absent here falls
         * back to itself via [pillName].
         */
        val pillNames: Map<String, String> = CinemaPillNames.all

        fun pillName(cinema: String): String = pillNames[cinema] ?: cinema
    }
}

/**
 * Drop screenings already in the past and re-sort cinemas by the earliest
 * remaining slot of the day. Mirrors the server's `toSchedules`:
 * showtimes `<= now - 30min` drop; within a day, cinemas order by their
 * earliest remaining slot; emptied cinema-groups / days / films are removed.
 *
 * The server prunes at request time, so a fresh payload is already pruned;
 * running this locally on foreground keeps cached data fresh as wall-clock
 * advances without a round-trip.
 */
fun List<Film>.prunedPastShowings(now: Instant = Instant.now(), zone: ZoneId = WARSAW): List<Film> =
    mapNotNull { film ->
        val days = film.showings.mapNotNull { day ->
            val kept = day.cinemas.mapNotNull { cg ->
                cg.keeping { ShowtimeClock.isFuture(it, day.date, now, zone) }
            }
            if (kept.isEmpty()) null
            else day.copy(
                cinemas = kept.sortedBy { ShowtimeClock.earliestMinutes(it) }
            )
        }
        film.narrowedTo(days)
    }

/** This film narrowed to [days] for a listing, or null when no day is left. */
private fun Film.narrowedTo(days: List<DayShowings>): Film? =
    if (days.isEmpty()) null else copy(showings = days)

/** This cinema-group with only the showtimes [isKept] passes, or null when none is left. */
private fun CinemaShowings.keeping(isKept: (Showtime) -> Boolean): CinemaShowings? =
    showtimes.filter(isKept).takeIf { it.isNotEmpty() }?.let { copy(showtimes = it) }

/**
 * The multi-value filter axes as EXCLUSION sets: a film is ruled out when every value it carries
 * on an axis is excluded. A film with no genre / director / cast listed is never ruled out by
 * that axis. Countries intentionally has NO isEmpty guard on the film (matches iOS): a film with
 * no countries is a subset of any non-empty exclusion set, so it drops as soon as any country is
 * excluded.
 */
private class ExcludedAxes(
    val countries: Set<String>,
    val genres: Set<String>,
    val directors: Set<String>,
    val cast: Set<String>,
) {
    fun rulesOut(film: Film): Boolean =
        (countries.isNotEmpty() && countries.containsAll(film.countries)) ||
            allExcluded(film.genres, genres) ||
            allExcluded(film.directors, directors) ||
            allExcluded(film.cast, cast)

    private fun allExcluded(values: List<String>, excluded: Set<String>): Boolean =
        excluded.isNotEmpty() && values.isNotEmpty() && excluded.containsAll(values)
}

/**
 * Apply the cross-screen filter stack — date / format / search / hidden /
 * excluded cinemas / excluded country|director|cast — to a film list.
 *
 * [disabledCinemas] is the one cinema axis, an EXCLUSION set mirroring the web's
 * `disabledCinemas`: any cinema-group whose name is in the set is dropped; empty
 * = every cinema shown. Both city shapes use it — a flat city ticks cinemas off
 * one by one in the Filtry sheet, a split city (e.g. London) by area — and it is
 * global across cities, so a stale name from another city simply never matches.
 *
 * Semantics mirror the web's `applyFilters()`: drop a showtime whose
 * format/time fails, drop a cinema-group that's excluded or whose every showtime
 * fell out, drop a day whose every cinema fell out, drop a film whose every day
 * fell out. A film stays visible while one badge somewhere still passes.
 */
fun List<Film>.filteredFor(
    date: DateFilter,
    format: FormatFilter,
    query: String,
    hidden: Set<String>,
    disabledCinemas: Set<String> = emptySet(),
    excludedCountries: Set<String> = emptySet(),
    excludedGenres: Set<String> = emptySet(),
    excludedDirectors: Set<String> = emptySet(),
    excludedCast: Set<String> = emptySet(),
    now: Instant = Instant.now(),
    zone: ZoneId = WARSAW,
): List<Film> {
    val trimmedQuery = query.trim().lowercase()
    val excluded = ExcludedAxes(excludedCountries, excludedGenres, excludedDirectors, excludedCast)
    return mapNotNull { film ->
        if (film.title in hidden) return@mapNotNull null
        if (trimmedQuery.isNotEmpty() && !film.matchesSearch(trimmedQuery)) return@mapNotNull null
        if (excluded.rulesOut(film)) return@mapNotNull null

        val days = film.showings.mapNotNull { day ->
            if (!date.matches(day.date, now, zone)) return@mapNotNull null
            val cinemas = day.cinemas.mapNotNull { cg ->
                if (cg.cinema in disabledCinemas) return@mapNotNull null
                cg.keeping { format.isEmpty || format.matches(it) }
            }
            if (cinemas.isEmpty()) null else day.copy(cinemas = cinemas)
        }
        film.narrowedTo(days)
    }
}

/**
 * Order a filtered film list for display, mirroring the web's `compareCards`:
 *
 *  - [SortOption.EARLIEST] — nearest upcoming showing first.
 *  - [SortOption.RATING]   — highest weighted rating first, ties broken by the
 *                            earliest showing.
 *
 * `sortedWith` is stable, so films that tie on every key keep their input
 * order — the same final fallback the web uses (`a.index - b.index`).
 */
fun List<Film>.sortedFor(sort: SortOption): List<Film> = when (sort) {
    SortOption.EARLIEST -> sortedBy { it.earliestShowing }
    SortOption.RATING -> sortedWith(
        compareByDescending<Film> { it.ratings.weighted }.thenBy { it.earliestShowing },
    )
}

/**
 * Pivot the cross-cinema film list into per-cinema sections. Each output
 * Film carries only the showings at its section's cinema (so dropping the
 * per-card cinema label is non-lossy). Sections sort alphabetically by pill
 * name to match the Kina pill row; films inside keep input order.
 */
fun List<Film>.groupedByCinema(): List<CinemaSection> {
    val perCinema = LinkedHashMap<String, MutableList<Film>>()
    for (film in this) {
        val seen = LinkedHashSet<String>()
        for (day in film.showings) for (cg in day.cinemas) seen.add(cg.cinema)
        for (cinema in seen) {
            val days = film.showings.mapNotNull { day ->
                val kept = day.cinemas.filter { it.cinema == cinema }
                if (kept.isEmpty()) null else day.copy(cinemas = kept)
            }
            film.narrowedTo(days)?.let { perCinema.getOrPut(cinema) { mutableListOf() }.add(it) }
        }
    }
    return perCinema.keys
        .sortedBy { CinemaSection.pillName(it) }
        .map { CinemaSection(it, perCinema.getValue(it)) }
}

/**
 * Which of a card's showtime-format tokens a chip may drop.
 *
 * Dropping what every slot shares is what keeps a chip narrow enough for
 * two-per-row (`ShowtimeChipFitTest`) — six chips all saying "2D" tell you
 * nothing, and neither do six all saying "NAP" when the film screens no other
 * way. What survives is what actually separates one slot from the next.
 *
 * The comparison spans the WHOLE card — every cinema, every day — because that
 * is the span a reader compares across. A film Multikino screens dubbed and
 * Helios subtitled is mixed even though neither cinema is, and both keep their
 * tag; a film that is dubbed everywhere shows bare times. On the Kina tab each
 * card already holds one cinema, so the same rule reads as per-cinema there.
 * Same rule as the web's `FilmFormat` and iOS's `FormatTokenFilter`.
 */
object FormatTokenFilter {
    /** The tokens that say a slot's dimension. IMAX, 4DX, ScreenX and the rest
     *  are projection systems the vocabulary pairs with either (`IMAX 3D`,
     *  `4DE 3D`), not dimensions of their own. */
    val DIMENSION_TOKENS: Set<String> = setOf("2D", "3D")

    /** The dimension of a slot that names none. */
    const val IMPLIED_DIMENSION = "2D"

    /** What a chip on this card may drop: the tokens EVERY showtime across
     *  [days] carries, whatever they name. A showtime naming no dimension
     *  counts as [IMPLIED_DIMENSION] (Kino Rialto writes `DUB` where Helios
     *  writes `2D DUB`); a format-less one sits out. Held to the vectors the
     *  web and iOS read too, `test/resources/format-token-strip.json`. */
    fun tokensToStrip(days: List<DayShowings>): Set<String> {
        val tokenSets = days
            .flatMap { it.cinemas }
            .flatMap { it.showtimes }
            .map { it.format.split(" ").filter(String::isNotEmpty).toSet() }
            .filter { it.isNotEmpty() }
            .map { if (it.none(DIMENSION_TOKENS::contains)) it + IMPLIED_DIMENSION else it }
        val first = tokenSets.firstOrNull() ?: return emptySet()
        return tokenSets.drop(1).fold(first) { acc, s -> acc.intersect(s) }
    }

    fun filter(format: String, common: Set<String>): String {
        if (common.isEmpty()) return format
        return format.split(" ").filter { it.isNotEmpty() && it !in common }.joinToString(" ")
    }
}
