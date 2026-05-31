package pl.kinowo.filter

import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import java.time.Instant

/**
 * One cinema's slice of a filtered film list — every film that plays there,
 * with each film's showings restricted to this cinema. Drives the Kina tab's
 * cinema-grouped layout.
 */
data class CinemaSection(val cinema: String, val films: List<Film>) {
    val id: String get() = cinema

    companion object {
        /** Long cinema name → short pill label. */
        val pillNames: Map<String, String> = mapOf(
            "Cinema City Kinepolis" to "Kinepolis",
            "Cinema City Poznań Plaza" to "Poznań Plaza",
            "Helios Posnania" to "Helios",
            "Kino Apollo" to "Apollo",
            "Kino Bułgarska 19" to "Bułgarska 19",
            "Kino Malta Charlie Monroe" to "Malta Charlie Monroe",
            "Kino Muza" to "Muza",
            "Kino Pałacowe" to "Pałacowe",
            "Kino Rialto" to "Rialto",
            "Multikino Stary Browar" to "Multikino",
        )

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
fun List<Film>.prunedPastShowings(now: Instant = Instant.now()): List<Film> =
    mapNotNull { film ->
        val days = film.showings.mapNotNull { day ->
            val kept = day.cinemas.mapNotNull { cg ->
                val future = cg.showtimes.filter { ShowtimeClock.isFuture(it, day.date, now) }
                if (future.isEmpty()) null
                else cg.copy(showtimes = future)
            }
            if (kept.isEmpty()) null
            else day.copy(
                cinemas = kept.sortedBy { ShowtimeClock.earliestMinutes(it) }
            )
        }
        if (days.isEmpty()) null else film.copy(showings = days)
    }

/**
 * Apply the cross-screen filter stack — date / format / search / hidden /
 * per-cinema / excluded country|director|cast — to a film list. Both Filmy
 * and Kina go through this; they differ only in the `disabledCinemas` set.
 *
 * Semantics mirror the web's `applyFilters()`: drop a showtime whose
 * format/time fails, drop a cinema-group whose every showtime fell out, drop
 * a day whose every cinema fell out, drop a film whose every day fell out. A
 * film stays visible while one badge somewhere still passes.
 */
fun List<Film>.filteredFor(
    date: DateFilter,
    format: FormatFilter,
    query: String,
    hidden: Set<String>,
    disabledCinemas: Set<String>,
    excludedCountries: Set<String> = emptySet(),
    excludedGenres: Set<String> = emptySet(),
    excludedDirectors: Set<String> = emptySet(),
    excludedCast: Set<String> = emptySet(),
    now: Instant = Instant.now(),
): List<Film> {
    val q = query.trim().lowercase()
    return mapNotNull { film ->
        if (film.title in hidden) return@mapNotNull null
        if (q.isNotEmpty() && !film.title.lowercase().contains(q)) return@mapNotNull null
        // Countries intentionally has NO isEmpty guard (matches iOS): a film
        // with no countries is a subset of any non-empty exclusion set, so it
        // drops as soon as any country is excluded.
        if (excludedCountries.isNotEmpty() && excludedCountries.containsAll(film.countries))
            return@mapNotNull null
        if (excludedGenres.isNotEmpty() && film.genres.isNotEmpty() && excludedGenres.containsAll(film.genres))
            return@mapNotNull null
        if (excludedDirectors.isNotEmpty() && film.directors.isNotEmpty() && excludedDirectors.containsAll(film.directors))
            return@mapNotNull null
        if (excludedCast.isNotEmpty() && film.cast.isNotEmpty() && excludedCast.containsAll(film.cast))
            return@mapNotNull null

        val days = film.showings.mapNotNull { day ->
            if (!date.matches(day.date, now)) return@mapNotNull null
            val cinemas = day.cinemas.mapNotNull { cg ->
                if (cg.cinema in disabledCinemas) return@mapNotNull null
                val times = if (format.isEmpty) cg.showtimes
                else cg.showtimes.filter { format.matches(it) }
                if (times.isEmpty()) null else cg.copy(showtimes = times)
            }
            if (cinemas.isEmpty()) null else day.copy(cinemas = cinemas)
        }
        if (days.isEmpty()) null else film.copy(showings = days)
    }
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
            if (days.isEmpty()) continue
            perCinema.getOrPut(cinema) { mutableListOf() }.add(film.copy(showings = days))
        }
    }
    return perCinema.keys
        .sortedBy { CinemaSection.pillName(it) }
        .map { CinemaSection(it, perCinema.getValue(it)) }
}

/**
 * Tokens shared by every showtime of a cinema-group — used to drop a
 * redundant "2D" label when all of a cinema's slots are 2D.
 */
object FormatTokenFilter {
    fun commonTokens(cinema: CinemaShowings): Set<String> {
        val tokenSets = cinema.showtimes
            .map { it.format.split(" ").filter(String::isNotEmpty).toSet() }
            .filter { it.isNotEmpty() }
        val first = tokenSets.firstOrNull() ?: return emptySet()
        return tokenSets.drop(1).fold(first) { acc, s -> acc.intersect(s) }
    }

    fun filter(format: String, common: Set<String>): String {
        if (common.isEmpty()) return format
        return format.split(" ").filter { it.isNotEmpty() && it !in common }.joinToString(" ")
    }
}
