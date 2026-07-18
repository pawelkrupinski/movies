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
        /**
         * Long cinema name → short pill label, one entry per cinema across every
         * city — mirrors the web's `Cinema.pillMap` (same city grouping/order) so
         * a pill reads identically on every platform. A name absent here falls
         * back to itself via [pillName].
         */
        val pillNames: Map<String, String> = mapOf(
            // Poznań
            "Kino Apollo" to "Apollo",
            "Kino Bułgarska 19" to "Bułgarska 19",
            "Kino Malta Charlie Monroe" to "Malta Charlie Monroe",
            "Helios Posnania" to "Helios",
            "Cinema City Kinepolis" to "Kinepolis",
            "Kino Muza" to "Muza",
            "Multikino Stary Browar" to "Multikino",
            "Kino Pałacowe" to "Pałacowe",
            "Cinema City Poznań Plaza" to "Poznań Plaza",
            "Kino Rialto" to "Rialto",
            // Wrocław
            "Cinema City Wroclavia" to "Wroclavia",
            "Cinema City Korona" to "Korona",
            "Multikino Pasaż Grunwaldzki" to "Pasaż Grunwaldzki",
            "Helios Magnolia Park" to "Magnolia",
            "Helios Aleja Bielany" to "Aleja Bielany",
            "Kino Nowe Horyzonty" to "Nowe Horyzonty",
            "Dolnośląskie Centrum Filmowe" to "DCF",
            // Warszawa
            "Cinema City Arkadia" to "Arkadia",
            "Cinema City Bemowo" to "Bemowo",
            "Cinema City Galeria Północna" to "Galeria Północna",
            "Cinema City Janki" to "Janki",
            "Cinema City Mokotów" to "Mokotów",
            "Cinema City Promenada" to "Promenada",
            "Cinema City Sadyba" to "Sadyba",
            "Multikino Złote Tarasy" to "Złote Tarasy",
            "Multikino Młociny" to "Młociny",
            "Multikino Reduta" to "Reduta",
            "Multikino Targówek" to "Targówek",
            "Multikino Wola Park" to "Wola Park",
            "Helios Blue City" to "Blue City",
            "Kino Muranów" to "Muranów",
            "Kino Luna" to "Luna",
            "Kino Elektronik" to "Elektronik",
            "Kino Iluzjon" to "Iluzjon",
            "KinoGram" to "KinoGram",
            "Kino Kultura" to "Kultura",
            "Kino Amondo" to "Amondo",
            "Kino na Boku" to "na Boku",
            "Kino Głębocka 66" to "Głębocka 66",
            "KINOMUZEUM" to "Kinomuzeum",
            "Kino Świt" to "Świt",
            "Kino Kępa" to "Kępa",
            "KINOkawiarnia Stacja Falenica" to "Stacja Falenica",
            "Służewski Dom Kultury" to "SDK",
            "Kino Atlantic" to "Atlantic",
            "Kinoteka" to "Kinoteka",
            "Kino U-jazdowski" to "U-jazdowski",
            "Kino Cytadela" to "Cytadela",
            // Kraków
            "Cinema City Bonarka" to "Bonarka",
            "Cinema City Kazimierz" to "Kazimierz",
            "Cinema City Zakopianka" to "Zakopianka",
            "Multikino Kraków" to "Multikino",
            "Kino Mikro" to "Mikro",
            "Mikro Bronowice" to "Mikro Bronowice",
            "Kino Sfinks" to "Sfinks",
            // Łódź
            "Cinema City Manufaktura" to "Manufaktura",
            "Multikino Łódź" to "Multikino",
            "Helios Łódź" to "Helios",
            "Kino Charlie" to "Charlie",
            // Katowice
            "Cinema City Punkt 44" to "Punkt 44",
            "Cinema City Silesia" to "Silesia",
            "Multikino Katowice" to "Multikino",
            "Helios Katowice" to "Helios",
            "Kino Kosmos" to "Kosmos",
            "Kino Światowid" to "Światowid",
            // Szczecin
            "Helios Kupiec" to "Helios",
            "Multikino Szczecin" to "Multikino",
            "Kino Pionier 1907" to "Pionier",
            // Białystok
            "Helios Alfa" to "Alfa",
            "Helios Biała" to "Biała",
            "Helios Jurowiecka" to "Jurowiecka",
            "Kino Forum" to "Forum",
            // Trójmiasto (Gdańsk · Gdynia · Sopot)
            "Multikino Gdańsk" to "Multikino",
            "Helios Metropolia" to "Metropolia",
            "Helios Forum" to "Forum",
            "Helios Riviera" to "Riviera",
            "Kino Spektrum" to "Spektrum",
            "Kino Kameralne Cafe" to "Kameralne",
            "Kino IKM" to "IKM",
            "Kino Muzeum" to "Muzeum",
            "Kino Żak" to "Żak",
            "KinoPort" to "KinoPort",
            // Bydgoszcz
            "Cinema City Bydgoszcz" to "Cinema City",
            "Multikino Bydgoszcz" to "Multikino",
            "Helios Bydgoszcz" to "Helios",
            "Kino Orzeł" to "Orzeł",
            // Lublin
            "Cinema City Felicity" to "Felicity",
            "Cinema City Lublin Plaza" to "Lublin Plaza",
            "Multikino Lublin" to "Multikino",
            "Kino Bajka" to "Bajka",
            // Częstochowa
            "Cinema City Jurajska" to "Jurajska",
            "Cinema City Wolność" to "Wolność",
            // Radom
            "Helios Radom" to "Helios",
            "Multikino Radom" to "Multikino",
            // Sosnowiec
            "Helios Sosnowiec" to "Helios",
            "Cinema City Sosnowiec" to "Cinema City",
            // Toruń
            "Cinema City Czerwona Droga" to "Czerwona Droga",
            "Cinema City Toruń Plaza" to "Plaza",
            // Kielce
            "Helios Kielce" to "Helios",
            "Multikino Kielce" to "Multikino",
            // Rzeszów
            "Helios Rzeszów" to "Helios",
            "Multikino Rzeszów" to "Multikino",
            "Kino Zorza" to "Zorza",
            // Gliwice
            "Cinema City Gliwice" to "Cinema City",
            // Zabrze
            "Multikino Zabrze" to "Multikino",
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
): List<Film> {
    val trimmedQuery = query.trim().lowercase()
    return mapNotNull { film ->
        if (film.title in hidden) return@mapNotNull null
        if (trimmedQuery.isNotEmpty() && !film.title.lowercase().contains(trimmedQuery)) return@mapNotNull null
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
