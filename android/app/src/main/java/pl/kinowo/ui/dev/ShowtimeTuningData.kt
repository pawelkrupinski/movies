package pl.kinowo.ui.dev

import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import kotlinx.coroutines.launch
import pl.kinowo.filter.CinemaSection
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.model.Ratings
import pl.kinowo.model.Showtime

/**
 * Deterministic sample repertoire — identical every launch. Films 1 & 2
 * exercise every showtime-rendering case; films 3–6 are ordinary so the grid
 * reads like a real screen. [sections] regroups them by cinema for the Kina
 * page; [detailFilm] (+ [detailDetails]) feeds the Film page. Mirrors iOS
 * `TuningSampleData`.
 */
object ShowtimeTuningData {
    val films: List<Film> = listOf(edgeCases(), manyShowtimes(), drama(), kids(), lateNight(), festival())

    /** Cinema-grouped view of the sample films for the Kina page. Uses real
     *  cinema display names so `CinemaSection.pillName` shortens them the way
     *  the live Kina tab does. */
    val sections: List<CinemaSection> = listOf(
        CinemaSection("Cinema City Kinepolis", listOf(films[0], films[2], films[5])),
        CinemaSection("Multikino Stary Browar", listOf(films[1], films[3])),
        CinemaSection("Helios Posnania", listOf(films[4])),
    )

    /** One rich film for the Film-detail page — genres / directors / cast /
     *  countries populated (the listing samples leave them empty) so every meta
     *  block renders and its fonts are tunable. */
    val detailFilm: Film = Film(
        title = "Wszystkie przypadki",
        runtimeMinutes = 128,
        releaseYear = 2025,
        genres = listOf("Dramat", "Thriller"),
        ratings = ratings(7.8, 81, 92, 7.4),
        countries = listOf("Polska", "Francja"),
        directors = listOf("Jan Kowalski"),
        cast = listOf("Anna Nowak", "Piotr Wiśniewski", "Maria Lewandowska"),
        showings = edgeCases().showings,
    )

    /** Synopsis for [detailFilm] (matched by title). No trailers — the tuning
     *  preview keeps the embedded-player WebView out of the picture. */
    val detailDetails: FilmDetails = FilmDetails(
        title = "Wszystkie przypadki",
        originalTitle = "All The Cases",
        synopsis = "Wciągający dramat o splątanych losach kilku rodzin w powojennej Europie. " +
            "Reżyser prowadzi widza przez kolejne odsłony tajemnicy, stopniowo odsłaniając, " +
            "jak jeden wybór potrafi zaważyć na życiu wszystkich bohaterów. Tekst celowo długi, " +
            "by dało się dostroić rozmiar i odstępy opisu na ekranie szczegółów.",
    )

    private fun ratings(imdb: Double?, mc: Int?, rt: Int?, fw: Double?) =
        Ratings(imdb = imdb, metascore = mc, rottenTomatoes = rt, filmweb = fw)

    private fun st(time: String, format: String = "", room: String? = null) =
        Showtime(time = time, format = format, room = room, bookingURL = null)

    // 1 — every case: empty / short / long / multi-token formats, a room
    // (long-press tooltip), common-token stripping (Multikino's showtimes all
    // share "2D NAP" → rendered bare), two days, two cinemas.
    private fun edgeCases() = Film(
        title = "1 — Wszystkie przypadki",
        runtimeMinutes = 128,
        releaseYear = 2025,
        ratings = ratings(7.8, 81, 92, 7.4),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino Pod Baranami", cinemaURL = "https://example.com",
                        showtimes = listOf(
                            st("10:00"),
                            st("12:30", "2D"),
                            st("14:45", "2D DUB", room = "Sala 1"),
                            st("18:00", "IMAX 3D NAP", room = "IMAX"),
                            st("20:15", "3D NAP"),
                            st("22:30", "2D NAP"),
                            st("23:59", "4DX 3D DUB"),
                        ),
                    ),
                    CinemaShowings(
                        cinema = "Multikino",
                        showtimes = listOf(st("11:00", "2D NAP"), st("13:00", "2D NAP"), st("15:00", "2D NAP")),
                    ),
                ),
            ),
            DayShowings(
                date = "2026-06-09", label = "Wtorek 9 czerwca",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino Pod Baranami",
                        showtimes = listOf(st("16:00", "2D"), st("19:30", "VOSE")),
                    ),
                ),
            ),
        ),
    )

    // 2 — many showtimes → wraps across several rows.
    private fun manyShowtimes() = Film(
        title = "2 — Dużo seansów",
        runtimeMinutes = 95,
        releaseYear = 2026,
        ratings = ratings(6.2, null, 55, null),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Cinema City",
                        showtimes = listOf(
                            st("09:15", "2D"), st("10:30", "2D DUB"), st("11:45", "3D"), st("13:00", "2D NAP"),
                            st("14:15", "2D"), st("15:30", "3D DUB"), st("16:45", "2D"), st("18:00", "IMAX 2D"),
                            st("19:15", "2D NAP"), st("20:30", "3D NAP"), st("21:45", "2D"), st("23:00", "2D DUB"),
                        ),
                    ),
                ),
            ),
        ),
    )

    private fun drama() = Film(
        title = "3 — Dramat", runtimeMinutes = 142, releaseYear = 2024,
        ratings = ratings(8.1, 88, null, 7.9),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Kino Pod Baranami", showtimes = listOf(st("17:00", "2D NAP"), st("20:00", "2D NAP"))),
                ),
            ),
        ),
    )

    private fun kids() = Film(
        title = "4 — Dla dzieci", runtimeMinutes = 88, releaseYear = 2026,
        ratings = ratings(null, null, 78, 6.8),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Multikino", showtimes = listOf(st("10:00", "2D DUB"), st("12:00", "2D DUB"), st("14:00", "3D DUB"))),
                ),
            ),
        ),
    )

    private fun lateNight() = Film(
        title = "5 — Nocny seans", runtimeMinutes = 116, releaseYear = 2025,
        ratings = ratings(7.0, 64, 71, null),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Cinema City", showtimes = listOf(st("22:00", "2D NAP"), st("23:30", "2D NAP", room = "Sala 7"))),
                ),
            ),
        ),
    )

    private fun festival() = Film(
        title = "6 — Festiwal", runtimeMinutes = 101, releaseYear = 2023,
        ratings = ratings(7.5, null, null, 8.2),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek 8 czerwca",
                cinemas = listOf(
                    CinemaShowings("Kino Pod Baranami", showtimes = listOf(st("15:00", "VOSE"), st("18:30", "OV"), st("21:00", "VOSE"))),
                ),
            ),
        ),
    )
}
