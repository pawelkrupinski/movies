package pl.kinowo

import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Ratings
import pl.kinowo.model.Showtime
import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime

/** Shared readable builders for the filter/showtime test suites. */
object TestData {

    fun slot(time: String, format: String = "2D NAP", room: String? = null): Showtime =
        Showtime(time = time, format = format, room = room, bookingURL = null)

    fun cinema(name: String, times: List<Showtime>): CinemaShowings =
        CinemaShowings(cinema = name, cinemaURL = null, showtimes = times)

    fun day(date: String, cinemas: List<CinemaShowings>, label: String = date): DayShowings =
        DayShowings(date = date, label = label, cinemas = cinemas)

    fun film(
        title: String,
        days: List<DayShowings>,
        countries: List<String> = emptyList(),
        genres: List<String> = emptyList(),
        directors: List<String> = emptyList(),
        cast: List<String> = emptyList(),
        ratings: Ratings = Ratings.EMPTY,
        runtimeMinutes: Int = 100,
    ): Film =
        Film(
            title = title,
            posterURL = null,
            fallbackPosterURLs = emptyList(),
            runtimeMinutes = runtimeMinutes,
            genres = genres,
            ratings = ratings,
            countries = countries,
            directors = directors,
            cast = cast,
            showings = days,
        )

    /** A film with one day at one cinema: the smallest listing [pl.kinowo.ui.common.Showings] renders. */
    fun oneDayFilm(
        date: String,
        cinemaName: String,
        slots: List<Showtime>,
        label: String = date,
        title: String = "T",
    ): Film = film(title, listOf(day(date, listOf(cinema(cinemaName, slots)), label)))

    /** A wall-clock moment in Warsaw, as an [Instant]. */
    fun warsawInstant(
        year: Int, month: Int, day: Int, hour: Int = 12, minute: Int = 0,
    ): Instant =
        ZonedDateTime.of(year, month, day, hour, minute, 0, 0, ZoneId.of("Europe/Warsaw"))
            .toInstant()
}
