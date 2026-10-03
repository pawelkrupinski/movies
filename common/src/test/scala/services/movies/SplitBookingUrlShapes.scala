package services.movies

import java.time.LocalDateTime

import models.Showtime

/** The stored shapes of a row whose booking URLs are split at the prefix they share — the row's
 *  `bookingUrlPrefix` and each showtime's `bookingUrlRest` — as the fields a `screenings` or a
 *  `web_screenings` row carries them in, beside the showtimes each must read back as. Shared by
 *  both rows' codec specs. */
object SplitBookingUrlShapes {
  private val at       = """{ "$date": "2026-12-17T13:00:00Z" }"""
  private val dateTime = LocalDateTime.of(2026, 12, 17, 13, 0)
  private val split    = s"""{ "dateTime": $at, "bookingUrlRest": "101", "room": "1" }, { "dateTime": $at, "bookingUrlRest": "2" }"""
  private val read     = Seq(
    Showtime(dateTime, Some("https://kino.example/buy?show=101"), Some("1")),
    Showtime(dateTime, Some("https://kino.example/buy?show=2")))

  /** (the row's prefix and showtimes fields, what its showtimes read as). */
  val rows: Seq[(String, Seq[Showtime])] = Seq(
    // The prefix before the showtimes, as a row is written.
    s""""bookingUrlPrefix": "https://kino.example/buy?show=", "showtimes": [$split]""" -> read,
    // After them, as a field a later update added lands.
    s""""showtimes": [$split], "bookingUrlPrefix": "https://kino.example/buy?show="""" -> read,
    // Beside a showtime stored whole, and one with no URL.
    s""""bookingUrlPrefix": "https://kino.example/buy?show=", "showtimes": [$split, { "dateTime": $at, "bookingUrl": "https://other.example/1" }, { "dateTime": $at }]""" ->
      (read ++ Seq(Showtime(dateTime, Some("https://other.example/1")), Showtime(dateTime, None))),
    // Remainders with no prefix to complete them: no URL, never a remainder posing as one.
    s""""showtimes": [$split]""" -> read.map(_.copy(bookingUrl = None)))
}
