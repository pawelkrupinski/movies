package models

import java.time.LocalDateTime

case class Showtime(
  dateTime:   LocalDateTime,
  bookingUrl: Option[String],
  room:       Option[String] = None
)
