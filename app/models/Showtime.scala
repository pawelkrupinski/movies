package models

import java.time.LocalDateTime

case class Showtime(
  dateTime:   LocalDateTime,
  bookingUrl: Option[String],
  room:       Option[String] = None,
  // Format tokens, e.g. List("2D","NAP","ATMOS") or List("IMAX","2D"). Empty when unknown.
  // Stored token-wise (not as "2D/NAP/ATMOS") so different cinemas' separators
  // (Helios "/", Cinema City " ") don't leak into deduplication logic.
  format:     List[String] = Nil
)
