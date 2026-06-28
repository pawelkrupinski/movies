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
) {
  /** Is this showtime still worth showing at `now`? True until [[Showtime.Grace]]
   *  past its start, so a film whose screening just began still lists for a short
   *  window rather than vanishing mid-session. The single rule the web's
   *  `toSchedules` filters list views by and the worker's source-films gauge
   *  counts by — keeping the two apples-to-apples (no drift on the grace edge). */
  def isUpcoming(now: LocalDateTime): Boolean = dateTime.isAfter(now.minus(Showtime.Grace))
}

object Showtime {
  import java.time.Duration

  /** How long after a showtime starts it still counts as "upcoming" — a screening
   *  that began up to 30 min ago is still listed/counted, then drops. */
  val Grace: Duration = Duration.ofMinutes(30)
}
