package controllers

import java.time.LocalDate

case class TruncatedShowings(
  showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
  hiddenCount: Int
)

object ShowingsTruncation {
  private val PillsPerRow       = 6
  private val MinHiddenShowtimes = 3

  def truncate(
    allShowings: Seq[(LocalDate, Seq[CinemaShowtimes])],
    maxRows: Int,
    showCinemaHeaders: Boolean
  ): TruncatedShowings = {
    var lineCount = 0
    var hidden    = 0
    val visible   = scala.collection.mutable.ArrayBuffer.empty[(LocalDate, Seq[CinemaShowtimes])]
    var capped    = false

    for ((date, cinemas) <- allShowings) {
      if (capped) {
        for (cs <- cinemas) hidden += cs.showtimes.size
      } else {
        val keptCinemas = scala.collection.mutable.ArrayBuffer.empty[CinemaShowtimes]
        var dayLines    = 1 // date label

        for (cs <- cinemas) {
          if (capped) {
            hidden += cs.showtimes.size
          } else {
            val pillRows    = math.ceil(cs.showtimes.size.toDouble / PillsPerRow).toInt.max(1)
            val cinemaLines = (if (showCinemaHeaders) 1 else 0) + pillRows
            if (lineCount + dayLines + cinemaLines <= maxRows) {
              keptCinemas += cs
              dayLines += cinemaLines
            } else {
              hidden += cs.showtimes.size
              capped = true
            }
          }
        }

        if (keptCinemas.nonEmpty) {
          visible += ((date, keptCinemas.toSeq))
          lineCount += dayLines
        }
      }
    }

    if (hidden <= MinHiddenShowtimes) TruncatedShowings(allShowings, 0)
    else TruncatedShowings(visible.toSeq, hidden)
  }

  def showtimeNoun(n: Int): String = {
    if (n == 1) "seans"
    else {
      val mod10 = n % 10; val mod100 = n % 100
      if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) "seanse" else "seansów"
    }
  }
}
