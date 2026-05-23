package controllers

import java.time.{LocalDate, LocalDateTime}
import models.{CinemaCityKinepolis, CinemaCityPoznanPlaza, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShowingsTruncationSpec extends AnyFlatSpec with Matchers {

  private val cinema1 = CinemaCityKinepolis
  private val cinema2 = CinemaCityPoznanPlaza
  private val date1   = LocalDate.of(2026, 5, 23)
  private val date2   = LocalDate.of(2026, 5, 24)

  private def slots(n: Int): Seq[Showtime] =
    (0 until n).map(i => Showtime(LocalDateTime.of(2026, 5, 23, 10 + i, 0), None))

  private def showings(entries: (LocalDate, Seq[CinemaShowtimes])*): Seq[(LocalDate, Seq[CinemaShowtimes])] =
    entries.toSeq

  "truncate" should "return all showings when under the row cap" in {
    val all = showings(date1 -> Seq(CinemaShowtimes(cinema1, slots(3))))
    val result = ShowingsTruncation.truncate(all, maxRows = 20, showCinemaHeaders = true)
    result.showings shouldBe all
    result.hiddenCount shouldBe 0
  }

  it should "truncate at cinema boundaries when over the cap" in {
    // 2 dates × 2 cinemas × 12 slots each
    // date label=1, cinema header=1, ceil(12/6)=2 pill rows → 4 lines per cinema
    // date1: 1 (label) + 4 (cinema1) + 4 (cinema2) = 9
    // date2: 1 (label) + 4 (cinema1) + 4 (cinema2) = 9
    // total = 18 rows → fits in 20
    // With cap=10: date1 fills 9, date2 cinema1 would add 1+4=5 → 14 > 10 → capped
    val all = showings(
      date1 -> Seq(CinemaShowtimes(cinema1, slots(12)), CinemaShowtimes(cinema2, slots(12))),
      date2 -> Seq(CinemaShowtimes(cinema1, slots(12)), CinemaShowtimes(cinema2, slots(12)))
    )
    val result = ShowingsTruncation.truncate(all, maxRows = 10, showCinemaHeaders = true)
    result.showings.size shouldBe 1
    result.showings.head._1 shouldBe date1
    result.hiddenCount shouldBe 24
  }

  it should "skip truncation when 3 or fewer screenings would be hidden" in {
    // date1: 1 (label) + 1 (header) + 1 (pill row for 2 slots) = 3
    // date2: 1 (label) + 1 (header) + 1 (pill row for 3 slots) = 3 → total 6
    // With cap=4: date1 uses 3, date2 cinema1 adds 3 → 6 > 4 → would hide 3
    // 3 hidden ≤ minHidden threshold → returns all
    val all = showings(
      date1 -> Seq(CinemaShowtimes(cinema1, slots(2))),
      date2 -> Seq(CinemaShowtimes(cinema1, slots(3)))
    )
    val result = ShowingsTruncation.truncate(all, maxRows = 4, showCinemaHeaders = true)
    result.showings shouldBe all
    result.hiddenCount shouldBe 0
  }

  it should "account for cinema headers only when showCinemaHeaders is true" in {
    // Without headers: date label=1 + ceil(12/6)=2 → 3 lines per cinema-day
    // date1: 1 + 2 = 3, date2: 1 + 2 = 3 → total 6
    // With cap=4: date1=3 fits, date2 would add 3 → 6 > 4 → hidden
    val all = showings(
      date1 -> Seq(CinemaShowtimes(cinema1, slots(12))),
      date2 -> Seq(CinemaShowtimes(cinema1, slots(12)))
    )
    val result = ShowingsTruncation.truncate(all, maxRows = 4, showCinemaHeaders = false)
    result.showings.size shouldBe 1
    result.hiddenCount shouldBe 12
  }

  "showtimeNoun" should "use Polish plural rules" in {
    ShowingsTruncation.showtimeNoun(1) shouldBe "seans"
    ShowingsTruncation.showtimeNoun(2) shouldBe "seanse"
    ShowingsTruncation.showtimeNoun(3) shouldBe "seanse"
    ShowingsTruncation.showtimeNoun(4) shouldBe "seanse"
    ShowingsTruncation.showtimeNoun(5) shouldBe "seansów"
    ShowingsTruncation.showtimeNoun(12) shouldBe "seansów"
    ShowingsTruncation.showtimeNoun(13) shouldBe "seansów"
    ShowingsTruncation.showtimeNoun(22) shouldBe "seanse"
    ShowingsTruncation.showtimeNoun(25) shouldBe "seansów"
  }
}
