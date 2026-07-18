package services.cinemas

import org.scalatest.matchers.should.Matchers
import models._
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.SlotsToMovies

import java.time.LocalDateTime

/** Guards the invariant fold every hand-rolled cinema scraper shares via
  * [[SlotsToMovies.fold]]: group-by-title, dedup + time-sort the showtimes,
  * drop a film with no showtimes, and title-sort the result. A regression here
  * would silently change the merged output of ~20 scrapers at once. */
class SlotsToMoviesSpec extends AnyFlatSpec with Matchers {

  private val cinema = KinoMuza
  private def dt(h: Int) = LocalDateTime.of(2026, 6, 8, h, 0)
  private case class Slot(title: String, dateTime: LocalDateTime, booking: Option[String])

  private def fold(slots: Seq[Slot]): Seq[CinemaMovie] =
    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking)
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.map(_.booking).headOption.flatten,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }

  "fold" should "group slots by title into one CinemaMovie per title" in {
    val out = fold(Seq(
      Slot("Dune", dt(18), Some("b1")),
      Slot("Dune", dt(20), Some("b2")),
      Slot("Wicked", dt(19), Some("b3"))
    ))
    out.map(_.movie.title) shouldBe Seq("Dune", "Wicked")
    out.head.showtimes.map(_.dateTime) shouldBe Seq(dt(18), dt(20))
  }

  it should "dedup showtimes by (dateTime, bookingUrl) and sort by time" in {
    val out = fold(Seq(
      Slot("Dune", dt(20), Some("b1")),
      Slot("Dune", dt(18), Some("b1")),
      Slot("Dune", dt(20), Some("b1")) // exact duplicate
    ))
    out.head.showtimes.map(_.dateTime) shouldBe Seq(dt(18), dt(20))
  }

  it should "keep same-time screenings with different booking links" in {
    val out = fold(Seq(
      Slot("Dune", dt(20), Some("b1")),
      Slot("Dune", dt(20), Some("b2"))
    ))
    out.head.showtimes should have size 2
  }

  it should "drop a film whose slots all yield no showtimes" in {
    // With a distinctBy that collapses everything and an empty source there are
    // no showtimes, so the film is dropped.
    fold(Seq.empty) shouldBe empty
  }

  it should "sort the resulting movies by title" in {
    val out = fold(Seq(
      Slot("Zorro", dt(18), None),
      Slot("Amelia", dt(18), None)
    ))
    out.map(_.movie.title) shouldBe Seq("Amelia", "Zorro")
  }

  it should "honour a custom distinctBy key" in {
    val out = SlotsToMovies.fold(
      Seq(Slot("Dune", dt(20), Some("b1")), Slot("Dune", dt(20), Some("b2"))),
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking),
      distinctBy = _.dateTime // dedup on time alone, ignoring booking
    ) { (title, _, showtimes) =>
      CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, showtimes)
    }
    out.head.showtimes should have size 1
  }
}
