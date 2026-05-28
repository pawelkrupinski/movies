package controllers

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class PlanControllerSpec extends AnyFlatSpec with Matchers {

  private def schedule(
    title:   String,
    cinema:  Cinema,
    date:    LocalDate,
    times:   Seq[(String, Option[String])] // (HH:mm, room)
  ): FilmSchedule = {
    val showtimes = times.map { case (hhmm, room) =>
      val Array(h, m) = hhmm.split(":")
      Showtime(date.atTime(h.toInt, m.toInt), bookingUrl = None, room = room)
    }
    FilmSchedule(
      movie     = Movie(title),
      posterUrl = None,
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      cinemaFilmUrls = Seq.empty,
      showings  = Seq((date, Seq(CinemaShowtimes(cinema, showtimes))))
    )
  }

  "PlanController.viewData" should "flatten each (cinema × showtime) into one PlanShowing" in {
    val fs = schedule(
      "Tytuł",
      Multikino,
      LocalDate.of(2026, 5, 28),
      Seq(("18:30", Some("Sala 5")), ("21:00", Some("Sala 5")))
    )

    val data = PlanController.viewData(Seq(fs))
    data.showings shouldBe Seq(
      PlanShowing("Tytuł", "Multikino Stary Browar", Some("Sala 5"), "2026-05-28", "18:30"),
      PlanShowing("Tytuł", "Multikino Stary Browar", Some("Sala 5"), "2026-05-28", "21:00")
    )
  }

  it should "expose a sorted, deduplicated movies list (drives the picker)" in {
    val today = LocalDate.of(2026, 5, 28)
    val a     = schedule("Bravo", Multikino, today, Seq(("18:00", None)))
    val b     = schedule("Alfa",  Helios,    today, Seq(("19:00", None)))
    val data  = PlanController.viewData(Seq(a, b))
    data.movies shouldBe Seq("Alfa", "Bravo")
  }

  it should "group rooms per cinema, dropping cinemas with no room data" in {
    val today = LocalDate.of(2026, 5, 28)
    val withRoom = schedule("X", Multikino, today, Seq(("18:00", Some("Sala 7")), ("20:00", Some("Sala 5"))))
    val noRoom   = schedule("Y", KinoApollo, today, Seq(("19:00", None)))

    val data = PlanController.viewData(Seq(withRoom, noRoom))
    data.cinemaRooms.map(_.cinema) shouldBe Seq("Multikino Stary Browar")
    data.cinemaRooms.head.rooms    shouldBe Seq("Sala 5", "Sala 7")  // sorted
  }

  it should "drop blank/whitespace-only room strings" in {
    val fs = schedule(
      "X",
      Multikino,
      LocalDate.of(2026, 5, 28),
      Seq(("18:00", Some("   ")), ("19:00", Some("Sala 1")))
    )
    val data = PlanController.viewData(Seq(fs))
    data.showings.map(_.room) shouldBe Seq(None, Some("Sala 1"))
    data.cinemaRooms.head.rooms shouldBe Seq("Sala 1")
  }
}
