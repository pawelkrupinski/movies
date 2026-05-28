package controllers

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

class FilterDescriptionSpec extends AnyFlatSpec with Matchers {

  // ── Test fixtures ───────────────────────────────────────────────────────────

  private def movie(title: String, countries: List[String] = Nil): Movie =
    Movie(title = title, countries = countries)

  private def slot(cinema: Cinema, room: String, time: String): Showtime = {
    val (h, m) = time.split(':').map(_.toInt) match { case Array(a, b) => (a, b) }
    Showtime(dateTime = LocalDateTime.of(2026, 5, 17, h, m), bookingUrl = None, room = Some(room))
  }

  private def film(
    title:     String,
    cinema:    Cinema,
    rooms:     Seq[String],
    countries: List[String] = Nil,
    director:  Seq[String] = Nil,
    cast:      Seq[String] = Nil,
  ): FilmSchedule = FilmSchedule(
    movie         = movie(title, countries),
    posterUrl     = None,
    synopsis      = None,
    cast          = cast,
    director      = director,
    cinemaFilmUrls = Nil,
    showings      = Seq(
      LocalDate.of(2026, 5, 17) -> Seq(CinemaShowtimes(cinema, rooms.map(r => slot(cinema, r, "18:00"))))
    )
  )

  private val schedules: Seq[FilmSchedule] = Seq(
    film("Belle",                 Multikino, Seq("Sala 5", "Sala 7"), List("Japonia"),  Seq("Mamoru Hosoda")),
    film("Diabeł nosi Pradę 2",  Multikino, Seq("Sala 9", "Sala 10"), List("USA"),     Seq("David Frankel")),
    film("Bizancjum",             Helios,    Seq("Sala 3"),            List("Polska"),  Seq("Anna Smith")),
  )

  // ── Default (no filter) ─────────────────────────────────────────────────────

  "FilterDescription.forIndex with an empty query" should "produce the default Kinowo title" in {
    val meta = FilterDescription.forIndex(Map.empty, schedules)
    meta.title       shouldBe FilterDescription.DefaultTitle
    meta.description shouldBe FilterDescription.DefaultDescription
  }

  it should "ignore unrecognised params and date=today (the default)" in {
    val meta = FilterDescription.forIndex(Map("date" -> Seq("today"), "junk" -> Seq("noise")), schedules)
    meta.title shouldBe "Kinowo"
  }

  // ── Single-filter phrasing ──────────────────────────────────────────────────

  "date=tomorrow" should "surface as 'jutro'" in {
    val meta = FilterDescription.forIndex(Map("date" -> Seq("tomorrow")), schedules)
    meta.title shouldBe "Kinowo — filmy jutro"
  }

  "date=2026-05-30" should "surface as the ISO date verbatim" in {
    val meta = FilterDescription.forIndex(Map("date" -> Seq("2026-05-30")), schedules)
    meta.title shouldBe "Kinowo — filmy 2026-05-30"
  }

  "imax=1" should "surface as 'IMAX'" in {
    FilterDescription.forIndex(Map("imax" -> Seq("1")), schedules).title shouldBe "Kinowo — filmy IMAX"
  }

  "dim=3D and lang=NAP" should "combine in order" in {
    val meta = FilterDescription.forIndex(Map("dim" -> Seq("3D"), "lang" -> Seq("NAP")), schedules)
    meta.title shouldBe "Kinowo — filmy 3D, z napisami"
  }

  "from=18:30" should "surface as 'od 18:30'" in {
    FilterDescription.forIndex(Map("from" -> Seq("18:30")), schedules).title shouldBe "Kinowo — filmy od 18:30"
  }

  "q=Diabeł" should "surface inside Polish quotation marks" in {
    FilterDescription.forIndex(Map("q" -> Seq("Diabeł")), schedules).title shouldBe "Kinowo — filmy „Diabeł”"
  }

  // ── Room inclusion / exclusion (smaller set wins) ──────────────────────────

  "room= with all-but-one excluded" should "describe the single INCLUDED room" in {
    // 5 rooms in the universe (Multikino × 4 + Helios × 1). Excluding 4 leaves 1.
    val excluded = Seq(
      "Multikino Stary Browar|Sala 5",
      "Multikino Stary Browar|Sala 9",
      "Multikino Stary Browar|Sala 10",
      "Helios Posnania|Sala 3",
    ).mkString(",")
    val meta = FilterDescription.forIndex(Map("room" -> Seq(excluded)), schedules)
    meta.title shouldBe "Kinowo — filmy w sali Sala 7"
  }

  "room= excluding just one" should "describe as 'bez sal …'" in {
    val meta = FilterDescription.forIndex(
      Map("room" -> Seq("Multikino Stary Browar|Sala 5")),
      schedules
    )
    // Excluded set is the smaller one (1 vs 4 included) — describe via 'bez'.
    meta.title shouldBe "Kinowo — filmy bez sal Sala 5"
  }

  "room= excluding many rooms" should "summarise the count instead of listing them" in {
    // Universe has 5 rooms. Pick exclusion such that BOTH sides are > 3 so
    // the helper falls into the count branch.
    val schedulesBig = Seq(
      film("A", Multikino, Seq("Sala 1", "Sala 2", "Sala 3", "Sala 4", "Sala 5", "Sala 6", "Sala 7", "Sala 8"))
    )
    val excluded = (1 to 4).map(i => s"Multikino Stary Browar|Sala $i").mkString(",")
    val meta = FilterDescription.forIndex(Map("room" -> Seq(excluded)), schedulesBig)
    // 4 included, 4 excluded — pickIncluded wins (≤ excluded.size), and 4 > 3 → count.
    meta.title shouldBe "Kinowo — filmy 4 sal"
  }

  // ── Cinema inclusion / exclusion ───────────────────────────────────────────

  "cinema= disabling all but two" should "describe the two included via pill names" in {
    val excluded = Cinema.all.map(_.displayName)
      .filterNot(c => c == Multikino.displayName || c == Helios.displayName)
      .mkString(",")
    val meta = FilterDescription.forIndex(Map("cinema" -> Seq(excluded)), schedules)
    meta.title shouldBe "Kinowo — filmy w Helios, Multikino"
  }

  "cinema= disabling one" should "describe as 'bez <pill name>'" in {
    FilterDescription.forIndex(
      Map("cinema" -> Seq(Multikino.displayName)),
      schedules,
    ).title shouldBe "Kinowo — filmy bez Multikino"
  }

  // ── Multi-filter combinations and truncation ───────────────────────────────

  "multiple filters" should "join with comma in URL order" in {
    val meta = FilterDescription.forIndex(
      Map("date" -> Seq("tomorrow"), "dim" -> Seq("3D"), "imax" -> Seq("1")),
      schedules,
    )
    meta.title shouldBe "Kinowo — filmy jutro, 3D, IMAX"
  }

  "a long filter combination" should "truncate the title at MaxTitle with an ellipsis" in {
    val excluded = (1 to 8).map(i => s"X|Room $i").mkString(",")  // unknown cinemas, so room phrase is summary
    val meta = FilterDescription.forIndex(
      Map(
        "q"   -> Seq("Mandalorian i Grogu w wielkim galaktycznym konflikcie"),
        "dim" -> Seq("3D"),
        "lang" -> Seq("NAP"),
      ),
      schedules,
    )
    meta.title.length should be <= FilterDescription.MaxTitle
    meta.title should endWith ("…")
    meta.title should startWith ("Kinowo — filmy")
  }
}
