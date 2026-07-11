package controllers

import models._
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

class FilterDescriptionSpec extends AnyFlatSpec with Matchers {

  // ── Test fixtures ───────────────────────────────────────────────────────────

  private def movie(title: String, countries: List[String] = Nil, genres: Seq[String] = Nil): Movie =
    Movie(title = title, countries = countries, genres = genres)

  private def slot(cinema: Cinema, room: String, time: String): Showtime = {
    val (h, m) = time.split(':').map(_.toInt) match { case Array(a, b) => (a, b) }
    Showtime(dateTime = LocalDateTime.of(2026, 5, 17, h, m), bookingUrl = None, room = Some(room))
  }

  private def film(
    title:     String,
    cinema:    Cinema,
    rooms:     Seq[String],
    countries: List[String] = Nil,
    genres:    Seq[String] = Nil,
    director:  Seq[String] = Nil,
    cast:      Seq[String] = Nil,
  ): FilmSchedule = FilmSchedule(
    movie         = movie(title, countries, genres),
    posterUrl     = None,
    synopsis      = None,
    cast          = cast,
    director      = director,
    cinemaFilmUrls = Nil,
    showings      = Seq(
      LocalDate.of(2026, 5, 17) -> Seq(CinemaShowtimes(cinema, rooms.map(r => slot(cinema, r, "18:00"))))
    ),
    resolved      = TestReadModel.resolved(title, None, MovieRecord())
  )

  private val schedules: Seq[FilmSchedule] = Seq(
    film("Belle",                 Multikino, Seq("Sala 5", "Sala 7"), List("Japonia"),  genres = Seq("Animacja"), director = Seq("Mamoru Hosoda")),
    film("Diabeł nosi Pradę 2",  Multikino, Seq("Sala 9", "Sala 10"), List("USA"),     genres = Seq("Komedia"),  director = Seq("David Frankel")),
    film("Bizancjum",             Helios,    Seq("Sala 3"),            List("Polska"),  genres = Seq("Dramat"),   director = Seq("Anna Smith")),
  )

  // ── Default (no filter) ─────────────────────────────────────────────────────

  "FilterDescription.forIndex with an empty query" should "produce the keyword-rich default city title" in {
    val meta = FilterDescription.forIndex(Poznan,Map.empty, schedules)
    meta.title       shouldBe FilterDescription.defaultTitle(Poznan)
    meta.title       should include("Repertuar kin w Poznaniu")
    meta.title       should include("godziny seansów")
    meta.title       should endWith("| Kinowo")
    meta.description shouldBe FilterDescription.defaultDescription(Poznan)
    meta.description should include("poznańskich")
    meta.description should include("godziny seansów")
  }

  it should "ignore unrecognised parameters and date=today (the default)" in {
    val meta = FilterDescription.forIndex(Poznan,Map("date" -> Seq("today"), "junk" -> Seq("noise")), schedules)
    meta.title shouldBe FilterDescription.defaultTitle(Poznan)
  }

  // ── Single-filter phrasing ──────────────────────────────────────────────────

  "date=tomorrow" should "surface as 'jutro'" in {
    val meta = FilterDescription.forIndex(Poznan,Map("date" -> Seq("tomorrow")), schedules)
    meta.title shouldBe "Kinowo — filmy jutro"
  }

  "date=anytime" should "stay silent — it's the no-restriction default view" in {
    // `?date=anytime` widens the date filter to show all dates. The page is
    // already "every film" — no extra word in the OG belongs there. Only date
    // values that actually narrow (today is silent too; tomorrow / week /
    // specific ISO surface as their phrase) deserve a phrase.
    val meta = FilterDescription.forIndex(Poznan,Map("date" -> Seq("anytime")), schedules)
    meta.title shouldBe FilterDescription.defaultTitle(Poznan)
  }

  "date=2026-05-30" should "surface as the ISO date verbatim" in {
    val meta = FilterDescription.forIndex(Poznan,Map("date" -> Seq("2026-05-30")), schedules)
    meta.title shouldBe "Kinowo — filmy 2026-05-30"
  }

  "imax=1" should "surface as 'IMAX'" in {
    FilterDescription.forIndex(Poznan,Map("imax" -> Seq("1")), schedules).title shouldBe "Kinowo — filmy IMAX"
  }

  "dim=3D and lang=NAP" should "combine in order" in {
    val meta = FilterDescription.forIndex(Poznan,Map("dim" -> Seq("3D"), "lang" -> Seq("NAP")), schedules)
    meta.title shouldBe "Kinowo — filmy 3D, z napisami"
  }

  "from=18:30" should "surface as 'od 18:30'" in {
    FilterDescription.forIndex(Poznan,Map("from" -> Seq("18:30")), schedules).title shouldBe "Kinowo — filmy od 18:30"
  }

  "q=Diabeł" should "surface inside Polish quotation marks" in {
    FilterDescription.forIndex(Poznan,Map("q" -> Seq("Diabeł")), schedules).title shouldBe "Kinowo — filmy „Diabeł”"
  }

  // ── Room inclusion (URL items = INCLUDED rooms) ────────────────────────────

  "room=<single room> via per-value entries" should "describe the included room" in {
    // JS writes `?room=A&room=B` (one entry per included item) — Play
    // surfaces that as `Map("room" -> Seq("A", "B"))`. The helper treats the
    // list as the inclusion set; with 1 in / 4 out, "w sali" wins.
    val meta = FilterDescription.forIndex(Poznan,
      Map("room" -> Seq("Multikino Stary Browar|Sala 5")),
      schedules,
    )
    meta.title shouldBe "Kinowo — filmy w sali Sala 5"
  }

  "room=<all-but-one>" should "describe as 'bez sal …' (smaller side wins)" in {
    // 5 rooms in the universe. Including 4 leaves 1 excluded — excluded side
    // is smaller, so we describe via 'bez'.
    val included = Seq(
      "Multikino Stary Browar|Sala 5",
      "Multikino Stary Browar|Sala 9",
      "Multikino Stary Browar|Sala 10",
      "Helios Posnania|Sala 3",
    )
    val meta = FilterDescription.forIndex(Poznan,Map("room" -> included), schedules)
    meta.title shouldBe "Kinowo — filmy bez sal Sala 7"
  }

  "room=<legacy comma-list>" should "still narrow correctly for old shared URLs" in {
    // Pre-flip URLs encoded the list as one comma-joined value. Stay
    // compatible so a bookmarked `?room=A,B` doesn't break.
    val meta = FilterDescription.forIndex(Poznan,
      Map("room" -> Seq("Multikino Stary Browar|Sala 5,Multikino Stary Browar|Sala 7")),
      schedules,
    )
    meta.title shouldBe "Kinowo — filmy w salach Sala 5, Sala 7"
  }

  "room=<half-and-half summary>" should "fall back to a count when both sides are large" in {
    val schedulesBig = Seq(
      film("A", Multikino, Seq("Sala 1", "Sala 2", "Sala 3", "Sala 4", "Sala 5", "Sala 6", "Sala 7", "Sala 8"))
    )
    val included = (1 to 4).map(i => s"Multikino Stary Browar|Sala $i")
    val meta = FilterDescription.forIndex(Poznan,Map("room" -> included), schedulesBig)
    // 4 included, 4 excluded — pickIncluded wins (≤ excluded.size), 4 > 3 → count.
    meta.title shouldBe "Kinowo — filmy 4 sal"
  }

  // ── Cinema inclusion (URL items = ENABLED cinemas) ─────────────────────────

  "cinema=<just two>" should "describe the two included via pill names" in {
    val meta = FilterDescription.forIndex(Poznan,
      Map("cinema" -> Seq(Multikino.displayName, Helios.displayName)),
      schedules,
    )
    meta.title shouldBe "Kinowo — filmy w Helios, Multikino"
  }

  "cinema=<all but one>" should "describe as 'bez <pill name>'" in {
    val included = Cinema.all.map(_.displayName).filterNot(_ == Multikino.displayName)
    FilterDescription.forIndex(Poznan,
      Map("cinema" -> included),
      schedules,
    ).title shouldBe "Kinowo — filmy bez Multikino"
  }

  // ── Genre inclusion (URL items = INCLUDED genres) ──────────────────────────

  "genre=<single genre>" should "describe the included genre with the 'gatunku' preposition" in {
    // 1 of 3 genres included → included side is smaller → singular 'gatunku'.
    FilterDescription.forIndex(Poznan,
      Map("genre" -> Seq("Komedia")),
      schedules,
    ).title shouldBe "Kinowo — filmy gatunku Komedia"
  }

  "genre=<all but one>" should "describe via 'bez gatunków' (smaller excluded side wins)" in {
    FilterDescription.forIndex(Poznan,
      Map("genre" -> Seq("Animacja", "Komedia")),
      schedules,
    ).title shouldBe "Kinowo — filmy bez gatunków Dramat"
  }

  // ── Multi-filter combinations and truncation ───────────────────────────────

  "multiple filters" should "join with comma in URL order" in {
    val meta = FilterDescription.forIndex(Poznan,
      Map("date" -> Seq("tomorrow"), "dim" -> Seq("3D"), "imax" -> Seq("1")),
      schedules,
    )
    meta.title shouldBe "Kinowo — filmy jutro, 3D, IMAX"
  }

  "a long filter combination" should "truncate the title at MaxTitle with an ellipsis" in {
    val excluded = (1 to 8).map(i => s"X|Room $i").mkString(",")  // unknown cinemas, so room phrase is summary
    val meta = FilterDescription.forIndex(Poznan,
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

  // ── English deployment (UK city → en-GB language) ──────────────────────────
  // `London.country` is `UnitedKingdom` (en-GB), so every phrase renders in
  // English while `Poznan` (above) stays byte-identical Polish.

  "cityHeading" should "read the declined Polish locative / the English 'in {city}'" in {
    FilterDescription.cityHeading(Poznan) shouldBe "Repertuar kin w Poznaniu"
    FilterDescription.cityHeading(London) shouldBe "Cinema listings in London"
  }

  "FilterDescription for a UK city" should "produce the English default title + description" in {
    FilterDescription.defaultTitle(London) shouldBe "Cinema listings in London – today's showtimes | Kinowo"
    val d = FilterDescription.defaultDescription(London)
    d should include ("All London cinema listings – today's showtimes")
    d should include ("IMDb, Filmweb, Metacritic and Rotten Tomatoes ratings")
    d should include ("See what's on today in London")
  }

  it should "use 'films' (not 'filmy') as the filtered-title noun" in {
    FilterDescription.forIndex(London, Map("date" -> Seq("tomorrow")), schedules).title shouldBe "Kinowo — films tomorrow"
  }

  it should "phrase date / dim / lang / imax / from filters in English" in {
    FilterDescription.forIndex(London, Map("date" -> Seq("week")), schedules).title shouldBe "Kinowo — films this week"
    FilterDescription.forIndex(London, Map("dim" -> Seq("3D"), "lang" -> Seq("NAP")), schedules).title shouldBe "Kinowo — films 3D, with subtitles"
    FilterDescription.forIndex(London, Map("lang" -> Seq("DUB")), schedules).title shouldBe "Kinowo — films with dubbing"
    FilterDescription.forIndex(London, Map("imax" -> Seq("1")), schedules).title shouldBe "Kinowo — films IMAX"
    FilterDescription.forIndex(London, Map("from" -> Seq("18:30")), schedules).title shouldBe "Kinowo — films from 18:30"
  }

  it should "wrap the search query in English double quotes" in {
    FilterDescription.forIndex(London, Map("q" -> Seq("Dune")), schedules).title shouldBe "Kinowo — films “Dune”"
  }

  it should "phrase room include / exclude / summary in English" in {
    FilterDescription.forIndex(London,
      Map("room" -> Seq("Multikino Stary Browar|Sala 5")), schedules,
    ).title shouldBe "Kinowo — films in screen Sala 5"

    val allButOne = Seq(
      "Multikino Stary Browar|Sala 5", "Multikino Stary Browar|Sala 9",
      "Multikino Stary Browar|Sala 10", "Helios Posnania|Sala 3",
    )
    FilterDescription.forIndex(London, Map("room" -> allButOne), schedules)
      .title shouldBe "Kinowo — films without screens Sala 7"

    val schedulesBig = Seq(
      film("A", Multikino, Seq("Sala 1", "Sala 2", "Sala 3", "Sala 4", "Sala 5", "Sala 6", "Sala 7", "Sala 8"))
    )
    val four = (1 to 4).map(i => s"Multikino Stary Browar|Sala $i")
    FilterDescription.forIndex(London, Map("room" -> four), schedulesBig).title shouldBe "Kinowo — films 4 screens"
  }

  it should "phrase genre / country / director filters in English" in {
    FilterDescription.forIndex(London, Map("genre" -> Seq("Komedia")), schedules).title shouldBe "Kinowo — films genre Komedia"
    FilterDescription.forIndex(London, Map("genre" -> Seq("Animacja", "Komedia")), schedules).title shouldBe "Kinowo — films without genres Dramat"
    FilterDescription.forIndex(London, Map("country" -> Seq("Japonia")), schedules).title shouldBe "Kinowo — films from Japonia"
    FilterDescription.forIndex(London, Map("director" -> Seq("David Frankel")), schedules).title shouldBe "Kinowo — films dir. David Frankel"
  }

  it should "phrase cinema filters with the English 'at' / 'without' prepositions" in {
    val cinemas = London.cinemaDisplayNames
    // Include just the first cinema → smaller included side → "at <pill>".
    FilterDescription.forIndex(London, Map("cinema" -> Seq(cinemas.head)), schedules)
      .title should startWith ("Kinowo — films at ")
    // Include all but one → smaller excluded side → "without <pill>".
    FilterDescription.forIndex(London, Map("cinema" -> cinemas.tail), schedules)
      .title should startWith ("Kinowo — films without ")
  }
}
