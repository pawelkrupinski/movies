package clients.iluzjon

import clients.tools.FakeHttpFetch
import models.{KinoIluzjon, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, IluzjonClient}

import java.time.{LocalDate, LocalDateTime}

class IluzjonClientSpec extends AnyFlatSpec with Matchers {

  private val today   = LocalDate.of(2026, 6, 5)
  private val client  = new IluzjonClient(new FakeHttpFetch("iluzjon"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  "IluzjonClient.fetch" should "return 29 films and 54 showtimes" in {
    results.size shouldBe 29
    results.flatMap(_.showtimes).size shouldBe 54
  }

  it should "assign Kino Iluzjon to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoIluzjon)
  }

  it should "merge a film's screenings across day sections" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Zawodowcy")                   shouldBe 8
    counts("Orły Republiki")              shouldBe 7
    counts("Zawieście czerwone latarnie") shouldBe 2
  }

  it should "enrich runtime / year / countries / director from the detail page" in {
    val d = detailFor("Zawieście czerwone latarnie")
    d.runtimeMinutes shouldBe Some(125)
    d.releaseYear    shouldBe Some(1991)
    d.countries      shouldBe Seq("Chiny", "Hongkong", "Tajwan")
    // posterUrl is a LISTING field (the repertoire card's `covers/` image); the
    // detail page carries a different `cache/` poster, so assert the bare movie.
    byTitle("Zawieście czerwone latarnie").posterUrl shouldBe Some("https://www.iluzjon.fn.org.pl/public/covers/movie-7815.jpg")
    d.synopsis.getOrElse("").length should be > 20
  }

  it should "read cast and original title from the detail page" in {
    val d = detailFor("Zawieście czerwone latarnie")
    d.cast          shouldBe Seq("Gong Li", "Saifei He", "Jingwu Ma", "Cuifen Cao")
    d.originalTitle shouldBe Some("Da hong deng long gao gao gua")
  }

  it should "carry the auditorium and a Filmoteka booking URL on each showtime" in {
    byTitle("Zawieście czerwone latarnie").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 15, 30),
        Some("https://bilety.iluzjon.fn.org.pl/MSI/default.aspx?event_id=21750&typetran=1&returnlink=http://www.iluzjon.fn.org.pl/"),
        Some("Stolica"), Nil
      )
  }
}
