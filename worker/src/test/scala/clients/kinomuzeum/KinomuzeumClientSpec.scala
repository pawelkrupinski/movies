package clients.kinomuzeum

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import models.{Kinomuzeum, Showtime}
import services.cinemas.common.FilmDetail
import services.cinemas.pl.KinomuzeumClient

import java.time.{LocalDate, LocalDateTime}

class KinomuzeumClientSpec extends AnyFlatSpec with Matchers {

  private val today   = LocalDate.of(2026, 6, 5)
  private val client  = new KinomuzeumClient(new FakeHttpFetch("kinomuzeum"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  // The "Koncert i dyskusja …" listing is a live event, dropped by the mixed-in
  // OnlyMovieEventsFilter — fetch() returns films only.
  "KinomuzeumClient.fetch" should "return 15 films and 37 showtimes (the concert event is filtered)" in {
    results.size shouldBe 15
    results.flatMap(_.showtimes).size shouldBe 37
  }

  it should "assign KINOMUZEUM to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Kinomuzeum)
  }

  it should "merge a film listed under several slugs into one title" in {
    byTitle("Ojczyzna").showtimes.size shouldBe 7
  }

  it should "carry the film page URL and listing poster on the bare fetch result" in {
    val m = byTitle("Milcząca przyjaciółka")
    m.filmUrl              shouldBe Some("https://artmuseum.pl/wydarzenia/milczaca-przyjaciolka-1")
    m.posterUrl.getOrElse("") should startWith ("https://api-sf.artmuseum.pl")
  }

  it should "enrich runtime / year / countries / director from the detail page" in {
    val d = detailFor("Milcząca przyjaciółka")
    d.runtimeMinutes shouldBe Some(147)
    d.releaseYear    shouldBe Some(2025)
    d.countries      shouldBe Seq("Niemcy", "Francja", "Węgry")
    d.director       shouldBe Seq("Ildiko Enyedi")
  }

  it should "resolve section-header dates and build sklep booking URLs" in {
    byTitle("Milcząca przyjaciółka").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 6, 14, 30),
        Some("https://sklep.artmuseum.pl/rezerwacja/rezerwacja/nienumerowane.html?id=21960&idt=fa5a8b1d69b04693cb728ad042e24275"),
        None, Nil
      )
  }
}
