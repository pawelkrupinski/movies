package clients.es

import clients.tools.{FakeHttpFetch, RecordArtesiete}
import models.{Cinema, CinemaMovie, SpanishRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.es.{ArtesieteClient, ArtesieteParser}

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays Artesiete La Torre's (Zaragoza) whole scrape from disk — its venue page as
 * recorded 2026-10-12 by `clients.tools.RecordArtesiete`, cut to the
 * `:fullsessionsinfo` prop the parser reads (the page's other props carry the
 * chain's back-office credentials and are never checked in).
 */
class ArtesieteClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val LaTorre: Cinema =
    SpanishRoster.theaterIdByCinema.collectFirst { case (c, RecordArtesiete.TheaterId) => c }.value

  private val films: Seq[CinemaMovie] =
    new ArtesieteClient(new FakeHttpFetch("artesiete", strict = true), RecordArtesiete.Page, LaTorre).fetch()

  private def film(title: String): CinemaMovie = films.find(_.movie.title == title).value

  "fetch" should "turn the page's sessions into one row per film, every row on the venue" in {
    films should have size 19
    films.map(_.cinema).toSet shouldBe Set(LaTorre)
    films.flatMap(_.showtimes) should have size 121
  }

  it should "reach as far as the venue sells — two months out" in {
    val days = films.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    days.head shouldBe LocalDate.of(2026, 10, 12)
    days.last shouldBe LocalDate.of(2026, 12, 20)
    days should have size 14
  }

  it should "carry each session's room, format and booking page" in {
    val doomsday = film("VENGADORES: DOOMSDAY")
    val session  = doomsday.showtimes.find(s => s.dateTime == LocalDateTime.of(2026, 12, 17, 17, 0) && s.room.contains("Sala 05")).value
    session.format shouldBe List("INFINITY")
    session.bookingUrl.value shouldBe "https://latorre.artesiete.es/Session/21/21/15900/VENGADORES%3A%20DOOMSDAY/77310"
    doomsday.showtimes.map(_.format).distinct should contain (List("3D", "4K", "ATMOS"))
  }

  it should "carry the film's sheet off its sessions" in {
    val doomsday = film("VENGADORES: DOOMSDAY")
    doomsday.movie.originalTitle.value shouldBe "Avengers: Doomsday"
    doomsday.movie.runtimeMinutes.value shouldBe 165
    doomsday.posterUrl.value shouldBe "https://latorre.artesiete.es/Posters/avengersdoomsday.jpg"
    doomsday.externalIds shouldBe Map("artesiete" -> "15900")
    doomsday.ageRating shouldBe None   // "Pendiente de Calificacion"
  }

  it should "tag an original-version session with its version" in {
    film("LA OTRA MADRE").showtimes.map(_.format) should contain (List("VOSE"))
  }

  "formatTokens" should "read the chain's composed format words, dropping the baseline" in {
    ArtesieteParser.formatTokens("Digital") shouldBe Nil
    ArtesieteParser.formatTokens("4K Laser Atmos 3D") shouldBe List("3D", "4K", "LASER", "ATMOS")
    ArtesieteParser.formatTokens("Láser VO ") shouldBe List("LASER", "VO")
    ArtesieteParser.formatTokens("Digital VO") shouldBe List("VO")
    ArtesieteParser.formatTokens("INGLES SUB ESPANOL") shouldBe List("VOSE")
    ArtesieteParser.formatTokens("Infinity Vision") shouldBe List("INFINITY")
    ArtesieteParser.formatTokens("Hora Silenciosa") shouldBe Nil
  }

  "films" should "fail a page that carries no sessions prop, and read an empty one as nothing on sale" in {
    an[IllegalStateException] should be thrownBy ArtesieteParser.films("<html>mantenimiento</html>", "https://x.artesiete.es", LaTorre)
    ArtesieteParser.films("<div :fullsessionsinfo='[]'></div>", "https://x.artesiete.es", LaTorre) shouldBe empty
  }
}
