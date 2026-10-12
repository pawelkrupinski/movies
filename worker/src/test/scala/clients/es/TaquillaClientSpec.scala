package clients.es

import clients.tools.{FakeHttpFetch, RecordTaquilla}
import models.{Cinema, CinemaMovie, SpanishRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.es.{TaquillaClient, TaquillaParser}

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays mk2 El Ingenio's (Vélez-Málaga) whole taquilla.com scrape from disk — the
 * real responses recorded 2026-10-12 by `clients.tools.RecordTaquilla`: the venue's
 * calendar (`/json2/search/calendar`, 22 days to 2027-02-23) and every day's events
 * (`/json2/search/events?t10d=<day>`), each cut to the fields the parser reads.
 * `today` is pinned to the capture date, the day window's start.
 */
class TaquillaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Today = LocalDate.of(2026, 10, 12)
  private val Ingenio: Cinema =
    SpanishRoster.theaterIdByCinema.collectFirst { case (c, RecordTaquilla.TheaterId) => c }.value

  private def client(today: LocalDate = Today) =
    new TaquillaClient(new FakeHttpFetch("taquilla", strict = true), RecordTaquilla.Venue, Ingenio, today = today)

  private val films: Seq[CinemaMovie] = client().fetch()

  private def film(title: String): CinemaMovie = films.find(_.movie.title == title).value

  "planChunks" should "be every day the venue's calendar sells, advance sales months out included" in {
    val days = client().planChunks()
    days should have size 22
    days.head shouldBe "2026-10-12"
    days.last shouldBe "2027-02-23"
  }

  it should "start at today, never at a day already past" in {
    client(LocalDate.of(2026, 12, 16)).planChunks().head shouldBe "2026-12-17"
  }

  "fetch" should "merge each film's days into one row, every row on the venue" in {
    films should have size 25
    films.map(_.cinema).toSet shouldBe Set(Ingenio)
    films.flatMap(_.showtimes) should have size 207
    films.map(_.movie.title).distinct should have size films.size
  }

  it should "reach as far as the venue sells — the opera and ballet relays months out" in {
    val days = films.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    days.head shouldBe Today
    days.last shouldBe LocalDate.of(2027, 2, 23)
    film("Ballet El Cascanueces").showtimes.map(_.dateTime) should contain (LocalDateTime.of(2026, 12, 1, 20, 15))
  }

  it should "carry the film's catalogue row and its Taquilla page" in {
    val doomsday = film("Vengadores: Doomsday")
    doomsday.movie.runtimeMinutes.value shouldBe 165
    doomsday.synopsis.value should startWith ("Primera de las dos películas")
    doomsday.filmUrl.value shouldBe "https://www.taquilla.com/entradas/vengadores-doomsday"
    doomsday.externalIds shouldBe Map("taquilla" -> "7481722")
    doomsday.posterUrl.value should startWith ("https://")
    // "PDC" is a certificate still pending: none, not "PDC".
    doomsday.ageRating shouldBe None
  }

  it should "book at the chain's own ticketing page, and read its version off the link" in {
    val session = film("Vengadores: Doomsday").showtimes.find(_.dateTime == LocalDateTime.of(2026, 12, 23, 15, 45)).value
    session.bookingUrl.value should startWith ("https://cine.entradas.com/cine/velez-malaga/cinesur-el-ingenio/evento/125317/")
    session.bookingUrl.value should not include "&amp;"
    // The session is named plain "Vengadores: Doomsday"; the link asks for language=VOSE.
    session.format shouldBe List("VOSE")
  }

  it should "drop the '(Cine)' Taquilla files an event-cinema relay under, keeping the raw name" in {
    val nutcracker = film("Ballet El Cascanueces")
    nutcracker.movie.rawTitle.value shouldBe "Ballet El Cascanueces (Cine)"
  }

  // ── session markers ───────────────────────────────────────────────────────────

  "formatTokens" should "read the markers the session's name adds to the film's" in {
    TaquillaParser.formatTokens("VOSE Clayface", "Clayface") shouldBe List("VOSE")
    TaquillaParser.formatTokens("Vengadores: Doomsday (Infinity Vision) - PREMIUM", "Vengadores: Doomsday") shouldBe
      List("INFINITY", "PREMIUM")
    TaquillaParser.formatTokens("Dune: Parte Tres - INGLÉS SUBTITULADO EN ESPAÑOL (VOSE)", "Dune: Parte Tres") shouldBe List("VOSE")
    TaquillaParser.formatTokens("Naza - SUBTÍTULOS ESPAÑOL", "Naza") shouldBe List("VOSE")
    TaquillaParser.formatTokens("CAT Cronos", "Cronos") shouldBe List("CAT")
  }

  it should "ignore a marker the film's own title carries, and one it does not know" in {
    TaquillaParser.formatTokens("Avatar 3D", "Avatar 3D") shouldBe Nil
    TaquillaParser.formatTokens("Clayface - XE", "Clayface") shouldBe Nil
    TaquillaParser.formatTokens("Cats", "Cats") shouldBe Nil
  }

  "ageLabel" should "spell certificates the SensaCine way" in {
    TaquillaParser.ageLabel("M-12").value shouldBe "+12"
    TaquillaParser.ageLabel("NO RECOMENDADA PARA MENORES DE 16 AÑOS").value shouldBe "+16"
    TaquillaParser.ageLabel("TP").value shouldBe "APTA"
    TaquillaParser.ageLabel("APTA PARA TODOS LOS PÚBLICOS").value shouldBe "APTA"
    TaquillaParser.ageLabel("+18").value shouldBe "+18"
    Seq("PDC", "EDAD PENDIENTE DE CALIFICACIÓN", "-").foreach(TaquillaParser.ageLabel(_) shouldBe None)
  }

  "days" should "fail a body that is no calendar, and read an empty one as nothing on sale" in {
    an[Exception] should be thrownBy TaquillaParser.days("<html>Cargando…</html>")
    TaquillaParser.days("""{"meta":{}}""") shouldBe None
    TaquillaParser.days("""{"objects":[]}""").value shouldBe empty
    TaquillaParser.days("""{"objects":{"2026-10-13":0,"2026-10-12":3}}""").value shouldBe Seq(LocalDate.of(2026, 10, 12))
  }

  "day" should "fail a body that is no day's listing, so the chunk is retried" in {
    an[IllegalStateException] should be thrownBy TaquillaParser.day("""{"meta":{}}""", Ingenio)
  }
}
