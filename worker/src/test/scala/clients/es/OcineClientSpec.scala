package clients.es

import clients.tools.FakeHttpFetch
import models.{Cinema, CinemaMovie, SpanishRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.es.{OcineClient, OcineParser}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays Ocine Girona's whole scrape entirely from disk — the real responses
 * recorded 2026-09-25 from its own ticketing server `tickets.ocinegirona.es`:
 * the cartelera (`POST /api/v1/sessions`, 28 distinct films) and every film's detail
 * (`GET /api/v1/pelicula/<id>?lang=es`). Recorded by `clients.tools.RecordOcine`,
 * which blanks only the inlined base64 poster JPEGs. `today` is pinned to the
 * capture date because the client's far-date bound is relative to it.
 */
class OcineClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Today = LocalDate.of(2026, 9, 25)
  private val Girona: Cinema =
    SpanishRoster.theaterIdByCinema.collectFirst { case (c, "E0362") => c }.value

  private def clientOn(fixtures: HttpFetch) = new OcineClient(fixtures, "tickets.ocinegirona.es", Girona, today = Some(Today))

  private val films: Seq[CinemaMovie] = clientOn(new FakeHttpFetch("ocine")).fetch()

  private def film(title: String): CinemaMovie = films.find(_.movie.title == title).value

  "fetch" should "turn every film with a screening on sale into one row" in {
    // 28 films on the cartelera; the 6 announced releases with nothing on sale
    // yet (DIGGER, Whalefall, …) have no screening and so no row.
    films.size shouldBe 22
    films.map(_.cinema).toSet shouldBe Set(Girona)
    all(films.map(_.showtimes.size)) should be > 0
    films.flatMap(_.showtimes).size shouldBe 271
    films.map(_.movie.title) should not contain "DIGGER"
  }

  it should "reach as far as the venue sells — its advance sales months out" in {
    // The horizon is whatever the box office has on sale; SensaCine advertised
    // this venue only to 2026-10-17 on the same day.
    val days = films.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted
    days.head shouldBe Today
    days.last shouldBe LocalDate.of(2026, 12, 15)
    film("Dune Parte Tres").showtimes.map(_.dateTime.toLocalDate).distinct shouldBe Seq(LocalDate.of(2026, 12, 15))
  }

  it should "carry the film-level metadata off the detail" in {
    val odyssey = film("La Odisea")
    odyssey.movie.originalTitle.value shouldBe "The Odyssey"
    odyssey.movie.runtimeMinutes.value shouldBe 172
    odyssey.director shouldBe Seq("Christopher Nolan")
    odyssey.ageRating.value shouldBe "+12"
    odyssey.synopsis should not be empty
    odyssey.filmUrl.value shouldBe "https://tickets.ocinegirona.es/#/DetallPelicula/10311"
    odyssey.externalIds shouldBe Map("ocine" -> "10311")
    // The payload's poster is an inline JPEG, not a URL.
    odyssey.posterUrl shouldBe None
  }

  it should "leave the original title unset when it only repeats the title" in {
    film("Spider-man: Brand New Day").movie.originalTitle shouldBe None
  }

  it should "split a co-directed credit and spell certificates the SensaCine way" in {
    film("Vengadores: Endgame").director shouldBe Seq("Anthony Russo", "Joe Russo")
    film("Tadeo Jones y la lámpara maravillosa").ageRating.value shouldBe "APTA"
    // A release with no certificate yet carries none, not an empty string.
    film("La Bola Negra").ageRating shouldBe None
  }

  // ── a film is a group of variants ──────────────────────────────────────────

  "a film shown several ways" should "be ONE row, each showtime tagged with its own variant's formats" in {
    // Resident Evil: a plain 2D variant (14), a 4D room (5) and ATMOS (14).
    val resident = film("Resident Evil")
    resident.showtimes.size shouldBe 33
    films.count(_.movie.title == "Resident Evil") shouldBe 1
    resident.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 27, 22, 35)).value.format shouldBe List("2D", "4D")
    resident.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 25, 16, 30)).value.format shouldBe List("2D", "ATMOS")
    resident.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 25, 18, 30)).value.format shouldBe List("2D")
  }

  it should "put the language version last, in SensaCine's tokens" in {
    film("En el corazón de la bestia").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 9, 25, 16, 0)).value.format shouldBe List("ATMOS", "VOSE")
    film("Cronos").showtimes.head.format shouldBe List("2D", "CAT")
    film("Vengadores: Endgame").showtimes
      .find(_.dateTime == LocalDateTime.of(2026, 9, 25, 15, 45)).value.format shouldBe List("3D", "4D", "INFINITY")
  }

  it should "strip the variant marker a Catalan-only film's title carries" in {
    val conan = film("Detectiu conan: L'àngel caigut de la carretera")
    conan.movie.rawTitle.value shouldBe "Detectiu conan: L'àngel caigut de la carretera (Català)"
    conan.showtimes.map(_.format).distinct shouldBe Seq(List("2D", "CAT"))
  }

  "an ungrouped live event" should "carry the screenings it holds on itself" in {
    val bts = film("BTS WORLD TOUR 'ARIRANG' IN SAO PAULO: LIVE VIEWING")
    bts.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 31, 14, 45))
    bts.showtimes.head.format shouldBe List("2D", "VOSE")
    bts.showtimes.head.room.value shouldBe "SALA 11"
  }

  "a session" should "carry the room and the box office's own entry link for that screening" in {
    val first = film("Spider-man: Brand New Day").showtimes.head
    first.dateTime shouldBe LocalDateTime.of(2026, 9, 25, 15, 45)
    first.room.value shouldBe "SALA 8"
    first.bookingUrl.value shouldBe "https://tickets.ocinegirona.es/?plan=183786&extern=1&idioma=es"
  }

  // ── failure shapes ─────────────────────────────────────────────────────────

  "a cartelera fetch that fails" should "fail the scrape rather than read as an empty venue" in {
    an [Exception] should be thrownBy clientOn(new FakeHttpFetch("does-not-exist")).planChunks()
  }

  "a 200 that is not a cartelera" should "fail the scrape too" in {
    OcineParser.filmIds("""{"error":"TOKENCADUCATOINVALID"}""") shouldBe None
    OcineParser.filmIds("<html>maintenance</html>") shouldBe None
    OcineParser.filmIds("""{"pelicules":[]}""") shouldBe Some(Nil)
  }

  "planChunks" should "name every film on the cartelera once — running, upcoming and advance-sale" in {
    val ids = clientOn(new FakeHttpFetch("ocine")).planChunks()
    ids.size shouldBe 28 // Dune is in both `estrenes` and `anticipades`
    ids.distinct shouldBe ids
    ids should contain allOf ("10311", "10694", "10771")
  }

  "the client" should "name its host, public page and chain venue id" in {
    val client = clientOn(new FakeHttpFetch("ocine"))
    client.scrapeHosts shouldBe Set("tickets.ocinegirona.es")
    client.sourceUrl.value shouldBe "https://tickets.ocinegirona.es/"
    client.chainVenueId.value shouldBe "tickets.ocinegirona.es"
    client.chain shouldBe true
  }

  // ── pure rules, for combinations the capture does not hold ─────────────────

  "formatTokens" should "drop the baseline and keep an unknown room type rather than lose it" in {
    OcineParser.formatTokens(Seq("Estándar", "2D")) shouldBe List("2D")
    OcineParser.formatTokens(Seq("Digital")) shouldBe Nil
    OcineParser.formatTokens(Seq("ATMOS", "Screen X", "Versión original")) shouldBe List("SCREENX", "ATMOS", "VO")
    OcineParser.formatTokens(Seq("2D", "Sala Láser")) shouldBe List("2D", "SALA LÁSER")
  }

  "cleanTitle" should "strip only a trailing run of variant markers" in {
    OcineParser.cleanTitle("Spider-man: Brand New Day (Sala Premium) (VOSE)") shouldBe "Spider-man: Brand New Day"
    OcineParser.cleanTitle("Vengadores: Endgame (4D) (3D) (IV)") shouldBe "Vengadores: Endgame"
    OcineParser.cleanTitle("Spider-man: Brand New Day (URBAN) [VOSE]") shouldBe "Spider-man: Brand New Day"
    OcineParser.cleanTitle("Toc toc (2017)") shouldBe "Toc toc (2017)"
    OcineParser.cleanTitle("Dune Parte Tres") shouldBe "Dune Parte Tres"
  }
}
