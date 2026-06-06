package clients.nowe_horyzonty

import clients.tools.FakeHttpFetch
import models.{KinoNoweHoryzonty, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.NoweHoryzontyClient

import java.time.{LocalDate, LocalDateTime}

class NoweHoryzontyClientSpec extends AnyFlatSpec with Matchers {

  // The listing carries relative dates ("dzisiaj"/"jutro"); pin `today` to the
  // recording date so the fixture replay is deterministic.
  private val today   = LocalDate.of(2026, 6, 5)
  private val client  = new NoweHoryzontyClient(new FakeHttpFetch("nowe-horyzonty"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "NoweHoryzontyClient.fetch" should "return 91 films from the listing" in {
    results.size shouldBe 91
  }

  it should "return 110 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 110
  }

  it should "assign Kino Nowe Horyzonty to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoNoweHoryzonty)
  }

  it should "return correct showtime counts per film" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("The Amazing Digital Circus: The Last Act") shouldBe 4
    counts("Drzewo magii")                              shouldBe 3
    counts("Chłopiec na krańcach świata")               shouldBe 3
  }

  it should "enrich metadata from the op.s detail page" in {
    val m = byTitle("The Amazing Digital Circus: The Last Act")
    m.movie.runtimeMinutes shouldBe Some(93)
    m.movie.releaseYear    shouldBe Some(2026)
    m.movie.countries      shouldBe Seq("USA", "Australia")
    m.movie.genres         shouldBe Seq("Przygodowy", "Animowany", "Sci-Fi")
    m.filmUrl              shouldBe Some("https://www.kinonh.pl/op.s?id=22489")
    m.synopsis.getOrElse("").length should be > 30
  }

  it should "parse the original title and drop the age-rating clause from genres" in {
    val m = byTitle("Obcy")
    m.movie.originalTitle shouldBe Some("L’étranger")
    m.movie.genres        shouldBe Seq("Dramat", "Kryminał")
    m.director            shouldBe Seq("François Ozon")
  }

  it should "resolve 'dzisiaj' against the injected today and build booking URLs" in {
    byTitle("The Amazing Digital Circus: The Last Act").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 16, 30),
        Some("https://www.kinonh.pl/bilet.s?eventId=192744&forwardback=https%3A%2F%2Fwww.kinonh.pl%2Fprogram.s%3F"),
        None, Nil
      )
  }
}
