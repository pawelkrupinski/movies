package clients.kinogram

import clients.tools.FakeHttpFetch
import models.{KinoGram, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoGramClient

import java.time.LocalDateTime

class KinoGramClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoGramClient(new FakeHttpFetch("kinogram"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoGramClient.fetch" should "return 14 films and 84 showtimes from the GraphQL API" in {
    results.size shouldBe 14
    results.flatMap(_.showtimes).size shouldBe 84
  }

  it should "assign KinoGram to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoGram)
  }

  it should "return correct showtime counts per film" in {
    // The "(Dolby Atmos)" edition now strips to the base title, so the film spans
    // two client rows (the cache folds them into one card downstream); sum them.
    val counts = results.groupBy(_.movie.title).view.mapValues(_.map(_.showtimes.size).sum).toMap
    counts("Zawodowcy")                   shouldBe 17
    counts("Diabeł ubiera się u Prady 2") shouldBe 27 // 24 base + 3 Dolby Atmos
  }

  // KinoGram bakes the premium-sound edition into the GraphQL title
  // ("… (Dolby Atmos)"); strip it so those screenings fold onto the base film
  // instead of standing up a near-duplicate card. Dolby Atmos isn't one of the
  // app's format badges (2D/3D/DUB/NAP/IMAX/4DX), so it yields no badge token.
  it should "strip a trailing '(Dolby Atmos)' edition tag off the title" in {
    results.map(_.movie.title) should contain("Diabeł ubiera się u Prady 2")
    results.map(_.movie.title) should not contain "Diabeł ubiera się u Prady 2 (Dolby Atmos)"
  }

  it should "map English country names to Polish and keep only the Polish synopsis" in {
    val m = byTitle("Zawodowcy")
    m.movie.runtimeMinutes shouldBe Some(98)
    m.movie.releaseYear    shouldBe Some(2026)
    m.movie.originalTitle  shouldBe Some("In the Grey")
    m.movie.countries      shouldBe Seq("USA", "Wielka Brytania")
    m.movie.genres         shouldBe Seq("Akcja", "Dramat")
    m.synopsis.getOrElse("") should not include "[EN]"
    m.synopsis.getOrElse("").length should be > 50
    m.posterUrl.getOrElse("") should startWith ("https://media.user.com")
    m.trailerUrl.isDefined shouldBe true
  }

  it should "convert UTC screening times to Europe/Warsaw and build screening booking URLs" in {
    byTitle("Zawodowcy").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 15, 45),
        Some("https://bilety.kinogram.pl/screening/349ec18c-dc14-4235-a630-19c4c807c204"),
        None, Nil
      )
  }
}
