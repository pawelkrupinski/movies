package services.cinemas

import clients.tools.FakeHttpFetch
import models.KinoKadr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/** Fixture-replay spec for [[VisualTicketClient]] against the recorded
  * bilety.palac.art.pl repertoire page (Kino Studyjne Kadr, location-id 2).
  * Recorded: 2026-06-21. */
class VisualTicketClientSpec extends AnyFlatSpec with Matchers {

  private val client = new VisualTicketClient(
    new FakeHttpFetch("kino-kadr"),
    "https://bilety.palac.art.pl",
    KinoKadr,
    locationId = 2
  )

  private val films = client.fetch()

  "VisualTicketClient" should "return a non-empty list of films" in {
    films should not be empty
  }

  it should "tag every film with KinoKadr" in {
    films.map(_.cinema).distinct should be(Seq(KinoKadr))
  }

  it should "have non-empty titles on every film" in {
    films.map(_.movie.title).forall(_.nonEmpty) shouldBe true
  }

  it should "not carry the ' - seans filmowy' suffix in any title" in {
    films.map(_.movie.title) should not contain regex("(?i)seans filmowy")
  }

  it should "have at least one film with a future-dated screening" in {
    val today = LocalDate.of(2026, 6, 21)
    val hasFuture = films.exists(_.showtimes.exists(s => !s.dateTime.toLocalDate.isBefore(today)))
    hasFuture shouldBe true
  }

  it should "have valid (non-negative) screening times on all showtimes" in {
    val allTimes = films.flatMap(_.showtimes.map(_.dateTime))
    allTimes should not be empty
    allTimes.forall(dt => dt.getHour >= 0 && dt.getHour < 24) shouldBe true
  }

  it should "parse at least 10 distinct films" in {
    films.size should be >= 10
  }
}
