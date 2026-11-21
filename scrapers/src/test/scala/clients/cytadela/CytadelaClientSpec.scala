package clients.cytadela

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import models.{KinoCytadela, Showtime}
import services.cinemas.pl.CytadelaClient

import java.time.LocalDateTime

class CytadelaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new CytadelaClient(new FakeHttpFetch("kino-cytadela"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "CytadelaClient.fetch" should "return 9 films and 10 showtimes from /repertuar" in {
    results.size shouldBe 9
    results.flatMap(_.showtimes).size shouldBe 10
  }

  it should "assign Kino Cytadela to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoCytadela)
  }

  it should "read genre/year/runtime off the listing and date off the day header" in {
    val m = byTitle("Pucio")
    m.movie.runtimeMinutes shouldBe Some(45)
    m.movie.releaseYear    shouldBe Some(2026)
    m.movie.genres         shouldBe Seq("Animacja")
    m.director             shouldBe Seq("Marta Stróżycka")
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 6, 11, 0), Some("https://sklep.muzhp.pl/rezerwacja/rezerwacja/numerowane.html?id=17404&idt=98dd73275f980753c878e6a513e22226"), None, List("DUB"))
  }

  // Each row badges its language version ("dubbing", "napisy", "polski lektor", "oryginalny") in its own
  // `li.repertoire-item__info__item`, which the title never repeats; it was not read, so no Cytadela
  // screening said whether it was dubbed or subtitled.
  it should "read each row's language version off its info badge" in {
    def formats(title: String) = byTitle(title).showtimes.map(_.format).distinct
    formats("Pucio")                                          shouldBe Seq(List("DUB"))
    formats("Amelia")                                         shouldBe Seq(List("NAP"))
    formats("Niesamowite przygody skarpetek 3. Ale kosmos!")  shouldBe Seq(List("LEK"))
    formats("Historanki z Maluchem")                          shouldBe Seq(Nil) // "oryginalny": the original, no badge
  }

  // Regression: the description tails into an inline "♦ Bilety: … / ◊ sprawdź
  // dojazd … / W programie:" venue footer; it must not leak into the synopsis.
  it should "truncate the inline ticket/venue footer from synopses" in {
    val synopses = results.flatMap(_.filmUrl).flatMap(client.fetchFilmDetail).flatMap(_.synopsis)
    synopses.exists(_.contains("Kultowy duet Tom i Jerry")) shouldBe true // prose preserved
    synopses.foreach { s =>
      s should not include "♦"
      s should not include "sprawdź dojazd"
      s should not include "W programie:"
    }
  }
}
