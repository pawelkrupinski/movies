package clients.kino_zamek

import clients.tools.FakeHttpFetch
import models.KinoZamekSzczecin
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoZamekClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `bilety.zamek.szczecin.pl/MSI?sort=Name&date=2026-06`
 *  (showtime data) and `zamek.szczecin.pl/wydarzenia/kino/` (film allow-list)
 *  fixtures through the client.  `today` is pinned to 2026-06-07. */
class KinoZamekClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-zamek")
  // TODO: replace `KinoZamekSzczecin` with the real `KinoZamekSzczecin` case object
  // once it has been added to Cinema.scala and City.scala.
  private val client = new KinoZamekClient(http, KinoZamekSzczecin, today = LocalDate.of(2026, 6, 7))

  "KinoZamekClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with the supplied cinema" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoZamekSzczecin)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "exclude non-film events (concerts, workshops)" in {
    val movies = client.fetch()
    val titles = movies.map(_.movie.title.toLowerCase)
    // These are concerts / workshops in the MSI but absent from the kino listing:
    titles.filter(_.contains("koncert")) shouldBe empty
    titles.filter(_.contains("disco"))   shouldBe empty
    titles.filter(_.contains("joga"))    shouldBe empty
  }

  it should "pin a concrete film screening: Viridiana on 2026-06-07 at 19:00" in {
    // "VIRIDIANA – BUÑUEL. NIECH ŻYJĄ KAJDANY" is in the kino listing;
    // slug: "viridiana".  Cleaned title: "Viridiana – buñuel. Niech żyją kajdany"
    // (sentence-cased).
    val movies = client.fetch()
    val film = movies.find(_.movie.title.toLowerCase.startsWith("viridiana")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 19, 0))
  }
}
