package clients.kino_krapkowice

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoKrapkowice
import services.cinemas.pl.KdkKrapkowiceClient

import java.time.LocalDateTime

/** Replays the recorded `kdk.krapkowice.pl/kino` repertoire through the venue's
 *  own-site client. Kino Krapkowice used to be scraped through the
 *  bilety24-organizer page; this fixture proves its full programme is reachable
 *  first-hand on the cinema's own Drupal site, one fewer third party in between. */
class KdkKrapkowiceClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new KdkKrapkowiceClient(new FakeHttpFetch("kino-krapkowice"), KinoKrapkowice).fetch()

  "KdkKrapkowiceClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKrapkowice)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "keep the original mixed-case title (no proper-noun downcasing)" in {
    // The site serves nicely-cased titles, so unlike the ALL-CAPS MSI portals we
    // only strip the trailing version tag — "Mandalorian" must stay capitalised.
    movies.map(_.movie.title) should contain("Gwiezdne wojny: Mandalorian i Grogu")
  }

  it should "merge a film's 2D Nap + 2D Dub screenings into one row" in {
    // The fixture lists the Mandalorian film as separate "2D Nap" / "2D Dub" /
    // "2D NAP" / "2D DUB" rows; stripping the version tag folds them into ONE
    // film carrying every screening (06-15 19:00 napisy, 06-16 19:00 dubbing, …).
    val mandalorian = movies.filter(_.movie.title.toLowerCase.contains("mandalorian"))
    mandalorian.size shouldBe 1
    val times = mandalorian.head.showtimes.map(_.dateTime)
    times should contain(LocalDateTime.of(2026, 6, 15, 19, 0))
    times should contain(LocalDateTime.of(2026, 6, 16, 19, 0))
    times should contain(LocalDateTime.of(2026, 6, 19, 19, 30))
  }

  it should "surface the version as Showtime.format tokens" in {
    val mandalorian = movies.find(_.movie.title.toLowerCase.contains("mandalorian")).value
    val napScreening = mandalorian.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 15, 19, 0)).value
    napScreening.format should contain allOf ("2D", "NAP")
    val dubScreening = mandalorian.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 16, 19, 0)).value
    dubScreening.format should contain allOf ("2D", "DUB")
  }

  it should "read a date+time off the visible time, not the offset datetime attr" in {
    // "Drzewo magii" screens 2026-06-16 17:00 — the datetime attr's TIME part is
    // unreliable (spurious Z), so the real 17:00 comes from div.kino-godziny.
    val drzewo = movies.find(_.movie.title.toLowerCase.contains("drzewo magii")).value
    drzewo.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 16, 17, 0))
  }
}
