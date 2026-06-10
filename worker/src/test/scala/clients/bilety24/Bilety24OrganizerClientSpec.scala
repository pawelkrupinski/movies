package clients.bilety24

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import services.cinemas.Bilety24OrganizerClient

import java.time.LocalDateTime

/** Kino Kosmos, Kino Światowid and Kino Elektronik had their per-venue
 *  `*.bilety24.pl` subdomains decommissioned (DNS pointing at a dead host →
 *  red `ConnectException` bars on /uptime). bilety24.pl moved them onto the
 *  main domain at `www.bilety24.pl/kino/organizator/<slug>-<id>`. This replays
 *  each migrated organizer page through the new client and pins a concrete
 *  screening, proving one fetch recovers the full programme off the new URL. */
class Bilety24OrganizerClientSpec
    extends AnyFlatSpec
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  // (label, fixtureDir, organizerUrl, cinema, pinned title substring (lower-cased), pinned showtime)
  private val venues = Table(
    ("label", "dir", "url", "cinema", "title", "when"),
    ("Kino Kosmos", "kino-kosmos", "https://www.bilety24.pl/kino/organizator/kino-kosmos-1501",
      KinoKosmos: Cinema, "dzień objawienia", LocalDateTime.of(2026, 6, 11, 16, 30)),
    ("Kino Światowid", "kino-swiatowid", "https://www.bilety24.pl/kino/organizator/kino-swiatowid-1503",
      KinoSwiatowid, "erupcja", LocalDateTime.of(2026, 6, 10, 16, 15)),
    ("Kino Elektronik", "kino-elektronik", "https://www.bilety24.pl/kino/organizator/kino-elektronik-631",
      KinoElektronik, "kumotry", LocalDateTime.of(2026, 6, 12, 18, 15)),
    ("Kino CK Jędrzejów", "kino-ck", "https://www.bilety24.pl/kino/organizator/centrum-kultury-w-jedrzejowie-1458",
      KinoCK, "toy story 5", LocalDateTime.of(2026, 6, 19, 15, 0)),
    ("Kino Metalowiec Kraśnik", "kino-metalowiec", "https://www.bilety24.pl/kino/organizator/centrum-kultury-i-promocji-w-krasniku-1529",
      KinoMetalowiec, "mandalorian i grogu", LocalDateTime.of(2026, 6, 12, 17, 0)),
    ("Kino Sokolnia Słupca", "kino-sokolnia", "https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423",
      KinoSokolnia, "dzień objawienia", LocalDateTime.of(2026, 6, 12, 19, 0)),
    ("Kino Krapkowice", "kino-krapkowice", "https://www.bilety24.pl/kino/organizator/krapkowicki-dom-kultury-1244",
      KinoKrapkowice, "drzewo magii", LocalDateTime.of(2026, 6, 12, 17, 0))
  )

  forAll(venues) { (label, dir, url, cinema, titleSub, when) =>
    lazy val movies = new Bilety24OrganizerClient(new FakeHttpFetch(dir), url, cinema).fetch()

    it should s"return a non-empty, single-cinema film list — $label" in {
      movies should not be empty
      movies.map(_.cinema).toSet shouldBe Set(cinema)
      all(movies.map(_.showtimes)) should not be empty
    }

    it should s"pin a concrete screening that books on www.bilety24.pl — $label" in {
      val film = movies.find(_.movie.title.toLowerCase.contains(titleSub)).value
      val slot = film.showtimes.find(_.dateTime == when).value
      slot.bookingUrl.value should startWith("https://www.bilety24.pl/kino/")
    }
  }
}
