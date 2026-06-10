package clients.kino_kuznica

import clients.tools.FakeHttpFetch
import models.KinoKuznica
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.SystemBiletowyClient

import java.time.LocalDateTime

/** Replays the recorded `shd.systembiletowy.pl/index.php` repertoire (the
 *  Suchedniów cultural centre's Kino Kuźnica instance) through the generic
 *  systembiletowy client.
 *
 *  Kino Kuźnica was previously scraped from Filmweb, whose API had silently
 *  gone empty for it (every poll returned `[]`) though the cinema is open —
 *  this fixture is the proof its programme is real and reachable on its own
 *  ticketing portal. */
class SystemBiletowyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new SystemBiletowyClient(new FakeHttpFetch("kino-kuznica"), "https://shd.systembiletowy.pl", KinoKuznica).fetch()

  "SystemBiletowyClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKuznica)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "merge a film's dubbing + napisy screenings into one row" in {
    // The fixture lists "… MANDALORIAN & GROGU  dubbing" and "… napisy" as
    // separate rows; stripping the version tag must fold them into ONE film
    // carrying both the 16:00 (dubbed) and 18:30 (subtitled) screenings on 06-12.
    val mandalorian = movies.filter(_.movie.title.toLowerCase.contains("mandalorian"))
    mandalorian.size shouldBe 1
    val times = mandalorian.head.showtimes.map(_.dateTime)
    times should contain(LocalDateTime.of(2026, 6, 12, 16, 0))
    times should contain(LocalDateTime.of(2026, 6, 12, 18, 30))
  }

  it should "carry a per-screening booking link" in {
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).head should include("repertoire.html?id=")
  }
}
