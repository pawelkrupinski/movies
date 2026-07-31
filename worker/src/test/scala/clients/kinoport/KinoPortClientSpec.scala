package clients.kinoport

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{KinoPort, Showtime}
import services.cinemas.pl.KinoPortClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded 2026-07-31 capture of
  * https://gcsw.pl/wp-json/wp/v2/posts?categories=49 — the KinoPort repertoire
  * post on the venue's OWN site, which lists 42 screenings while the venue's old
  * Filmweb source (cinemaId 1735) returns `[]` for every date, painting the
  * uptime bar white. */
class KinoPortClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoPortClient(new FakeHttpFetch("kinoport"), KinoPort, LocalDate.of(2026, 7, 31))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoPortClient.fetch" should "fold the repertoire post into 23 films / 42 showtimes" in {
    results.size shouldBe 23
    results.flatMap(_.showtimes).size shouldBe 42
  }

  it should "assign KinoPort to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoPort)
  }

  it should "read the time off the <strong> and the day off the preceding <h4>" in {
    val m = byTitle("Ghost in the Shell")
    m.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 7, 31, 17, 30),
      LocalDateTime.of(2026, 8, 1, 19, 30),
      LocalDateTime.of(2026, 8, 2, 20, 0)
    )
    // No booking deep-link exists — tickets are sold at the box office only.
    m.showtimes.head shouldBe Showtime(LocalDateTime.of(2026, 7, 31, 17, 30), None, None, Nil)
    m.filmUrl shouldBe Some("https://gcsw.pl/2026/07/22/repertuar-kinoport-gdansk/")
  }

  it should "carry the year over from the previous month header when a header omits it" in {
    // The post reads "Lipiec 2026" then just "Sierpień" — every August screening
    // must still land in 2026, not default to some other year.
    val august = results.flatMap(_.showtimes).map(_.dateTime).filter(_.getMonthValue == 8)
    august should not be empty
    august.map(_.getYear).toSet shouldBe Set(2026)
  }

  it should "read runtime, release year and director off the screening paragraph" in {
    val m = byTitle("Arek. Mama. Panorama")
    m.movie.runtimeMinutes shouldBe Some(72)   // "(72′)" — U+2032 PRIME, not an apostrophe
    m.movie.releaseYear    shouldBe Some(2026)
    m.director             shouldBe Seq("Mikołaj Janik")
  }

  it should "split a co-directed film's 'X i Y' credit into two directors" in {
    byTitle("Człowiek do wszystkiego").director shouldBe Seq("Wilhelm Sasnal", "Anna Sasnal")
  }

  it should "leave a film with no 'reż.' credit without a director rather than inventing one" in {
    // "12:30 – Mała Amelia (75′)<em>2025</em> • dubbing" — year only, no director.
    byTitle("Mała Amelia").director          shouldBe Seq.empty
    byTitle("Mała Amelia").movie.releaseYear shouldBe Some(2025)
  }

  it should "drop the ARCHIWALNE SEANSE accordion instead of resurrecting finished screenings" in {
    // The same post carries identically-shaped past listings for 2–25 July under
    // an "ARCHIWALNE SEANSE" accordion. Parsing them would republish weeks of
    // screenings that already happened.
    val earliest = results.flatMap(_.showtimes).map(_.dateTime).min
    earliest shouldBe LocalDateTime.of(2026, 7, 30, 18, 0)
    // "Truposz" and "Mystery Train" appear ONLY in the archive.
    byTitle.keySet should not contain "Truposz"
    byTitle.keySet should not contain "Mystery Train"
  }
}
