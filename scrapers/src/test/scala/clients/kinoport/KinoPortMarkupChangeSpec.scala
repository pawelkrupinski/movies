package clients.kinoport

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.KinoPort
import services.cinemas.pl.KinoPortClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded 2026-09-08 capture of
  * https://gcsw.pl/wp-json/wp/v2/posts?categories=49, taken after gcsw.pl
  * restructured its Elementor post: the month header moved from `<h3>` to
  * `<h4>`, and — the part that actually broke parsing — the day headers
  * ("03.09 (CZWARTEK)") moved from `<h4>` to plain `<p>`, so
  * `KinoPortClient`'s `case "h4" => ...` branch never saw a day header again
  * and every screening paragraph was read with `day = None` and dropped.
  * KinoPort went white on this capture: 0 films, 0 showtimes.
  *
  * Some screenings also now split "17:00 – Tony" across three sibling
  * `<strong>` tags instead of one, and captions dropped their `<em>`
  * wrapper and their "reż." prefix ("1962, Orson Welles" instead of "1962,
  * reż. Orson Welles") — both covered below alongside the day-header fix. */
class KinoPortMarkupChangeSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoPortClient(
    new FakeHttpFetch("kinoport-markup-change-2026-09"), KinoPort, LocalDate.of(2026, 9, 8))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoPortClient.fetch" should "not be empty against the restructured markup" in {
    results should not be empty
    results.flatMap(_.showtimes) should not be empty
  }

  it should "read a day header that now lives in a <p>, not an <h4>" in {
    val m = byTitle("Proces")
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 3, 17, 30))
  }

  it should "read a title split across multiple sibling <strong> tags" in {
    // "<strong>17:00</strong> <strong>– </strong><strong>Tony</strong> (106')..."
    val m = byTitle("Tony")
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 4, 17, 0))
  }

  it should "read a caption with no 'reż.' prefix and no <em> wrapper" in {
    // "<strong>17:30 – Proces</strong> (118')<br />1962, Orson Welles<br />..."
    val m = byTitle("Proces")
    m.movie.releaseYear shouldBe Some(1962)
    m.director          shouldBe Seq("Orson Welles")
  }
}
