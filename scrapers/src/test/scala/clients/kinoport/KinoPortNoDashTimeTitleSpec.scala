package clients.kinoport

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.KinoPort
import services.cinemas.pl.KinoPortClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded 2026-09-18 capture of
  * https://gcsw.pl/wp-json/wp/v2/posts?categories=49. From 19.09 on, gcsw.pl
  * started publishing some screenings without the "HH:MM – Title" en-dash
  * separator at all — just "12:30 PUCIO KOCHA ZWIERZAKI", time running
  * straight into the (uppercase) title. `TimeTitlePat` required the
  * separator, so every such screening silently failed to parse and dropped:
  * FilmwebDiff's 2026-09-18 run showed KinoPort at ours=2 vs fw=10 for the
  * 3-day window, almost entirely these undashed screenings (Pucio Kocha
  * Zwierzaki, Asterix i Obelix: Misja Kleopatra ×2, Orlando ×2, Moje Matki,
  * Tony, Ojczyzna). */
class KinoPortNoDashTimeTitleSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoPortClient(
    new FakeHttpFetch("kinoport-no-dash-time-title-2026-09"), KinoPort, LocalDate.of(2026, 9, 18))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoPortClient.fetch" should "read a screening whose time has no dash before the title" in {
    val m = byTitle("PUCIO KOCHA ZWIERZAKI")
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 19, 12, 30))
  }

  it should "read an undashed screening sharing a title with a dashed one elsewhere in the window" in {
    val m = byTitle("ASTERIX I OBELIX: MISJA KLEOPATRA")
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 19, 13, 30))
  }

  it should "still read a normally-dashed screening in the same post" in {
    val m = byTitle("Tony")
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 4, 17, 0))
  }
}
