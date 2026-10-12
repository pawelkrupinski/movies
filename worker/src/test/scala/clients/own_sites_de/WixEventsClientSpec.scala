package clients.own_sites_de

import clients.tools.FakeHttpFetch
import models.{Cinema, GermanRoster, TimeZones}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.WixEventsClient

import java.time.{LocalDate, LocalDateTime}

/** Replays Steinhaus Bautzen's Wix events page (https://www.steinhaus-bautzen.de/kino,
 *  2026-10-12, cut to its `wix-warmup-data` block): its film Wednesdays, beside the
 *  Bischofswerda club's "Herbstkino" evenings the same list holds. */
class WixEventsClientSpec extends AnyFlatSpec with Matchers with OptionValues {
  private val Page      = "https://www.steinhaus-bautzen.de/kino"
  private val steinhaus: Cinema = GermanRoster.theaterIdByCinema.collectFirst { case (c, "A2231") => c }.value

  private def fetch(today: LocalDate) =
    new WixEventsClient(new FakeHttpFetch("steinhaus-bautzen", strict = true), Page, "02625", TimeZones.Germany, steinhaus, today).fetch()

  private val movies = fetch(LocalDate.of(2026, 10, 12))

  "WixEventsClient" should "read the venue's own evenings, in its time zone, without their label" in {
    movies.map(m => m.movie.title -> m.showtimes.map(_.dateTime)).toMap shouldBe Map(
      "Nur ein kleines bisschen Glück" -> Seq(LocalDateTime.of(2026, 10, 21, 20, 0)),
      // 17:30 UTC after the clocks go back on the 25th: 18:30 in Bautzen.
      "Vier Minus Drei"                -> Seq(LocalDateTime.of(2026, 10, 27, 18, 30)),
      "A Sad And A Beautiful World"    -> Seq(LocalDateTime.of(2026, 10, 28, 20, 0)),
      "Spaziergang nach Syrakus"       -> Seq(LocalDateTime.of(2026, 11, 4, 20, 0)),
      "Vaterland"                      -> Seq(LocalDateTime.of(2026, 11, 11, 20, 0)),
      "Der Heimatlose"                 -> Seq(LocalDateTime.of(2026, 11, 25, 20, 0)),
    )
    movies.map(_.cinema).toSet shouldBe Set(steinhaus)
  }

  it should "leave out a partner's evenings on the same list" in {
    // "HERBSTKINO IM B28: Hamnet" is at Belmsdorfer Str. 28, 01877 Bischofswerda.
    movies.map(_.movie.title).filter(_.toLowerCase.contains("hamnet")) shouldBe empty
  }

  it should "drop the evenings already past" in {
    fetch(LocalDate.of(2026, 11, 5)).map(_.movie.title).toSet shouldBe Set("Vaterland", "Der Heimatlose")
  }

  it should "strip only a 'Kino' label off a title" in {
    WixEventsClient.filmTitle("KINO-Extra: Vier Minus Drei") shouldBe "Vier Minus Drei"
    WixEventsClient.filmTitle("Kino: Vaterland") shouldBe "Vaterland"
    WixEventsClient.filmTitle("Mission: Impossible") shouldBe "Mission: Impossible"
  }

  it should "fail on a page with no events data rather than report an empty programme" in {
    an[IllegalStateException] should be thrownBy WixEventsClient.parse("<html></html>", "02625", TimeZones.Germany, steinhaus)
  }
}
