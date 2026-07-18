package clients.kino_bajka

import models.KinoBajka
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoBajkaClient

import java.time.LocalDateTime

/** Replays the recorded Kino Bajka repertoire (11-07-2026 capture of
 *  `kinobajka.pl/repertuar/`) through the client. The WordPress page no longer
 *  server-renders the schedule as HTML — it ships the whole advance window as an
 *  HTML-entity-encoded JSON blob in `<div id="rep2" data-dane='{…}'>`, which the
 *  site's `rep2` widget parses client-side. The client reads that attribute
 *  (jsoup entity-decodes it) and parses the `{buy, dni:{date:[film…]}}` JSON, so
 *  a single fetch still carries every screening. */
class KinoBajkaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-bajka")

  "KinoBajkaClient" should "parse film screenings off the data-dane JSON blob" in {
    val movies = new KinoBajkaClient(http, KinoBajka).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoBajka)

    // Every film has at least one showtime; all dates fall in the captured 2026 window.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "pair each showtime with its enclosing day's date and read the runtime" in {
    val movies = new KinoBajkaClient(http, KinoBajka).fetch()

    // Pinned screening seen in the captured fixture: "Minionki i straszydła"
    // plays 11-07-2026 at 13:30 — the 13:30 lives under the 2026-07-11 `dni` key,
    // so this verifies the day-key → date pairing.
    val minionki = movies.find(_.movie.title.toLowerCase.contains("minionki")).value
    minionki.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 7, 11, 13, 30))

    // Runtime is read off the "… · 90 min" caption; the booking URL is the blob's
    // top-level `buy` host, shared by every showtime.
    minionki.movie.runtimeMinutes.value shouldBe 90
    val slot = minionki.showtimes.find(_.dateTime == LocalDateTime.of(2026, 7, 11, 13, 30)).value
    slot.bookingUrl.value shouldBe "https://bajka-lublin.biletpro24.pl/BiletPro24/reservation/screenings"
  }

  it should "merge a film's screenings across days into one CinemaMovie" in {
    val movies = new KinoBajkaClient(http, KinoBajka).fetch()

    // "Minionki i straszydła" plays on several days; it collapses to a single row
    // whose showtimes span more than one date.
    val minionki = movies.filter(_.movie.title.toLowerCase.contains("minionki"))
    minionki should have size 1
    minionki.head.showtimes.map(_.dateTime.toLocalDate).distinct.size should be > 1
  }
}
