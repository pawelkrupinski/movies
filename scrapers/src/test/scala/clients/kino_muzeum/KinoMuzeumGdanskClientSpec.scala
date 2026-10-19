package clients.kino_muzeum

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoMuzeumGdansk
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoMuzeumGdanskClient

import java.time.LocalDateTime

/**
 * Replays the real muzeum1939.pl repertoire capture (base page + every
 * `has-events` day page, recorded 2026-06-06 under
 * test/resources/fixtures/kino-muzeum/www.muzeum1939.pl/kino-muzeum/). The
 * listing renders one day at a time; the client walks the calendar's day links
 * and stitches the multi-day programme together, deriving each screening's year
 * from the day link's `timestamp` epoch.
 */
class KinoMuzeumGdanskClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val client  = new KinoMuzeumGdanskClient(new FakeHttpFetch("kino-muzeum"), KinoMuzeumGdansk)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoMuzeumGdanskClient.fetch" should "return the eight distinct films across the captured days" in {
    results.size shouldBe 8
    results.flatMap(_.showtimes).size shouldBe 18
  }

  it should "assign Kino Muzeum to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoMuzeumGdansk)
  }

  it should "give every film at least one plausible showtime" in {
    all(results.map(_.showtimes)) should not be empty
    val times = results.flatMap(_.showtimes).map(_.dateTime)
    all(times.map(_.getYear)) shouldBe 2026
    all(times.map(_.getHour)) should (be >= 8 and be <= 23)
  }

  it should "pin MILCZĄCA PRZYJACIÓŁKA's 6 June 17:00 screening with its booking link and listing metadata" in {
    val m = byTitle("MILCZĄCA PRZYJACIÓŁKA")
    m.movie.runtimeMinutes shouldBe Some(143)
    m.movie.releaseYear    shouldBe Some(2025)
    m.movie.countries      shouldBe Seq("Francja", "Niemcy", "Węgry")
    m.movie.genres         shouldBe Seq("Dramat")
    m.director             shouldBe Seq("Ildikó Enyedi")
    val st = m.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 6, 17, 0)).value
    st.bookingUrl.value should startWith("https://bilety.muzeum1939.pl/rezerwacja/numerowane.html?id=21173")
  }

  it should "derive the year from the day-link epoch, not the displayed DD.MM" in {
    // MIKEY I NICKY screens once, on 6 June 2026 — the listing date string is
    // bare "06.06"; the 2026 comes from the day link's `timestamp`.
    byTitle("MIKEY I NICKY").showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 6, 6, 15, 15))
  }

  it should "read the production year from the 'Rok produkcji' meta cell" in {
    // The cell text is "Rok produkcji 1976"; the displayed screening date is
    // 2026, so this must be the production year, not the screening year.
    byTitle("MIKEY I NICKY").movie.releaseYear shouldBe Some(1976)
  }
}
